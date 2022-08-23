use proc_macro::{TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Ident, Type, Field, LitStr, DataStruct, PathArguments, GenericArgument};
use syn::__private::{str, TokenStream2};
use syn::parse::ParseStream;

#[derive(Clone)]
struct CustomField {
    name: Ident,
    ty: Type,
    option: bool,
    attributes: Vec<Attribute>
}
#[proc_macro_derive(Builder, attributes(builder_vis, builder_rand, builder_validate, builder_into))]
pub fn builder(
    annotated_item: TokenStream,
)
    -> TokenStream
{
    let input = parse_macro_input!(annotated_item as DeriveInput);
    let struct_name = input.ident;
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed {ref named, ..}),
        ..
    }) = input.data {
        named
    } else {
        panic!("struct {} has no fields", struct_name);
    };

    let mut field_parser = FieldParser::new();

    field_parser.fields = fields.iter().map(
        | Field { ident, ty, attrs, ..} |
            {
                let (ty, option) = FieldParser::unwrap_type(ty);
                let attrs = attrs.iter().filter_map(|attr| {
                    if let Some(ident) = attr.path.get_ident() {
                        match ident.to_string().as_str() {
                            "builder_vis" => {
                                Some(match attr.parse_args::<LitStr>() {
                                    Ok(str) => match str.value().as_str() {
                                        "private" => Attribute::Vis(Vis::Private),
                                        "public" => Attribute::Vis(Vis::Public),
                                        _ => panic!("the args passed with builder_vis are invalid")
                                    }
                                    _ => panic!("must pass string as arg with builder_vis")
                                })
                            },
                            "builder_rand" => {
                                Some(match attr.parse_args::<LitStr>() {
                                    Ok(str) => match str.value().as_str() {
                                        "uuid" => Attribute::ValidatiobStep(ValidationStep::Set(SetType::Uuid)),
                                        _ => panic!("the args passed with builder_rand are invalid")
                                    }
                                    _ => panic!("must pass string as arg with builder_rand")
                                })
                            },
                            "builder_validate" => {
                                Some(match attr.parse_args::<LitStr>() {
                                    Ok(str) => match str.value().as_str() {
                                        "is_some" => Attribute::ValidatiobStep(ValidationStep::Check(CheckType::Some)),
                                        "len" => Attribute::ValidatiobStep(ValidationStep::Check(CheckType::Len(0))),
                                        _ => panic!("the args passed with builder_validate are invalid")
                                    }
                                    _ => panic!("must pass string as arg with builder_validate")
                                })
                            },
                            "builder_into" => {
                                Some(Attribute::Into)
                            }
                            _ => None
                        }
                    } else {
                        None
                    }
                }).collect::<Vec<Attribute>>();

                CustomField {
                    name: ident.clone().unwrap(),
                    ty: ty,
                    option,
                    attributes: attrs
                }
            }
    ).collect();

    let FieldPaserserMapResult {
        validation_checks,
        validation_sets,
        implemented_methods,
    } = field_parser.map_fields();

    let stream = quote! {
        impl crate::builder::Validate for #struct_name
        {
            fn validate(mut self)
            -> Result<Self, crate::errors::ValidationError> where Self: Sized
            {
                if true #(#validation_checks)* {
                    #(#validation_sets)*
                    Ok(self)
                } else {
                    Err(crate::errors::ValidationError)
                }
            }
        }

        impl<T: crate::builder::ParentBuilder> crate::builder::Builder<#struct_name, T>
        {
            #(#implemented_methods)*
        }

    }.into();

    return stream
}

#[derive(Default)]
struct FieldParser {
    fields: Vec<CustomField>,
}

impl FieldParser {
    fn new() -> Self {
        std::default::Default::default()
    }

    fn unwrap_type<'a>(ty: &'a Type) -> (Type, bool) {
        if let syn::Type::Path(syn::TypePath { path, .. }) = ty {
            if let Some(segment) = path.segments.last() {
                match segment.ident.to_string().as_str() {
                    "Option" => {
                        match &segment.arguments {
                            PathArguments::AngleBracketed(args) => match args.args.first() {
                                Some(GenericArgument::Type(ty)) => return (ty.clone(), true),
                                _ => panic!("option argument not a type"),
                            }
                            _ => panic!("argument to option not an AngleBracketed argument"),
                        }
                    }
                    _ => return (ty.clone(), false)
                }
            } else {
                panic!("field does not have a type")
            }
        } else {
            panic!("field does not have a type")
        };
    }

    fn map_fields(&self) -> FieldPaserserMapResult {
        let mut res = FieldPaserserMapResult {
            validation_checks: vec![],
            validation_sets: vec![],
            implemented_methods: vec![],
        };

        self.fields
            .iter()
            .for_each(|
                cf
            |{
                if !cf.attributes.iter().fold(false, |acc, attribute| {
                    if let Attribute::Vis(Vis::Private) = attribute {
                        true
                    } else {
                        acc
                    }
                }) {
                    res.implemented_methods.push(Self::implement_method(cf.clone()))
                }
            });

        self.fields
            .iter()
            .for_each(|
                cf
            |{
            cf.attributes.iter().for_each(|attr| match attr {
                Attribute::ValidatiobStep(ValidationStep::Check(_)) => {
                    res.validation_checks.push(Self::implement_validation_step(cf.name.clone(), attr.clone()));
                },
                Attribute::ValidatiobStep(ValidationStep::Set(_)) => {
                    res.validation_sets.push(Self::implement_validation_step(cf.name.clone(), attr.clone()));
                },
                _ => (),
            })
        });

        res
    }

    fn implement_method(field: CustomField) -> TokenStream2 {
        let CustomField { name, ty, option, attributes } = field;

        let into = attributes.iter().fold(false, |acc, attr| {
            Attribute::Into == *attr || acc
        });

        let variable = match into {
            true => quote!{ #name.into() },
            false => quote!{ #name },
        };

        let body = match option {
            true => quote!{
                self.body.#name = Some(#variable);

                self
            },
            false => quote!{
                self.body.#name = #variable;

                self
            },
        };

        return match into {
            true => quote!{
                pub fn #name<I: Into<#ty>>(mut self, #name: I) -> Self {
                    #body
                }
            },
            false => quote!{
                pub fn #name(mut self, #name: #ty) -> Self {
                    #body
                }
            },
        };
    }

    fn implement_validation_step(name: Ident, attr: Attribute) -> TokenStream2 {

        match attr {
            Attribute::ValidatiobStep(ValidationStep::Check(CheckType::Some)) =>
                    quote! { && self.#name.is_some() },
            Attribute::ValidatiobStep(ValidationStep::Check(CheckType::Len(_))) =>
                    quote! { && self.#name.len() > 0 },
            Attribute::ValidatiobStep(ValidationStep::Set(SetType::Uuid)) =>
                    quote! { self.#name = Some(uuid::Uuid::new_v4().to_string()); },
            Attribute::Vis(_) => panic!("cannot pass vis attribute to implement_validation_step"),
            Attribute::Into => panic!("cannot pass vis attribute to implement_validation_step"),
        }
    }
}

#[derive(Clone)]
struct FieldPaserserMapResult {
    validation_checks: Vec<TokenStream2>,
    validation_sets: Vec<TokenStream2>,
    implemented_methods: Vec<TokenStream2>,
}

#[derive(Clone, PartialEq)]
enum Attribute {
    ValidatiobStep(ValidationStep),
    Vis(Vis),
    Into
}

#[derive(Clone, PartialEq)]
enum Vis {
    Private,
    Public
}

#[derive(Clone, PartialEq)]
enum ValidationStep {
    Check(CheckType),
    Set(SetType),
}

#[derive(Clone, PartialEq)]
enum SetType {
    Uuid,
}

#[derive(Clone, PartialEq)]
enum CheckType {
    Some,
    Len(i32),
}



