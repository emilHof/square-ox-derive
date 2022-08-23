use square_ox_derive::Builder;


struct BuildError;
struct ValidationError;

// Any Object that is buildable implements this trait
// When implemented it allows the use of the object with the Builder::from() method
pub trait Validate {
    fn validate(self) -> Result<Self, ValidationError> where Self: Sized;
}

// When implemented, a builder holding the type the trait is implemented on can release a builder
// holding the T type.
// Allows the use of the .sub_builder_from() method.
pub trait AddField<T> {
    fn add_field(&mut self, field: T);
}

// This trait allows a builder to release a sub builder and allows that sub builder to add its field
// to the releasing builders body.
pub trait BackIntoBuilder<T: crate::builder::Validate, U: ParentBuilder + BackIntoBuilder<T, U>> {
    fn add_field(self, field: T) -> Self;
    fn sub_builder_from(self, body: T) -> Builder<T, U>;
}

// The builder struct holds a body of type T and a parent_builder of type U, where T implements
// Validate and U implements ParentBuilder
pub struct Builder<T, U>
    where T: Validate,
          U: ParentBuilder
{
    pub(crate) body: T,
    pub(crate) parent_builder: Option<U>
}

pub struct Nil;

// the ParentBuilder trait allows types to be placed within a builder's parent_builder field
pub trait ParentBuilder {}

// both Nil and Builder<T, U> where T implements validate and U implements ParentBuilder implement
// the ParentBuilder trait automatically
impl ParentBuilder for Nil{}

impl<T: Validate, U: ParentBuilder> ParentBuilder for Builder<T, U> {}

// gives builders the ability to validate and build the objects they hold in their body field.
impl<T: Validate, U: ParentBuilder> Builder<T, U> {
    pub async fn build(self) -> Result<T, BuildError> {
        match self.body.validate() {
            Ok(body) => Ok(body),
            Err(_) => Err(BuildError)
        }
    }
}

// Allows a builder that holds a parent builder that implements the BackIntoBuilder trait to return
// the builder it is holding while also validating and adding its content to the body of the parent
// builder.
impl<T: Validate, V: ParentBuilder + BackIntoBuilder<T, V>> Builder<T, V> {
    pub fn into_parent_builder(self) -> Result<V, BuildError> {
        match self.body.validate() {
            Ok(body) => {
                Ok(self.parent_builder.unwrap().add_field(body))
            },
            Err(_) => Err(BuildError)
        }
    }
}

// The BackIntoBuilder trait is automatically implemented on any Builder with body of type T and a
// Parent Builder whose body V implements the AddField trait for type T
impl<V: AddField<T> + Validate, U: ParentBuilder, T: Validate> BackIntoBuilder<T, Builder<V, U>> for Builder<V, U> {
    fn add_field(mut self, field: T) -> Self {
        self.body.add_field(field);

        self
    }

    fn sub_builder_from(self, body: T) -> Builder<T, Builder<V, U>> {
        Builder {
            body,
            parent_builder: Some(self),
        }
    }
}

// Any type T that implements the Validate trait can be used in the Builder::from() method to return
// a builder of type Builder<T, Nil>
impl<T: Validate> From<T> for Builder<T, Nil> {
    fn from(body: T) -> Self {
        Builder {
            body,
            parent_builder: None::<Nil>
        }
    }
}

#[test]
fn it_works() {
    #[derive(Builder)]
    struct Example {
        field_0: String,
        field_1: Option<i32>,
    }
}