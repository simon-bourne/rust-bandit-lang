use std::fmt;

#[derive(Clone, Eq, PartialEq)]
pub enum Constant {
    TypeOfType,
    Type(String),
}

impl Constant {
    pub fn typ(&self) -> Self {
        Self::TypeOfType
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::TypeOfType => "Type",
            Self::Type(name) => name,
        })
    }
}
