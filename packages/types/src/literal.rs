use std::fmt;

#[derive(Clone, Eq, PartialEq)]
pub enum Literal {
    TypeOfType,
    Type(String),
}

impl Literal {
    pub fn typ(&self) -> Self {
        Self::TypeOfType
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::TypeOfType => "Type",
            Self::Type(name) => name,
        })
    }
}
