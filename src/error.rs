use std::fmt;
use std::fmt::{Formatter, Display};

#[derive(Debug)]
pub enum Error {
    SynError(syn::Error),
    UnsupportedError(String, Option<syn::Ident>)
}

pub type Result<T> = std::result::Result<T, Error>;

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::SynError(err) => {
                write!(f, "Couldn't parse Rust code: {}", err)
            },
            Error::UnsupportedError(reason, maybe_ident) => {
                let loc = if let Some(ident) = maybe_ident {
                    format!(" while processing symbol \"{}\"", ident.to_string())
                } else {
                    String::from("")
                };
                write!(f, "Unable to export C# code{} because {}", loc, reason)
            }
        }
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::SynError(ref err) => {
                Some(err)
            },
            Error::UnsupportedError(_, _) => {
                None
            }
        }
    }
}

pub fn add_ident<T>(result: Result<T>, ident: &syn::Ident) -> Result<T> {
    match result {
        Err(Error::UnsupportedError(reason, None)) => {
            Err(Error::UnsupportedError(reason, Some(ident.clone())))
        },
        _ => result
    }
}
