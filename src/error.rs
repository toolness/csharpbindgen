use std::fmt;
use std::fmt::{Formatter, Display};

/// This represents an error from the library.
#[derive(Debug)]
pub enum Error {
    /// A wrapped error from the Syn library, caused when
    /// parsing the source Rust code.
    SynError(syn::Error),
    /// Represents a feature of the source Rust code that cannot
    /// be converted to C#. The first item is the error message,
    /// and the second is the identifier in the source Rust
    /// code that caused the error.
    UnsupportedError(String, Option<syn::Ident>)
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

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

pub(crate) fn add_ident<T>(result: Result<T>, ident: &syn::Ident) -> Result<T> {
    match result {
        Err(Error::UnsupportedError(reason, None)) => {
            Err(Error::UnsupportedError(reason, Some(ident.clone())))
        },
        _ => result
    }
}
