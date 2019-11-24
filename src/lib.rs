//! csharpbindgen is a library for generating low-level C# bindings
//! from Rust code.
//!
//! It is currently in a very primitive state, largely designed for use by the
//! [Unity Pathfinder plugin][plugin] and missing many features.
//!
//! ## Quick start
//!
//! The library is intended for use via a [Cargo build script][build].
//!
//! Here's an example of a simple program that converts some simple Rust code
//! into C#:
//!
//! ```
//! let rust = r#"
//!   pub unsafe extern "C" fn my_func(foo: i32) -> f32 { /* ... */ }
//! "#;
//!
//! let code = csharpbindgen::Builder::new("MyDll", rust.to_string())
//!     .class_name("MyStuff")
//!     .generate()
//!     .unwrap();
//!
//! println!("{}", code);
//! ```
//!
//! This will print out something like the following C# code:
//!
//! ```csharp
//! // This file has been auto-generated, please do not edit it.
//!
//! using System;
//! using System.Runtime.InteropServices;
//!
//! internal class MyStuff {
//!     [DllImport("MyDll")]
//!     internal static extern Single my_func(Int32 foo);
//! }
//! ```
//!
//! For a more complete example, see the Unity Pathfinder plugin's [`build.rs`][].
//!
//!
//! [plugin]: https://github.com/toolness/pathfinder-unity-fun
//! [build]: https://doc.rust-lang.org/cargo/reference/build-scripts.html
//! [`build.rs`]: https://github.com/toolness/pathfinder-unity-fun/blob/master/build.rs

use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use syn::Item;

mod error;
mod ignores;
mod symbol_config;

pub use error::Error;
use error::Result;
use symbol_config::{SymbolConfig, SymbolConfigManager};

const INDENT: &'static str = "    ";

/// Enumeration for C#'s access modifiers.
#[derive(Clone, Copy)]
pub enum CSAccess {
    Private,
    Protected,
    Internal,
    Public,
}

impl Display for CSAccess {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CSAccess::Private => "private",
                CSAccess::Protected => "protected",
                CSAccess::Internal => "internal",
                CSAccess::Public => "public",
            }
        )
    }
}

impl Default for CSAccess {
    fn default() -> Self {
        CSAccess::Internal
    }
}

struct CSTypeDef {
    name: String,
    ty: CSType,
}

impl CSTypeDef {
    pub fn from_rust_type_def(rust_type_def: &syn::ItemType) -> Result<Self> {
        Ok(CSTypeDef {
            name: rust_type_def.ident.to_string(),
            ty: CSType::from_rust_type(&rust_type_def.ty)?,
        })
    }
}

#[derive(Clone)]
struct CSType {
    name: String,
    is_ptr: bool,

    descr: CSTyDescr,
}

#[derive(Clone)]
enum CSTyDescr {
    /// named type, including primitives and unknown types
    Named,
    /// struct type
    Struct(Rc<CSStruct>),
    /// delegate type
    Delegate(Box<CSDelegate>),
}

impl CSType {
    fn void() -> Self {
        CSType {
            name: "void".to_owned(),
            is_ptr: false,
            descr: CSTyDescr::Named,
        }
    }

    pub fn from_rust_type(rust_type: &syn::Type) -> Result<Self> {
        match rust_type {
            syn::Type::Path(type_path) => {
                let last = type_path
                    .path
                    .segments
                    .last()
                    .expect("expected at least one path segment on type!");
                Ok(CSType {
                    name: last.value().ident.to_string(),
                    is_ptr: false,
                    descr: CSTyDescr::Named,
                })
            }

            syn::Type::Ptr(type_ptr) => {
                let mut wrapped_type = CSType::from_rust_type(&type_ptr.elem)?;
                if wrapped_type.is_ptr {
                    return unsupported(format!(
                        "double pointers for {} are unsupported!",
                        wrapped_type.name
                    ));
                }
                wrapped_type.is_ptr = true;
                Ok(wrapped_type)
            }

            syn::Type::BareFn(bare_fn) => {
                let d = CSDelegate::from_rust_fn(bare_fn)?;
                Ok(CSType {
                    name: "__bare_fn".into(),
                    is_ptr: false,
                    descr: CSTyDescr::Delegate(Box::new(d)),
                })
            }

            _ => unsupported("the type is unsupported"),
        }
    }
}

impl Display for CSType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let name = to_cs_primitive(&self.name);
        if self.is_ptr {
            match &self.descr {
                CSTyDescr::Struct(_s) => write!(f, "ref {}", name),
                _ => write!(f, "IntPtr /* {} */", name),
            }
        } else {
            write!(f, "{}", name)
        }
    }
}

#[derive(Clone)]
struct CSDelegate {
    output: Option<CSType>,
    args: Vec<(String, CSType)>,
}

impl CSDelegate {
    pub fn from_rust_fn(bare_fn: &syn::TypeBareFn) -> Result<Self> {
        if bare_fn.lifetimes.is_some() {
            return unsupported("lifetimes are unsupported!");
        }
        if bare_fn.variadic.is_some() {
            return unsupported("variadic is unsupported!");
        }

        let output = match bare_fn.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ref out_ty) => Some(CSType::from_rust_type(out_ty)?),
        };

        let mut args = Vec::new();
        for (i, fn_arg) in bare_fn.inputs.iter().enumerate() {
            let name = match fn_arg.name {
                Some((syn::BareFnArgName::Named(ref ident), _)) => ident.to_string(),
                _ => format!("_unnamed_{}", i),
            };
            args.push((name, CSType::from_rust_type(&fn_arg.ty)?));
        }

        Ok(CSDelegate { output, args })
    }

    pub fn return_ty(&self) -> CSType {
        self.output.clone().unwrap_or(CSType::void())
    }
}

struct CSConst {
    name: String,
    ty: CSType,
    value: String,
    cfg: SymbolConfig,
}

impl CSConst {
    pub fn from_rust_const(rust_const: &syn::ItemConst, cfg: SymbolConfig) -> Result<Self> {
        let value = if let syn::Expr::Lit(expr_lit) = &rust_const.expr.borrow() {
            if let syn::Lit::Int(lit_int) = &expr_lit.lit {
                lit_int.value().to_string()
            } else {
                return unsupported(format!(
                    "Unsupported const expression literal value: {:?}",
                    expr_lit
                ));
            }
        } else {
            return unsupported(format!(
                "Unsupported const expression value: {:?}",
                rust_const.expr
            ));
        };
        Ok(CSConst {
            name: munge_cs_name(rust_const.ident.to_string()),
            ty: CSType::from_rust_type(&rust_const.ty)?,
            value,
            cfg,
        })
    }
}

struct CSStructField {
    name: String,
    ty: CSType,
}

impl CSStructField {
    pub fn from_named_rust_field(rust_field: &syn::Field) -> Result<Self> {
        Ok(CSStructField {
            name: munge_cs_name(rust_field.ident.as_ref().unwrap().to_string()),
            ty: CSType::from_rust_type(&rust_field.ty)?,
        })
    }

    pub fn to_string(&self) -> String {
        to_cs_var_decl(&self.ty, &self.name)
    }
}

struct CSStruct {
    name: String,
    fields: Vec<CSStructField>,
    cfg: SymbolConfig,
}

impl CSStruct {
    pub fn from_rust_struct(rust_struct: &syn::ItemStruct, cfg: SymbolConfig) -> Result<Self> {
        let mut fields = vec![];

        if let syn::Fields::Named(rust_fields) = &rust_struct.fields {
            for rust_field in rust_fields.named.iter() {
                fields.push(CSStructField::from_named_rust_field(rust_field)?);
            }
        }
        Ok(CSStruct {
            name: rust_struct.ident.to_string(),
            fields,
            cfg,
        })
    }
}

impl Display for CSStruct {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "[Serializable]")?;
        writeln!(f, "[StructLayout(LayoutKind.Sequential)]")?;
        writeln!(f, "{} struct {} {{", self.cfg.access, self.name)?;
        for field in self.fields.iter() {
            writeln!(f, "{}{} {};", INDENT, self.cfg.access, field.to_string())?;
        }

        let constructor_args: Vec<String> =
            self.fields.iter().map(|field| field.to_string()).collect();
        writeln!(
            f,
            "\n{}{} {}({}) {{",
            INDENT,
            self.cfg.access,
            self.name,
            constructor_args.join(", ")
        )?;
        for field in self.fields.iter() {
            writeln!(
                f,
                "{}{}this.{} = {};",
                INDENT, INDENT, field.name, field.name
            )?;
        }
        writeln!(f, "{}}}", INDENT)?;

        writeln!(f, "}}")
    }
}

struct CSFuncArg {
    name: String,
    ty: CSType,
}

impl CSFuncArg {
    pub fn from_rust_arg_captured(rust_arg: &syn::ArgCaptured) -> Result<Self> {
        if let syn::Pat::Ident(pat_ident) = &rust_arg.pat {
            Ok(CSFuncArg {
                name: munge_cs_name(pat_ident.ident.to_string()),
                ty: CSType::from_rust_type(&rust_arg.ty)?,
            })
        } else {
            unsupported(format!(
                "captured arg pattern is unsupported: {:?}",
                rust_arg.pat
            ))
        }
    }

    pub fn to_string(&self) -> String {
        to_cs_var_decl(&self.ty, &self.name)
    }
}

struct CSFunc {
    name: String,
    args: Vec<CSFuncArg>,
    return_ty: Option<CSType>,
    cfg: SymbolConfig,
}

impl CSFunc {
    pub fn from_rust_fn(rust_fn: &syn::ItemFn, cfg: SymbolConfig) -> Result<Self> {
        let mut args = vec![];

        for input in rust_fn.decl.inputs.iter() {
            if let syn::FnArg::Captured(cap) = input {
                args.push(CSFuncArg::from_rust_arg_captured(&cap)?);
            } else {
                return unsupported(format!(
                    "Input for function '{}' is unsupported: {:?}",
                    rust_fn.ident.to_string(),
                    input
                ));
            }
        }

        let return_ty = match &rust_fn.decl.output {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ty) => Some(CSType::from_rust_type(&ty)?),
        };

        Ok(CSFunc {
            name: rust_fn.ident.to_string(),
            args,
            return_ty,
            cfg,
        })
    }
}

impl Display for CSFunc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let return_ty = match &self.return_ty {
            None => String::from("void"),
            Some(ty) => ty.to_string(),
        };
        let args: Vec<String> = self.args.iter().map(|arg| arg.to_string()).collect();
        write!(
            f,
            "{} static extern {} {}({});",
            self.cfg.access,
            return_ty,
            self.name,
            args.join(", ")
        )
    }
}

struct CSFile {
    class_name: String,
    dll_name: String,
    consts: Vec<CSConst>,
    structs: Vec<Rc<CSStruct>>,
    funcs: Vec<CSFunc>,
    type_defs: HashMap<String, CSTypeDef>,
    delegate_defs: HashMap<String, CSDelegate>,
}

impl CSFile {
    pub fn new(class_name: String, dll_name: String) -> Self {
        CSFile {
            class_name,
            dll_name,
            consts: vec![],
            structs: vec![],
            funcs: vec![],
            type_defs: HashMap::new(),
            delegate_defs: HashMap::new(),
        }
    }

    pub fn populate_from_rust_file(
        &mut self,
        rust_file: &syn::File,
        cfg_mgr: &SymbolConfigManager,
    ) -> Result<()> {
        for item in rust_file.items.iter() {
            match item {
                Item::Const(item_const) => {
                    if let Some(cfg) = cfg_mgr.get(&item_const.ident) {
                        let cs_const = error::add_ident(
                            CSConst::from_rust_const(&item_const, cfg),
                            &item_const.ident,
                        )?;
                        self.consts.push(cs_const);
                    }
                }
                Item::Struct(item_struct) => {
                    if let Some(cfg) = cfg_mgr.get(&item_struct.ident) {
                        let cs_struct = error::add_ident(
                            CSStruct::from_rust_struct(&item_struct, cfg),
                            &item_struct.ident,
                        )?;
                        self.structs.push(Rc::new(cs_struct));
                    }
                }
                Item::Fn(item_fn) => {
                    if item_fn.abi.is_some() {
                        if let Some(cfg) = cfg_mgr.get(&item_fn.ident) {
                            let cs_func = error::add_ident(
                                CSFunc::from_rust_fn(&item_fn, cfg),
                                &item_fn.ident,
                            )?;
                            self.funcs.push(cs_func);
                        }
                    }
                }
                Item::Type(item_type) => {
                    if let Some(_cfg) = cfg_mgr.get(&item_type.ident) {
                        let type_def = error::add_ident(
                            CSTypeDef::from_rust_type_def(&item_type),
                            &item_type.ident,
                        )?;

                        match type_def.ty.descr {
                            CSTyDescr::Delegate(d) => {
                                self.delegate_defs.insert(type_def.name.clone(), *d.clone());
                            }
                            _ => {
                                self.type_defs.insert(type_def.name.clone(), type_def);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        Ok(())
    }

    fn resolve_types(&mut self) -> Result<()> {
        let mut struct_map: HashMap<&str, &Rc<CSStruct>> = HashMap::new();

        for st in self.structs.iter() {
            struct_map.insert(&st.name, &st);
        }

        for func in self.funcs.iter_mut() {
            for arg in func.args.iter_mut() {
                if let Some(ty) = resolve_type_def(&arg.ty, &self.type_defs)? {
                    arg.ty = ty;
                }
                if let Some(st) = struct_map.get(&arg.ty.name.as_ref()) {
                    arg.ty.descr = CSTyDescr::Struct((*st).clone());
                }
            }
            if let Some(return_ty) = &func.return_ty {
                if let Some(ty) = resolve_type_def(return_ty, &self.type_defs)? {
                    func.return_ty = Some(ty);
                }
            }
        }

        for (_name, d) in self.delegate_defs.iter_mut() {
            if let Some(ty) = resolve_type_def(&d.return_ty(), &self.type_defs)? {
                d.output = Some(ty);
            }

            for (_arg_name, arg_ty) in d.args.iter_mut() {
                if let Some(ty) = resolve_type_def(&arg_ty, &self.type_defs)? {
                    *arg_ty = ty;
                }
            }
        }

        Ok(())
    }
}

impl Display for CSFile {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(
            f,
            "// This file has been auto-generated, please do not edit it.\n"
        )?;
        writeln!(f, "using System;")?;
        writeln!(f, "using System.Runtime.InteropServices;\n")?;

        for st in self.structs.iter() {
            writeln!(f, "{}", st)?;
        }
        writeln!(f, "{} class {} {{", CSAccess::default(), self.class_name)?;

        for con in self.consts.iter() {
            writeln!(
                f,
                "{}{} const {} {} = {};\n",
                INDENT, con.cfg.access, con.ty, con.name, con.value
            )?;
        }

        for (name, d) in self.delegate_defs.iter() {
            write!(f, "{}internal delegate {} {}(", INDENT, d.return_ty(), name)?;
            for (i, (name, ty)) in d.args.iter().enumerate() {
                if i == 0 {
                    write!(f, "{} {}", ty, name)?;
                } else {
                    write!(f, ", {} {}", ty, name)?;
                }
            }
            writeln!(f, ");\n")?;
        }

        for func in self.funcs.iter() {
            writeln!(f, "{}[DllImport(\"{}\")]", INDENT, self.dll_name)?;
            writeln!(f, "{}{}\n", INDENT, func)?;
        }
        writeln!(f, "}}")
    }
}

/// A [builder pattern] for the Rust-to-C# conversion process.
///
/// [builder pattern]: https://doc.rust-lang.org/1.0.0/style/ownership/builders.html
pub struct Builder {
    class_name: String,
    dll_name: String,
    rust_code: String,
    sconfig: SymbolConfigManager,
}

impl Builder {
    /// Creates a new instance with the following arguments:
    ///
    /// * `dll_name` is the name of the DLL that the C# `DllImport` attribute
    ///   will be bound to for all exported functions.
    ///
    /// * `rust_code` is the source Rust code to convert into C#.
    pub fn new<T: AsRef<str>>(dll_name: T, rust_code: String) -> Self {
        Builder {
            class_name: String::from("RustExports"),
            dll_name: String::from(dll_name.as_ref()),
            rust_code,
            sconfig: SymbolConfigManager::new(),
        }
    }

    /// Sets the name of the C# class that will contain all exported functions.
    /// If never called, the C# class will be called `RustExports`.
    pub fn class_name<T: AsRef<str>>(mut self, class_name: T) -> Self {
        self.class_name = String::from(class_name.as_ref());
        self
    }

    /// Specifies a list of Rust identifier patterns to be ignored (i.e., not
    /// exported to C#).
    ///
    /// The pattern syntax is currently very simple: if it ends with a `*`, it
    /// matches any Rust identifier that starts with the part of the pattern before
    /// the `*` (e.g., `Boop*` matches `BoopJones` and `BoopFoo`). Otherwise, it
    /// represents an exact match to a Rust identifier.
    pub fn ignore(mut self, ignores: &[&str]) -> Self {
        self.sconfig.ignores.add_static_array(ignores);
        self
    }

    /// Specifies that the given Rust identifier should be exported to C# with the
    /// given C# access modifier. By default, all exports are given the `internal`
    /// access modifier.
    pub fn access<T: AsRef<str>>(mut self, symbol_name: T, access: CSAccess) -> Self {
        self.sconfig
            .config_map
            .insert(String::from(symbol_name.as_ref()), SymbolConfig { access });
        self
    }

    /// Performs the conversion of source Rust code to C#.
    pub fn generate(self) -> Result<String> {
        let syntax = parse_file(&self.rust_code)?;
        let mut program = CSFile::new(self.class_name, self.dll_name);
        program.populate_from_rust_file(&syntax, &self.sconfig)?;
        program.resolve_types()?;
        Ok(format!("{}", program))
    }
}

fn parse_file(rust_code: &String) -> Result<syn::File> {
    match syn::parse_file(rust_code) {
        Ok(result) => Ok(result),
        Err(err) => Err(Error::SynError(err)),
    }
}

fn resolve_type_def(ty: &CSType, type_defs: &HashMap<String, CSTypeDef>) -> Result<Option<CSType>> {
    if let Some(type_def) = type_defs.get(&ty.name) {
        if ty.is_ptr && type_def.ty.is_ptr {
            return unsupported(format!(
                "double pointer to {} via type {} is unsupported!",
                type_def.ty.name, type_def.name
            ));
        }
        if let CSTyDescr::Delegate(_d) = &type_def.ty.descr {
            // resolve delegate type to named type
            return Ok(Some(CSType {
                name: ty.name.to_owned(),
                is_ptr: false,
                descr: CSTyDescr::Named,
            }));
        }
        Ok(Some(type_def.ty.clone()))
    } else {
        Ok(None)
    }
}

fn munge_cs_name(name: String) -> String {
    match name.as_ref() {
        "string" => String::from("str"),
        _ => name,
    }
}

fn to_cs_primitive<'a>(type_name: &'a str) -> &'a str {
    match type_name {
        "i8" => "SByte",
        "u8" => "Byte",
        "i16" => "Int16",
        "u16" => "UInt16",
        "i32" => "Int32",
        "u32" => "UInt32",
        "u64" => "Int64",
        "i64" => "UInt64",
        "isize" => "IntPtr",
        "usize" => "UIntPtr",
        "f32" => "Single",
        "f64" => "Double",
        _ => type_name,
    }
}

fn to_cs_var_decl<T: AsRef<str>>(ty: &CSType, name: T) -> String {
    format!("{} {}", ty, name.as_ref())
}

fn unsupported<T, S>(msg: S) -> Result<T>
where
    S: Into<String>,
{
    Err(Error::UnsupportedError(msg.into(), None))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_it_errors_on_invalid_rust_code() {
        let err = Builder::new("Blarg", String::from("HELLO THERE"))
            .generate()
            .unwrap_err();
        let err_msg = format!("{}", err);
        assert_eq!(err_msg, "Couldn't parse Rust code: expected `!`");
    }

    #[test]
    fn test_it_errors_on_unsupported_rust_code() {
        let err = Builder::new(
            "Blarg",
            String::from(
                r#"
            pub type MyFunkyThing<'a> = &'a u8;
        "#,
            ),
        )
        .generate()
        .unwrap_err();
        assert_eq!(
            format!("{}", err),
            "Unable to export C# code while processing symbol \"MyFunkyThing\" because the type is unsupported"
        );
    }
}
