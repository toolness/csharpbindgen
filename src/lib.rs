use std::collections::HashMap;
use std::borrow::Borrow;
use std::fmt::{Formatter, Display};
use std::fmt;
use std::rc::Rc;
use syn::Item;

pub mod error;
mod symbol_config;
mod ignores;

use symbol_config::{SymbolConfigManager, SymbolConfig};
use error::{Error, Result};

const INDENT: &'static str = "    ";

#[derive(Clone, Copy)]
pub enum CSAccess {
    Private,
    Protected,
    Internal,
    Public
}

impl Display for CSAccess {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            CSAccess::Private => "private",
            CSAccess::Protected => "protected",
            CSAccess::Internal => "internal",
            CSAccess::Public => "public"
        })
    }
}

impl Default for CSAccess {
    fn default() -> Self {
        CSAccess::Internal
    }
}

struct CSTypeDef {
    name: String,
    ty: CSType
}

impl CSTypeDef {
    pub fn from_rust_type_def(rust_type_def: &syn::ItemType) -> Result<Self> {
        Ok(CSTypeDef {
            name: rust_type_def.ident.to_string(),
            ty: CSType::from_rust_type(&rust_type_def.ty)?
        })
    }
}

#[derive(Clone)]
struct CSType {
    name: String,
    is_ptr: bool,
    st: Option<Rc<CSStruct>>
}

impl CSType {
    pub fn from_rust_type(rust_type: &syn::Type) -> Result<Self> {
        match rust_type {
            syn::Type::Path(type_path) => {
                let last = type_path.path.segments.last()
                  .expect("expected at least one path segment on type!");
                Ok(CSType {
                    name: last.value().ident.to_string(),
                    is_ptr: false,
                    st: None
                })
            },
            syn::Type::Ptr(type_ptr) => {
                let mut wrapped_type = CSType::from_rust_type(&type_ptr.elem)?;
                if wrapped_type.is_ptr {
                    return unsupported(format!(
                        "double pointers for {} are unsupported!", wrapped_type.name
                    ));
                }
                wrapped_type.is_ptr = true;
                Ok(wrapped_type)
            },
            _ => {
                unsupported(format!("the type is unsupported"))
            }
        }
    }
}

impl Display for CSType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let name = to_cs_primitive(&self.name);
        if self.is_ptr {
            if self.st.is_some() {
                write!(f, "ref {}", name)
            } else {
                write!(f, "IntPtr /* {} */", name)
            }
        } else {
            write!(f, "{}", name)
        }
    }
}

struct CSConst {
    name: String,
    ty: CSType,
    value: String,
    cfg: SymbolConfig
}

impl CSConst {
    pub fn from_rust_const(rust_const: &syn::ItemConst, cfg: SymbolConfig) -> Result<Self> {
        let value = if let syn::Expr::Lit(expr_lit) = &rust_const.expr.borrow() {
            if let syn::Lit::Int(lit_int) = &expr_lit.lit {
                lit_int.value().to_string()
            } else {
                return unsupported(format!(
                    "Unsupported const expression literal value: {:?}", expr_lit))
            }
        } else {
            return unsupported(format!(
                "Unsupported const expression value: {:?}", rust_const.expr))
        };
        Ok(CSConst {
            name: munge_cs_name(rust_const.ident.to_string()),
            ty: CSType::from_rust_type(&rust_const.ty)?,
            value,
            cfg
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
            ty: CSType::from_rust_type(&rust_field.ty)?
        })
    }

    pub fn to_string(&self) -> String {
        to_cs_var_decl(&self.ty, &self.name)
    }
}

struct CSStruct {
    name: String,
    fields: Vec<CSStructField>,
    cfg: SymbolConfig
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
            cfg
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

        let constructor_args: Vec<String> = self.fields
          .iter()
          .map(|field| field.to_string())
          .collect();
        writeln!(f, "\n{}{} {}({}) {{", INDENT, self.cfg.access, self.name, constructor_args.join(", "))?;
        for field in self.fields.iter() {
            writeln!(f, "{}{}this.{} = {};", INDENT, INDENT, field.name, field.name)?;
        }
        writeln!(f, "{}}}", INDENT)?;

        writeln!(f, "}}")
    }
}

struct CSFuncArg {
    name: String,
    ty: CSType
}

impl CSFuncArg {
    pub fn from_rust_arg_captured(rust_arg: &syn::ArgCaptured) -> Result<Self> {
        if let syn::Pat::Ident(pat_ident) = &rust_arg.pat {
            Ok(CSFuncArg {
                name: munge_cs_name(pat_ident.ident.to_string()),
                ty: CSType::from_rust_type(&rust_arg.ty)?
            })
        } else {
            unsupported(format!("captured arg pattern is unsupported: {:?}", rust_arg.pat))
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
    cfg: SymbolConfig
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
            syn::ReturnType::Type(_, ty) => {
                Some(CSType::from_rust_type(&ty)?)
            }
        };

        Ok(CSFunc {
            name: rust_fn.ident.to_string(),
            args,
            return_ty,
            cfg
        })
    }
}

impl Display for CSFunc {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let return_ty = match &self.return_ty {
            None => String::from("void"),
            Some(ty) => ty.to_string()
        };
        let args: Vec<String> = self.args
          .iter()
          .map(|arg| arg.to_string())
          .collect();
        write!(f, "{} static extern {} {}({});", self.cfg.access, return_ty, self.name, args.join(", "))
    }
}

struct CSFile {
    class_name: String,
    dll_name: String,
    consts: Vec<CSConst>,
    structs: Vec<Rc<CSStruct>>,
    funcs: Vec<CSFunc>,
    type_defs: HashMap<String, CSTypeDef>
}

impl CSFile {
    pub fn new(class_name: String, dll_name: String) -> Self {
        CSFile {
            class_name,
            dll_name,
            consts: vec![],
            structs: vec![],
            funcs: vec![],
            type_defs: HashMap::new()
        }
    }

    pub fn populate_from_rust_file(
        &mut self,
        rust_file: &syn::File,
        cfg_mgr: &SymbolConfigManager
    ) -> Result<()> {
        for item in rust_file.items.iter() {
            match item {
                Item::Const(item_const) => {
                    if let Some(cfg) = cfg_mgr.get(&item_const.ident) {
                        let cs_const = error::add_ident(
                            CSConst::from_rust_const(&item_const, cfg), &item_const.ident)?;
                        self.consts.push(cs_const);
                    }
                },
                Item::Struct(item_struct) => {
                    if let Some(cfg) = cfg_mgr.get(&item_struct.ident) {
                        let cs_struct = error::add_ident(
                            CSStruct::from_rust_struct(&item_struct, cfg), &item_struct.ident)?;
                        self.structs.push(Rc::new(cs_struct));
                    }
                },
                Item::Fn(item_fn) => {
                    if item_fn.abi.is_some() {
                        if let Some(cfg) = cfg_mgr.get(&item_fn.ident) {
                            let cs_func = error::add_ident(
                                CSFunc::from_rust_fn(&item_fn, cfg), &item_fn.ident)?;
                            self.funcs.push(cs_func);
                        }
                    }
                },
                Item::Type(item_type) => {
                    if let Some(_cfg) = cfg_mgr.get(&item_type.ident) {
                        let type_def = error::add_ident(
                            CSTypeDef::from_rust_type_def(&item_type), &item_type.ident)?;
                        self.type_defs.insert(type_def.name.clone(), type_def);
                    }
                },
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
                    arg.ty.st = Some((*st).clone());
                }
            }
            if let Some(return_ty) = &func.return_ty {
                if let Some(ty) = resolve_type_def(return_ty, &self.type_defs)? {
                    func.return_ty = Some(ty);
                }
            }
        }

        Ok(())
    }
}

impl Display for CSFile {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "// This file has been auto-generated, please do not edit it.\n")?;
        writeln!(f, "using System;")?;
        writeln!(f, "using System.Runtime.InteropServices;\n")?;

        for st in self.structs.iter() {
            writeln!(f, "{}", st)?;
        }
        writeln!(f, "{} class {} {{", CSAccess::default(), self.class_name)?;
        for con in self.consts.iter() {
            writeln!(f, "{}{} const {} {} = {};\n", INDENT, con.cfg.access, con.ty, con.name, con.value)?;
        }
        for func in self.funcs.iter() {
            writeln!(f, "{}[DllImport(\"{}\")]", INDENT, self.dll_name)?;
            writeln!(f, "{}{}\n", INDENT, func)?;
        }
        writeln!(f, "}}")
    }
}

pub struct Builder {
    class_name: String,
    dll_name: String,
    rust_code: String,
    sconfig: SymbolConfigManager
}

impl Builder {
    pub fn new<T: AsRef<str>>(
        dll_name: T,
        rust_code: String
    ) -> Self {
        Builder {
            class_name: String::from("RustExports"),
            dll_name: String::from(dll_name.as_ref()),
            rust_code,
            sconfig: SymbolConfigManager::new()
        }
    }

    pub fn class_name<T: AsRef<str>>(mut self, class_name: T) -> Self {
        self.class_name = String::from(class_name.as_ref());
        self
    }

    pub fn ignore(mut self, ignores: &[&str]) -> Self {
        self.sconfig.ignores.add_static_array(ignores);
        self
    }

    pub fn access<T: AsRef<str>>(mut self, symbol_name: T, access: CSAccess) -> Self {
        self.sconfig.config_map.insert(String::from(symbol_name.as_ref()), SymbolConfig {
            access
        });
        self
    }

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
        Err(err) => Err(Error::SynError(err))
    }
}

fn resolve_type_def(ty: &CSType, type_defs: &HashMap<String, CSTypeDef>) -> Result<Option<CSType>> {
    if let Some(type_def) = type_defs.get(&ty.name) {
        if ty.is_ptr && type_def.ty.is_ptr {
            unsupported(format!(
                "double pointer to {} via type {} is unsupported!",
                type_def.ty.name,
                type_def.name
            ))
        } else {
            Ok(Some(type_def.ty.clone()))
        }
    } else {
        Ok(None)
    }
}

fn munge_cs_name(name: String) -> String {
    match name.as_ref() {
        "string" => String::from("str"),
        _ => name
    }
}

fn to_cs_primitive<'a>(type_name: &'a str) -> &'a str {
    match type_name {
        "u8" => "byte",
        "f32" => "float",
        "i32" => "Int32",
        "u32" => "UInt32",
        "usize" => "UIntPtr",
        _ => type_name
    }
}

fn to_cs_var_decl<T: AsRef<str>>(ty: &CSType, name: T) -> String {
    format!("{} {}", ty, name.as_ref())
}

fn unsupported<T>(msg: String) -> Result<T> {
    Err(Error::UnsupportedError(msg, None))
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot_matches;

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
        let err = Builder::new("Blarg", String::from(r#"
            pub type MyFunkyThing = fn() -> void;
        "#)).generate().unwrap_err();
        assert_eq!(
            format!("{}", err),
            "Unable to export C# code while processing symbol \"MyFunkyThing\" because the type is unsupported"
        );
    }

    #[test]
    fn test_it_works() {
        let rust_code = r#"
            pub const BOOP: u8 = 1;
            pub const IGNORE_THIS_CONST: u8 = 2;

            #[repr(C)]
            pub struct MyStruct {
                pub foo: f32,
                pub bar: f32
            }

            #[repr(C)]
            pub struct IgnoreThisStruct { pub foo: f32 }

            pub type MyOpaqueRef = *mut MyOpaqueStruct;

            pub unsafe extern "C" fn public_func() {}

            #[repr(C)]
            pub struct PublicStruct {
                pub bop: i32,
            }

            fn unexported_func() {}

            pub unsafe extern "C" fn blarg(
                a: i32,
                b: *const MyStruct,
                c: MyOpaqueRef
            ) -> u8 { 120 }

            pub unsafe extern "C" fn ignore_this_func(a: i32) -> u8 { 120 }
        "#;

        let code = Builder::new("MyDll", String::from(rust_code))
          .class_name("MyStuff")
          .ignore(&["ignore_*", "IGNORE_*", "Ignore*"])
          .access("public_func", CSAccess::Public)
          .access("PublicStruct", CSAccess::Public)
          .generate()
          .unwrap();

        assert_snapshot_matches!("main_example", code);
    }
}
