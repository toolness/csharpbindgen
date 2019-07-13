use std::collections::HashMap;

use crate::ignores::Ignores;
use crate::CSAccess;

#[derive(Clone, Copy)]
pub struct SymbolConfig {
    pub access: CSAccess
}

impl Default for SymbolConfig {
    fn default() -> Self {
        SymbolConfig {
            access: CSAccess::default()
        }
    }
}

pub struct SymbolConfigManager {
    pub ignores: Ignores,
    pub config_map: HashMap<String, SymbolConfig>
}

impl SymbolConfigManager {
    pub fn new() -> Self {
        SymbolConfigManager {
            ignores: Ignores::new(),
            config_map: HashMap::new()
        }
    }

    pub fn get(&self, ident: &syn::Ident) -> Option<SymbolConfig> {
        let string = ident.to_string();
        if self.ignores.ignore(&string) {
            None
        } else {
            if let Some(cfg) = self.config_map.get(&string) {
                Some(*cfg)
            } else {
                Some(SymbolConfig::default())
            }
        }
    }
}
