pub(crate) struct GlobalConfig {
    pub use_safe_handles: bool,
}

impl Default for GlobalConfig {
    fn default() -> Self {
        GlobalConfig {
            use_safe_handles: false,
        }
    }
}
