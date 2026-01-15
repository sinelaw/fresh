use std::sync::Once;

/// Initialize the global tracing subscriber once (used by tests that run with `RUST_LOG`).
///
/// This sets up logging with the RUST_LOG environment variable, but filters out
/// noisy debug logs from SWC to keep test output clean.
pub fn init_tracing_from_env() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        // Build filter from environment, but suppress noisy SWC library logs
        let env_filter = tracing_subscriber::EnvFilter::from_default_env()
            .add_directive("swc_ecma_transforms_base=warn".parse().unwrap())
            .add_directive("swc_common=warn".parse().unwrap());

        let subscriber = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stdout);
        let _ = subscriber.try_init();
    });
}
