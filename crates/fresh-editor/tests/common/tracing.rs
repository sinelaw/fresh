use std::sync::Once;

/// Initialize the global tracing subscriber once (used by tests that run with `RUST_LOG`).
///
/// This sets up logging with the RUST_LOG environment variable, but filters out
/// noisy debug logs from SWC to keep test output clean.
pub fn init_tracing_from_env() {
    static INIT: Once = Once::new();
    INIT.call_once(|| {
        // Build filter from environment with a default of WARN level so that
        // diagnostic breadcrumbs and signal-handler dumps are visible without
        // drowning in verbose plugin-loading noise.  Set RUST_LOG=debug to
        // get the full firehose.
        let env_filter = tracing_subscriber::EnvFilter::builder()
            .with_default_directive(tracing_subscriber::filter::LevelFilter::WARN.into())
            .from_env_lossy()
            .add_directive("swc_ecma_transforms_base=warn".parse().unwrap())
            .add_directive("swc_common=warn".parse().unwrap());

        let subscriber = tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .with_writer(std::io::stdout);
        let _ = subscriber.try_init();
    });
}
