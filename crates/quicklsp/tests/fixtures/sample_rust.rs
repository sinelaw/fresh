// QuickLSP evaluation fixture: a realistic multi-construct Rust file.
// This file is indexed by the LSP evaluation test to exercise all features.

const MAX_RETRIES: u32 = 3;
const DEFAULT_TIMEOUT: u64 = 5000;

struct Config {
    host: String,
    port: u16,
    max_connections: usize,
}

enum Status {
    Active,
    Inactive,
    Error(String),
}

trait Handler {
    fn handle(&self, request: &Request) -> Response;
    fn name(&self) -> &str;
}

struct Request {
    method: String,
    path: String,
    body: Option<String>,
}

struct Response {
    status: u16,
    body: String,
}

fn create_config() -> Config {
    Config {
        host: "localhost".to_string(),
        port: 8080,
        max_connections: 100,
    }
}

fn process_request(config: &Config, request: &Request) -> Response {
    let status = if request.method == "GET" {
        Status::Active
    } else {
        Status::Inactive
    };

    let body = format!(
        "Handled {} {} on {}:{}",
        request.method, request.path, config.host, config.port
    );

    Response {
        status: 200,
        body,
    }
}

struct Server {
    config: Config,
    handlers: Vec<Box<dyn Handler>>,
}

impl Server {
    fn new(config: Config) -> Self {
        Server {
            config,
            handlers: Vec::new(),
        }
    }

    fn add_handler(&mut self, handler: Box<dyn Handler>) {
        self.handlers.push(handler);
    }

    fn run(&self) {
        let config = &self.config;
        for i in 0..MAX_RETRIES {
            let timeout = DEFAULT_TIMEOUT * (i as u64 + 1);
            println!("Attempt {} with timeout {}ms on port {}", i, timeout, config.port);
        }
    }
}

mod utils {
    pub fn sanitize_input(input: &str) -> String {
        input.trim().to_lowercase()
    }

    pub fn validate_port(port: u16) -> bool {
        port > 0 && port < 65535
    }
}

type StatusCode = u16;
type HandlerResult = Result<Response, String>;

fn validate_request(request: &Request) -> HandlerResult {
    if request.path.is_empty() {
        return Err("Empty path".to_string());
    }
    Ok(Response {
        status: 200,
        body: "OK".to_string(),
    })
}

// Unicode identifiers
fn données_utilisateur() -> String {
    "user data".to_string()
}

struct Über {
    wert: u32,
}

// Nested function-like constructs
fn outer() {
    fn inner() {
        let _ = 42;
    }
    inner();
}

const FINAL_STATUS: &str = "complete";
static GLOBAL_COUNTER: u32 = 0;
