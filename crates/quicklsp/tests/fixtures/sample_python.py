# QuickLSP evaluation fixture: a realistic Python file.

MAX_RETRIES = 3
DEFAULT_TIMEOUT = 5000

# Server configuration holding host and port.
class Config:
    def __init__(self, host, port):
        self.host = host
        self.port = port

    def display(self):
        """Display the config as host:port."""
        print(f"{self.host}:{self.port}")

# Main server class that manages handlers and request dispatching.
class Server:
    def __init__(self, config):
        self.config = config
        self.handlers = []

    # Register a new handler with the server.
    def add_handler(self, handler):
        self.handlers.append(handler)

    def run(self):
        """Start the server and retry on failure."""
        for i in range(MAX_RETRIES):
            timeout = DEFAULT_TIMEOUT * (i + 1)
            print(f"Attempt {i} with timeout {timeout}ms")

def process_request(config, request):
    """Process an incoming request using the given config."""
    return {"status": 200, "body": f"OK from {config.host}"}

# Validate and sanitize user input, raising ValueError if empty.
def validate_input(data):
    if not data:
        raise ValueError("Empty input")
    return data.strip()

# Base handler class for processing requests.
class Handler:
    def handle(self, request):
        """Handle a request by delegating to process_request."""
        return process_request(Config("localhost", 8080), request)
