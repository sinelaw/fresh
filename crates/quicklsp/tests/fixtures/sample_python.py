# QuickLSP evaluation fixture: a realistic Python file.

MAX_RETRIES = 3
DEFAULT_TIMEOUT = 5000

class Config:
    def __init__(self, host, port):
        self.host = host
        self.port = port

    def display(self):
        print(f"{self.host}:{self.port}")

class Server:
    def __init__(self, config):
        self.config = config
        self.handlers = []

    def add_handler(self, handler):
        self.handlers.append(handler)

    def run(self):
        for i in range(MAX_RETRIES):
            timeout = DEFAULT_TIMEOUT * (i + 1)
            print(f"Attempt {i} with timeout {timeout}ms")

def process_request(config, request):
    """Process an incoming request using the given config."""
    return {"status": 200, "body": f"OK from {config.host}"}

def validate_input(data):
    if not data:
        raise ValueError("Empty input")
    return data.strip()

class Handler:
    def handle(self, request):
        return process_request(Config("localhost", 8080), request)
