// QuickLSP evaluation fixture: a realistic JavaScript/TypeScript file.

const MAX_RETRIES = 3;
const DEFAULT_TIMEOUT = 5000;

/** Configuration for the server instance. */
interface Config {
    host: string;
    port: number;
    maxConnections: number;
}

interface Handler {
    handle(request: Request): Response;
    name: string;
}

type StatusCode = number;
type HandlerResult = Response | Error;

class Request {
    method: string;
    path: string;
    body?: string;

    constructor(method: string, path: string) {
        this.method = method;
        this.path = path;
    }
}

class Response {
    status: StatusCode;
    body: string;

    constructor(status: StatusCode, body: string) {
        this.status = status;
        this.body = body;
    }
}

/** Create a new default server configuration. */
function createConfig(): Config {
    return {
        host: "localhost",
        port: 8080,
        maxConnections: 100,
    };
}

/**
 * Process an incoming request with the given configuration.
 * Routes the request and returns an appropriate response.
 */
function processRequest(config: Config, request: Request): Response {
    const body = `Handled ${request.method} ${request.path} on ${config.host}:${config.port}`;
    return new Response(200, body);
}

class Server {
    config: Config;
    handlers: Handler[];

    constructor(config: Config) {
        this.config = config;
        this.handlers = [];
    }

    addHandler(handler: Handler) {
        this.handlers.push(handler);
    }

    run() {
        for (let i = 0; i < MAX_RETRIES; i++) {
            const timeout = DEFAULT_TIMEOUT * (i + 1);
            console.log(`Attempt ${i} with timeout ${timeout}ms`);
        }
    }
}

enum Status {
    Active = "active",
    Inactive = "inactive",
}

function validateRequest(request: Request): HandlerResult {
    if (!request.path) {
        return new Error("Empty path");
    }
    return new Response(200, "OK");
}

let globalCounter = 0;
var legacyFlag = true;
