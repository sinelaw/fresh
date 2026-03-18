// ES module (.mjs) syntax highlighting test
import { readFile } from 'fs/promises';

export function greet(name) {
    return `Hello, ${name}!`;
}

const config = {
    version: "1.0",
    enabled: true,
};

export default config;
