// ES module TypeScript (.mts) syntax highlighting test
export interface Config {
    version: string;
    enabled: boolean;
}

export function greet(name: string): string {
    return `Hello, ${name}!`;
}

const config: Config = {
    version: "1.0",
    enabled: true,
};

export default config;
