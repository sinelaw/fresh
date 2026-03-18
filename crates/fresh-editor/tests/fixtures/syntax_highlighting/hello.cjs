// CommonJS (.cjs) syntax highlighting test
const path = require('path');

function greet(name) {
    return `Hello, ${name}!`;
}

module.exports = { greet };

const config = {
    version: "1.0",
    enabled: true,
};
