#!/usr/bin/env node
const { spawn } = require('child_process');
const path = require('path');
const { getBinaryInfo } = require('./binary');

const info = getBinaryInfo();
const binPath = path.join(__dirname, 'bin', info.binaryName);

const child = spawn(binPath, process.argv.slice(2), { stdio: 'inherit' });
child.on('exit', (code) => process.exit(code || 0));
