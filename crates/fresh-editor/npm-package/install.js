const { install } = require('./binary-install');
install().catch(err => { console.error(err); process.exit(1); });
