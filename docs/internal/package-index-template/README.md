# Fresh Package Index

This repository contains the official package index for Fresh Editor plugins and themes.

## Structure

```
package-index/
├── README.md           # This file
├── plugins.json        # Plugin registry
├── themes.json         # Theme registry
├── blocklist.json      # Known malicious packages
└── schemas/
    ├── registry.schema.json    # JSON Schema for registry files
    └── package.schema.json     # JSON Schema for package manifests
```

## Adding a Package

1. Fork this repository
2. Add your package entry to `plugins.json` or `themes.json`
3. Submit a pull request

### Plugin Entry Format

```json
{
  "my-plugin-name": {
    "description": "Short description of what the plugin does",
    "repository": "https://github.com/username/fresh-my-plugin",
    "author": "Your Name",
    "license": "MIT",
    "keywords": ["keyword1", "keyword2"],
    "latest_version": "1.0.0",
    "fresh_min_version": "0.1.80"
  }
}
```

### Theme Entry Format

```json
{
  "my-theme-name": {
    "description": "Short description of the theme",
    "repository": "https://github.com/username/fresh-my-theme",
    "author": "Your Name",
    "license": "MIT",
    "variants": ["dark", "light"],
    "keywords": ["dark", "minimal", "colorful"]
  }
}
```

## Package Manifest

Your package repository should contain a `package.json` at its root:

```json
{
  "name": "my-plugin",
  "version": "1.0.0",
  "description": "Plugin description",
  "type": "plugin",
  "author": "Your Name <email@example.com>",
  "license": "MIT",
  "repository": "https://github.com/username/fresh-my-plugin",

  "fresh": {
    "min_version": "0.1.80",
    "entry": "main.ts"
  },

  "keywords": ["keyword1", "keyword2"]
}
```

For theme packages:

```json
{
  "name": "catppuccin",
  "version": "1.0.0",
  "description": "Soothing pastel theme collection",
  "type": "theme-pack",
  "author": "Theme Author",
  "license": "MIT",
  "repository": "https://github.com/catppuccin/fresh",

  "fresh": {
    "min_version": "0.1.75",
    "themes": [
      { "file": "mocha.json", "name": "Catppuccin Mocha", "variant": "dark" },
      { "file": "latte.json", "name": "Catppuccin Latte", "variant": "light" }
    ]
  }
}
```

## Using This Registry

Fresh automatically syncs this registry when you run `pkg: Sync Registry` from the command palette.

You can also add additional registry sources in your config:

```json
{
  "packages": {
    "sources": [
      "https://github.com/sinelaw/fresh-plugins-registry",
      "https://github.com/my-org/private-plugins"
    ]
  }
}
```

## Direct Installation

You don't need to be listed here to install a package. Users can install any git repository directly:

```
pkg: Install from URL
```

Then enter the git URL of your plugin or theme repository.

## License

The index itself is public domain (CC0). Individual packages retain their own licenses.
