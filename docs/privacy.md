# Privacy & Telemetry

Fresh checks for new versions to notify you when upgrades are available. Alongside this, it sends basic anonymous telemetry to help understand usage patterns.

The data collected includes:

- Fresh version
- Operating system and architecture (e.g., `linux-x86_64`, `macos-aarch64`)
- Terminal type (the `TERM` environment variable)
- A random installation ID, created on first run and stored in Fresh's data directory

No personal data, file contents, or usage behavior is collected. The check runs at most once a day, when Fresh starts.

## Disabling Upgrade Checks and Telemetry

You can disable both upgrade checking and telemetry with the same flag:

**Command line flag:**
```bash
fresh --no-upgrade-check
```

**Settings UI:** open **Settings** (**Edit → Settings...**), go to **General** and turn off **Check For Updates**, then **Save**.

![Settings at General, with Check For Updates selected](/images/settings-check-for-updates.png)

::: details Show JSON
In `~/.config/fresh/config.json`:
```json
{
  "check_for_updates": false
}
```
:::