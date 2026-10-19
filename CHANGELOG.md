# Changelog for `fisher`

## Unreleased

- Renamed: the product is now Theorem Fisher (Fisher for short), formerly Gauss or Open Gauss. The text, the prompts, the souls, the banner and skins, the identifiers, the environment variables (`FISHER_*`), the home (`~/.fisher`), the project directory (`.fisher/`), the config root (`fisher:`), the native backend (`fisher`) and the toolsets (`fisher-cli`, `fisher-acp`, ...) all say Fisher.
- The old names still work as a fallback, read side only: `FISHER_HOME`, else `GAUSS_HOME`, else `~/.fisher` or `~/.gauss` (whichever exists); `.fisher/` else `.gauss/` in a project; `FISHER_X` else `GAUSS_X`; `fisher.<path>` else `gauss.<path>` per config leaf; `--backends gauss` (and the config and env spellings) select the backend and print as `fisher`; `gauss-cli` resolves to `fisher-cli`. Where Fisher sets a variable for a child process it sets both names.
- Saving `config.yaml` writes the settings under `fisher:` and keeps an existing `gauss:` section untouched, so an older build that shares the file still works.
- Tools: the bench and e2e scripts take the key from `~/.fisher/.env`, else `~/.gauss/.env`, else the environment, and use `FISHER_HOME`; `tools/gen-defaults.py` applies the rename to what it imports from OpenGauss, so a regeneration reproduces the committed data. The corpus recorders under `tools/corpus/` still drive upstream's own `gauss` and keep its names.

## 0.2.0.0

- Release candidate for the cutover (phase E), macOS arm64.
- ACP server (`fisher acp`): the editor protocol upstream's acp_adapter speaks, over stdio.
- Cron (`fisher cron ...`, the `cronjob` tool and the `fisher cron serve` ticker): upstream's job store, schedules and tick.
- Proof-search swarm (phase D), Lean and MCP work (phase C), the agent and tools (phase B) and the core (phase A).

## 0.1.0.0

- Initial release
