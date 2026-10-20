# Changelog for `fisher`

## Unreleased

- Renamed: the product is now Theorem Fisher (Fisher for short). The text, the prompts, the souls, the banner and skins, the identifiers, the environment variables (`FISHER_*`), the home (`~/.fisher`), the project directory (`.fisher/`), the config root (`fisher:`), the native backend (`fisher`) and the toolsets (`fisher-cli`, `fisher-acp`, ...) all say Fisher.
- Clean break from the previous names: Fisher reads and writes only its own. There is no fallback for the home, the project directory, environment variables, config keys, the backend or the toolsets; a value under a previous name is not read, a previous backend name is refused with the list of valid ones, and child processes get only `FISHER_*` variables. Saving `config.yaml` writes only `fisher:`. To keep a home from before the rename, move your previous home directory to `~/.fisher`, rename its config root to `fisher:` and the previous prefix of the variables in its `.env` to `FISHER_`, and rename each project's previous project directory to `.fisher`. Upstream citations in code and docs name the upstream repository (see `UPSTREAM`) neutrally, and the old name is left only in `LICENSE` and `UPSTREAM`.
- Removed the Nous Portal provider: its login, model listing, status block, setup and model menu entries and its auxiliary backend are gone. Naming it (argument, `model.provider`, `FISHER_INFERENCE_PROVIDER`, or the active provider in `auth.json`) is an error that says to choose another provider with `fisher setup model`; `fisher status` shows that message as the provider, `fisher model` shows it as a warning and offers its menu, and `fisher login` or `logout --provider nous` prints it instead of an invalid choice. OpenRouter requests send a neutral referer.
- `fisher status` says the gateway is not part of Fisher instead of probing a gateway service that no Fisher command installs.
- Tools: the bench scripts take the key from the `.env` of `$FISHER_HOME` (else `~/.fisher`), else the environment; the live e2e scripts (`e2e-b3`, `e2e-b4`, `e2e-clm`, `e2e-e2`) and `tools/gate-c2.sh` copy `.env` and `config.yaml` (and, for B3, B4 and C2, `auth.json`) from `~/.fisher` into a throwaway `FISHER_HOME`, never from `$FISHER_HOME`, and `e2e-e1.py` runs `fisher acp` with the home it finds; bench readers show a run under the backend name it recorded. `tools/gen-defaults.py` applies the rename to what it imports from upstream and leaves out the Nous Portal provider, so a regeneration reproduces the committed data. The corpus recorders under `tools/corpus/` take the upstream program from the required `UPSTREAM_CLI` variable, build upstream's names at run time and write Fisher's names. Their output goes through `tools/corpus/post.py`, which removes the Nous Portal provider and upstream's referer the way the committed goldens have them. The A3 gate (upstream and Fisher sharing one home) is retired.

## 0.2.0.0

- Release candidate for the cutover (phase E), macOS arm64.
- ACP server (`fisher acp`): the editor protocol upstream's acp_adapter speaks, over stdio.
- Cron (`fisher cron ...`, the `cronjob` tool and the `fisher cron serve` ticker): upstream's job store, schedules and tick.
- Proof-search swarm (phase D), Lean and MCP work (phase C), the agent and tools (phase B) and the core (phase A).

## 0.1.0.0

- Initial release
