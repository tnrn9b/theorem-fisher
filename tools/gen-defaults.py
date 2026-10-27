#!/usr/bin/env python3
"""Regenerate the data files Fisher takes from a checkout of the upstream
repository (see UPSTREAM).

Usage: tools/gen-defaults.py /path/to/upstream-checkout

Fisher's own names are applied on the way (tools/gen_defaults_rename.py):
upstream's product name becomes Fisher and Theorem Fisher in the text, the
config root key is `fisher`, the default toolset `fisher-cli`, the env-var
catalogue says FISHER_*, the arXiv User-Agent says FisherAgent, and the
banner logo is FISHER. The Nous Portal provider, which Fisher removed, is
left out of the env-var catalogue and the fallback comment. Upstream's
version and release date are kept.

Run with a Python that has upstream's dependencies (rich 15.0.0 among
them). The checkout must be at the commit named in UPSTREAM; the script
refuses otherwise, so the data files always say which upstream they came
from.
"""
import importlib
import json
import pathlib
import subprocess
import sys

import yaml

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from gen_defaults_rename import (  # noqa: E402
    FISHER_AGENT_LOGO, FISHER_AGENT_LOGO_ASCII, OLD, drop_removed_env,
    drop_removed_lines, fisher_data, fisher_text)

repo = pathlib.Path(__file__).resolve().parent.parent
pinned = (repo / "UPSTREAM").read_text().split()[0]
src = pathlib.Path(sys.argv[1]).resolve()
head = subprocess.run(["git", "-C", str(src), "rev-parse", "HEAD"],
                      capture_output=True, text=True, check=True).stdout.strip()
if head != pinned:
    sys.exit(f"{src} is at {head}, UPSTREAM pins {pinned}")

sys.path.insert(0, str(src))
# Upstream's package is named after its product.
pkg = OLD.lower() + "_cli"
c = importlib.import_module(pkg + ".config")
DEFAULT_SOUL_MD = importlib.import_module(pkg + ".default_soul").DEFAULT_SOUL_MD

out = repo / "fisher-core" / "data"
out.mkdir(parents=True, exist_ok=True)
(out / "default-config.yaml").write_text(
    yaml.safe_dump(fisher_data(c.DEFAULT_CONFIG), sort_keys=False))
(out / "default-soul.md").write_text(fisher_text(DEFAULT_SOUL_MD))
(out / "security-comment.txt").write_text(fisher_text(c._SECURITY_COMMENT))
(out / "fallback-comment.txt").write_text(fisher_text(drop_removed_lines(c._FALLBACK_COMMENT)))
# Ordered pairs, because `config check` prints them in upstream's order.
(out / "env-vars.json").write_text(json.dumps(fisher_data({
    "required": drop_removed_env([[k, v] for k, v in c.REQUIRED_ENV_VARS.items()]),
    "optional": drop_removed_env([[k, v] for k, v in c.OPTIONAL_ENV_VARS.items()]),
    "by_version": {str(k): drop_removed_env(v) for k, v in c.ENV_VARS_BY_VERSION.items()},
}), indent=1) + "\n")

# Rich's terminal cell widths (the table it uses for "latest"), for
# Fisher.Rich.Cells: [start, end, width] ranges and the characters a
# variation selector widens.
from importlib.metadata import version as _pkg_version  # noqa: E402
from rich._unicode_data import load as _load_cells  # noqa: E402
_cells = _load_cells("latest")
(out / "cell-widths.json").write_text(json.dumps({
    "rich": _pkg_version("rich"),
    "unicode": _cells.unicode_version,
    "widths": [list(w) for w in _cells.widths],
    "narrow_to_wide": sorted(ord(ch) for ch in _cells.narrow_to_wide),
}) + "\n")

# The arXiv helper the startup context points managed sessions at.
wf = repo / "fisher-workflow" / "data"
wf.mkdir(parents=True, exist_ok=True)
(wf / "search_arxiv.py").write_bytes(fisher_text(
    (src / "skills" / "research" / "arxiv" / "scripts" / "search_arxiv.py").read_bytes().decode("utf-8")
).encode("utf-8"))

# The shell: built-in skins, banner art and the slash-command table.
cli = importlib.import_module(pkg)
b = importlib.import_module(pkg + ".banner")
cmds = importlib.import_module(pkg + ".commands")
se = importlib.import_module(pkg + ".skin_engine")
cli_data = repo / "fisher-cli" / "data"
cli_data.mkdir(parents=True, exist_ok=True)
(cli_data / "skins.json").write_text(json.dumps(
    fisher_data([skin for skin in se._BUILTIN_SKINS.values()]), indent=1, ensure_ascii=False) + "\n")
(cli_data / "banner.json").write_text(json.dumps({
    "version": cli.__version__,
    "release_date": cli.__release_date__,
    "logo": FISHER_AGENT_LOGO,
    "logo_ascii": FISHER_AGENT_LOGO_ASCII,
    "hero": getattr(b, OLD.upper() + "_CADUCEUS"),
    "hero_ascii": getattr(b, OLD.upper() + "_CADUCEUS_ASCII"),
    "compact": fisher_text(b.COMPACT_BANNER),
    "compact_ascii": fisher_text(b.COMPACT_BANNER_ASCII),
    "commands": fisher_data([[cat, [[k, v] for k, v in entries.items()]] for cat, entries in cmds.COMMANDS_BY_CATEGORY.items()]),
}, indent=1, ensure_ascii=False) + "\n")
print(f"wrote {out}, {wf} and {cli_data} from upstream {head}")
