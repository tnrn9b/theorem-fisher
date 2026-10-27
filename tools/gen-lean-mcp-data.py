#!/usr/bin/env python3
"""Regenerate fisher-lean/src/Fisher/Lean/McpData.hs from fisher-lean/data/lean-lsp-mcp-0.31.0.json.

The JSON is what lean-lsp-mcp 0.31.0 answers to initialize and tools/list
(recorded with tools/corpus/c1-mcp-dump.py), cut to the C1 tools. The
Haskell module holds it as one string literal, UTF-8 in the source.
"""
import json, pathlib
root = pathlib.Path(__file__).resolve().parent.parent
data = json.loads((root / "fisher-lean/data/lean-lsp-mcp-0.31.0.json").read_text(encoding="utf-8"))
lit = json.dumps(json.dumps(data, ensure_ascii=False), ensure_ascii=False)
assert "\\u" not in lit, "a \\u escape is not Haskell; extend the escaping"
(root / "fisher-lean/src/Fisher/Lean/McpData.hs").write_text(
    "-- | Generated from data/lean-lsp-mcp-0.31.0.json (tools/gen-lean-mcp-data.py): what\n"
    "-- lean-lsp-mcp 0.31.0 says in initialize and tools/list.\n"
    "module Fisher.Lean.McpData (mcpDataJson) where\n\nimport Data.Text (Text)\n\n"
    "mcpDataJson :: Text\nmcpDataJson = " + lit + "\n", encoding="utf-8")
