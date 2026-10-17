"""Fisher's launcher for Isabelle-MCP.

Run as `uvx --from isabelle-mcp==0.6.0 python <this file> ARGS...`; it runs
the server's entry point (isabelle_mcp.server:main) with ARGS.

Isabelle-MCP 0.6.0's FileWatcher.start logs a count (`avail`) that it reads
only on Linux, after the watcher has started, so everywhere else it raises
UnboundLocalError and the server stops at startup. That one error is
swallowed here, since the watcher is already running; any other error is
raised as before. On Linux, and on a version without the error, nothing
changes.
"""

import sys

try:
    from isabelle_mcp import file_watcher as _fw
except ImportError:
    _fw = None

_Watcher = getattr(_fw, "FileWatcher", None)
_start = getattr(_Watcher, "start", None)

if _start is not None:

    def _fisher_start(self):
        try:
            _start(self)
        except UnboundLocalError as e:
            if "avail" not in str(e) or not getattr(self, "_inotify_enabled", False):
                raise
            _fw.logger.info("Filesystem watcher started")

    _Watcher.start = _fisher_start


def main():
    from isabelle_mcp.server import main as server_main

    sys.argv[0] = "isabelle-mcp"
    server_main()


if __name__ == "__main__":
    main()
