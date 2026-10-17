-- | Shared mutable state is touched from concurrent tool calls (the MCP
-- server runs them in parallel, as the Python SDK does on one event loop),
-- so every update is atomic.
module Fisher.Lean.Ref (modifyRef) where

import Data.IORef (IORef, atomicModifyIORef')

modifyRef :: IORef a -> (a -> a) -> IO ()
modifyRef r f = atomicModifyIORef' r (\x -> (f x, ()))
