{-# LANGUAGE ForeignFunctionInterface #-}
-- | Terminal size and raw mode, through a small C shim.
module Fisher.Swarm.Pty
  ( getWinSize
  , setWinSize
  , withRawMode
  ) where

import Control.Exception (bracket)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.Posix.Types (Fd (..))

foreign import ccall unsafe "fisher_get_winsize" c_get :: CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "fisher_set_winsize" c_set :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "fisher_make_raw" c_raw :: CInt -> IO (Ptr ())
foreign import ccall unsafe "fisher_restore" c_restore :: CInt -> Ptr () -> IO ()

-- | (rows, columns), when the descriptor is a terminal.
getWinSize :: Fd -> IO (Maybe (Int, Int))
getWinSize (Fd fd) = alloca $ \r -> alloca $ \c -> do
  rc <- c_get fd r c
  if rc /= 0 then pure Nothing else do
    rows <- peek r
    cols <- peek c
    pure (Just (fromIntegral rows, fromIntegral cols))

setWinSize :: Fd -> Int -> Int -> IO ()
setWinSize (Fd fd) rows cols = () <$ c_set fd (fromIntegral rows) (fromIntegral cols)

-- | Run with a terminal in raw mode, restoring it afterwards. On a
-- descriptor that is not a terminal the action just runs.
withRawMode :: Fd -> IO a -> IO a
withRawMode (Fd fd) act = bracket (c_raw fd) (\saved -> if saved == nullPtr then pure () else c_restore fd saved) (const act)
