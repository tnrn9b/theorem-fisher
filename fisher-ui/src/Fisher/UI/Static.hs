{-# LANGUAGE TemplateHaskell #-}

-- | The React bundle, embedded at compile time from @ui/dist@ (committed,
-- so a build needs no Node). Rebuild it with @npm ci && npm run build@ in
-- @ui/@; a test fails while any file under @ui/src@ is newer than
-- @ui/dist/index.html@.
module Fisher.UI.Static
  ( staticFiles
  ) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir, makeRelativeToProject)

-- | Each file of @ui/dist@ by its path inside it ("index.html",
-- "assets/index-abc123.js").
staticFiles :: [(FilePath, ByteString)]
staticFiles = $(makeRelativeToProject "../ui/dist" >>= embedDir)
