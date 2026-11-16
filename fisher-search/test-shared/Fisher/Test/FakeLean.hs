-- | A Lean stand-in for the tests of fisher-search and fisher-cli: this
-- directory is a test source directory of both packages.
module Fisher.Test.FakeLean (fakeLean, fakeLeanBy, fakeLeanWith) where

import qualified Data.ByteString as BS
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))

import Fisher.Search.Check (LeanRun (..))
import Fisher.Search.Lean

-- | A Lean stand-in: a proof containing BAD does not compile, an open
-- goal warns, a proof that names an open goal (directly or through other
-- declarations) depends on sorryAx, every name's type is its statement.
fakeLean :: LeanRun
fakeLean = fakeLeanOn statementHead (const False)

-- | 'fakeLean' with the printed type of each declaration given by a function.
fakeLeanBy :: (Decl -> Text) -> LeanRun
fakeLeanBy printed = fakeLeanOn printed (const False)

-- | 'fakeLean' where a closed declaration the predicate names does not
-- compile either.
fakeLeanWith :: (Decl -> Bool) -> LeanRun
fakeLeanWith = fakeLeanOn statementHead

-- | The stand-in given each declaration's printed type and which closed
-- declarations it refuses.
fakeLeanOn :: (Decl -> Text) -> (Decl -> Bool) -> LeanRun
fakeLeanOn printed refused = LeanRun $ \root rel -> do
  t <- TE.decodeUtf8 <$> BS.readFile (root </> rel)
  let body = fst (T.breakOn "\n\n#print axioms" t)
      ds = declarations body
      opens = Set.fromList (map dName (filter isOpen ds))
      queried = [T.strip (T.drop (T.length "#print axioms ") l) | l <- T.lines t, "#print axioms " `T.isPrefixOf` l]
      errs = [rel <> ":" <> show (dStart d + 1) <> ":2: error: BAD" | d <- ds, "BAD" `T.isInfixOf` dProof d || (refused d && not (isOpen d))]
      warns = [rel <> ":" <> show (dStart d + 1) <> ":8: warning: declaration uses `sorry`" | d <- ds, isOpen d]
      uses dirty d = any (\o -> o /= dName d && o `Set.member` dirty) (T.words (dProof d))
      grow dirty =
        let next = Set.union dirty (Set.fromList [dName d | d <- ds, uses dirty d])
        in if next == dirty then dirty else grow next
      sorryDeps = grow opens
      info n = case findDecl n ds of
        Nothing -> []
        Just d ->
          [ "'" <> n <> "' depends on axioms: [" <> (if dName d `Set.member` sorryDeps then "sorryAx" else "propext") <> "]"
          , n <> " : " <> printed d ]
  pure (if null errs then 0 else 1, T.unlines (map T.pack (errs <> warns) <> concatMap info queried))
