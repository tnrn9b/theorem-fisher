-- | @fisher model@ against upstream's recorded pty runs
-- (@tools/corpus/b1_auth.py@, @model.json@), compared as
-- "Fisher.SetupSpec" compares setup's.
module Fisher.ModelSpec (spec) where

import Test.Hspec

import Fisher.Cli.Model (modelMain)
import Fisher.LLM.Sys
import Fisher.Py.Value
import Fisher.SetupSpec (goldenWith)

dispatch :: PyVal -> Sys -> IO Int
dispatch c sys = case maybe [] (\case PList xs -> [pyStrOf x | x <- xs]; _ -> []) (get "argv" c) of
  (_ : "model" : rest) -> modelMain sys rest
  argv -> fail ("no command for " <> show argv)

spec :: Spec
spec = goldenWith "model.json" dispatch
