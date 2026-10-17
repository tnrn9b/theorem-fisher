-- | The four file tools' schemas, exactly as upstream registers them
-- (generated from test/golden/b2/registry.json).
module Fisher.Tools.File.Schemas
  ( readFileSchema
  , writeFileSchema
  , patchSchema
  , searchFilesSchema
  ) where

import Fisher.Py.Value (PyVal (..))

readFileSchema :: PyVal
readFileSchema =
  PDict
    [ ("name", PStr "read_file")
    , ("description", PStr "Read a text file with line numbers and pagination. Use this instead of cat/head/tail in terminal. Output format: 'LINE_NUM|CONTENT'. Suggests similar filenames if not found. Use offset and limit for large files. NOTE: Cannot read images or binary files \x2014 use vision_analyze for images.")
    , ("parameters", PDict
      [ ("type", PStr "object")
      , ("properties", PDict
        [ ("path", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Path to the file to read (absolute, relative, or ~/path)")
          ])
        , ("offset", PDict
          [ ("type", PStr "integer")
          , ("description", PStr "Line number to start reading from (1-indexed, default: 1)")
          , ("default", PInt 1)
          , ("minimum", PInt 1)
          ])
        , ("limit", PDict
          [ ("type", PStr "integer")
          , ("description", PStr "Maximum number of lines to read (default: 500, max: 2000)")
          , ("default", PInt 500)
          , ("maximum", PInt 2000)
          ])
        ])
      , ("required", PList [PStr "path"])
      ])
    ]

writeFileSchema :: PyVal
writeFileSchema =
  PDict
    [ ("name", PStr "write_file")
    , ("description", PStr "Write content to a file, completely replacing existing content. Use this instead of echo/cat heredoc in terminal. Creates parent directories automatically. OVERWRITES the entire file \x2014 use 'patch' for targeted edits.")
    , ("parameters", PDict
      [ ("type", PStr "object")
      , ("properties", PDict
        [ ("path", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Path to the file to write (will be created if it doesn't exist, overwritten if it does)")
          ])
        , ("content", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Complete content to write to the file")
          ])
        ])
      , ("required", PList [PStr "path", PStr "content"])
      ])
    ]

patchSchema :: PyVal
patchSchema =
  PDict
    [ ("name", PStr "patch")
    , ("description", PStr "Targeted find-and-replace edits in files. Use this instead of sed/awk in terminal. Uses fuzzy matching (9 strategies) so minor whitespace/indentation differences won't break it. Returns a unified diff. Auto-runs syntax checks after editing.\n\nReplace mode (default): find a unique string and replace it.\nPatch mode: apply V4A multi-file patches for bulk changes.")
    , ("parameters", PDict
      [ ("type", PStr "object")
      , ("properties", PDict
        [ ("mode", PDict
          [ ("type", PStr "string")
          , ("enum", PList [PStr "replace", PStr "patch"])
          , ("description", PStr "Edit mode: 'replace' for targeted find-and-replace, 'patch' for V4A multi-file patches")
          , ("default", PStr "replace")
          ])
        , ("path", PDict
          [ ("type", PStr "string")
          , ("description", PStr "File path to edit (required for 'replace' mode)")
          ])
        , ("old_string", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Text to find in the file (required for 'replace' mode). Must be unique in the file unless replace_all=true. Include enough surrounding context to ensure uniqueness.")
          ])
        , ("new_string", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Replacement text (required for 'replace' mode). Can be empty string to delete the matched text.")
          ])
        , ("replace_all", PDict
          [ ("type", PStr "boolean")
          , ("description", PStr "Replace all occurrences instead of requiring a unique match (default: false)")
          , ("default", PBool False)
          ])
        , ("patch", PDict
          [ ("type", PStr "string")
          , ("description", PStr "V4A format patch content (required for 'patch' mode). Format:\n*** Begin Patch\n*** Update File: path/to/file\n@@ context hint @@\n context line\n-removed line\n+added line\n*** End Patch")
          ])
        ])
      , ("required", PList [PStr "mode"])
      ])
    ]

searchFilesSchema :: PyVal
searchFilesSchema =
  PDict
    [ ("name", PStr "search_files")
    , ("description", PStr "Search file contents or find files by name. Use this instead of grep/rg/find/ls in terminal. Ripgrep-backed, faster than shell equivalents.\n\nContent search (target='content'): Regex search inside files. Output modes: full matches with line numbers, file paths only, or match counts.\n\nFile search (target='files'): Find files by glob pattern (e.g., '*.py', '*config*'). Also use this instead of ls \x2014 results sorted by modification time.")
    , ("parameters", PDict
      [ ("type", PStr "object")
      , ("properties", PDict
        [ ("pattern", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Regex pattern for content search, or glob pattern (e.g., '*.py') for file search")
          ])
        , ("target", PDict
          [ ("type", PStr "string")
          , ("enum", PList [PStr "content", PStr "files"])
          , ("description", PStr "'content' searches inside file contents, 'files' searches for files by name")
          , ("default", PStr "content")
          ])
        , ("path", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Directory or file to search in (default: current working directory)")
          , ("default", PStr ".")
          ])
        , ("file_glob", PDict
          [ ("type", PStr "string")
          , ("description", PStr "Filter files by pattern in grep mode (e.g., '*.py' to only search Python files)")
          ])
        , ("limit", PDict
          [ ("type", PStr "integer")
          , ("description", PStr "Maximum number of results to return (default: 50)")
          , ("default", PInt 50)
          ])
        , ("offset", PDict
          [ ("type", PStr "integer")
          , ("description", PStr "Skip first N results for pagination (default: 0)")
          , ("default", PInt 0)
          ])
        , ("output_mode", PDict
          [ ("type", PStr "string")
          , ("enum", PList [PStr "content", PStr "files_only", PStr "count"])
          , ("description", PStr "Output format for grep mode: 'content' shows matching lines with line numbers, 'files_only' lists file paths, 'count' shows match counts per file")
          , ("default", PStr "content")
          ])
        , ("context", PDict
          [ ("type", PStr "integer")
          , ("description", PStr "Number of context lines before and after each match (grep mode only)")
          , ("default", PInt 0)
          ])
        ])
      , ("required", PList [PStr "pattern"])
      ])
    ]
