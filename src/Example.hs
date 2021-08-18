{-# LANGUAGE TemplateHaskell #-}

-- |

module Example where

import qualified Data.Set as Set
import           Language.Haskell.TH.Syntax
import           Typegraph

data Rec = Rec { field1 :: Name, field2 :: Module}

$(writeGraphviz
    ''Rec
    "name.dot"
    defaultConfig
      { unqualified =
          unqualified defaultConfig <>
          Set.fromList
            [ "template-haskell:Language.Haskell.TH.Syntax"
            , "typegraph-0.1.0.0-7QKJRTg5g8J6VGpmlVBjkj"
            ], noqualified = True
      })
