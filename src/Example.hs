{-# LANGUAGE TemplateHaskell #-}

-- |

module Example where

import qualified Data.Set as Set
import           Language.Haskell.TH.Syntax
import           Typegraph

$(writeGraphviz
    ''Name
    "name.dot"
    defaultConfig
      { unqualified =
          unqualified defaultConfig <>
          Set.fromList ["template-haskell:Language.Haskell.TH.Syntax"]
      })
