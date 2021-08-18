{-# LANGUAGE TemplateHaskell #-}

-- |

module Example where

import Language.Haskell.TH.Syntax
import Typegraph

$(writeGraphviz
    ''Name
    "name.dot"
    ["template-haskell:Language.Haskell.TH.Syntax", "base:GHC.Base"])
