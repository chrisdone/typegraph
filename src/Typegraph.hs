{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

module Typegraph
  ( writeGraphviz
  , Config(..)
  , defaultConfig
  ) where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Foldable
import           Data.Generics.Schemes
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (NameSpace(..))
import           System.IO

data Config = Config
  { unqualified :: Set String
  , ignore :: Set String
  , noqualified :: Bool
  }

defaultConfig :: Config
defaultConfig =
  Config
    { unqualified = Set.fromList ["base:GHC.Base", "ghc-prim:GHC.Types"]
    , ignore = Set.fromList ["integer-wired-in", "ghc-prim:GHC.Prim"]
    , noqualified = False
    }

writeGraphviz :: Name -> FilePath -> Config -> Q [Dec]
writeGraphviz name fp config = do
  edges <- execStateT (nameEdges name) mempty
  runIO
    (withFile
       fp
       WriteMode
       (\handle ->
          SB.hPutBuilder
            handle
            ("digraph G {\n" <>
             mconcat
               (List.intersperse
                  "\n"
                  (map (printEdge config) (M.toList edges))) <>
             "\n}")))
  pure []

printEdge :: Config -> (Name,Set Name) -> SB.Builder
printEdge config@Config {ignore} (name, names) =
  mconcat
    (List.intersperse
       "\n"
       [ quoted (printName config name) <> " -> " <>
       quoted (printName config dep)
       | dep <- toList names
       , not (isIgnored name)
       , not (isIgnored dep)
       ])
  where
    quoted x = "\"" <> x <> "\""
    isIgnored name' =
      Set.member
        (fromMaybe
           ""
           (do pkg <- namePackage name'
               mod' <- nameModule name'
               pure (pkg <> ":" <> mod')))
        ignore ||
      Set.member
        (fromMaybe
           ""
           (do pkg <- namePackage name'
               pure pkg))
        ignore

printName :: Config -> Name -> SB.Builder
printName Config {unqualified, noqualified} name =
  (if noqualified
     then ""
     else packageModule) <>
  base
  where
    packageModule =
      case do pkg <- namePackage name
              mod' <- nameModule name
              pure (pkg <> ":" <> mod') of
        Just prefix
          | Set.member prefix unqualified -> ""
        _ -> package <> module'
    package =
      case namePackage name of
        Nothing -> ""
        Just pkg
          | Set.member pkg unqualified -> ""
          | otherwise -> fromString pkg <> ":"
    module' =
      case nameModule name of
        Nothing -> ""
        Just pkg -> fromString pkg <> "."
    base = fromString (nameBase name)

nameEdges :: Name -> StateT (Map Name (Set Name)) Q ()
nameEdges name =
  when
    (isTypeName name)
    (do info <- lift (reify name)
        case info of
          TyConI dec -> do
            decEdges dec
          _ -> pure ())

decEdges :: Dec -> StateT (Map Name (Set Name)) Q ()
decEdges =
  \case
    DataD ctx name _tyvars _kind cons _deriv -> do
      seen <- get
      unless
        (M.member name seen)
        (do let children =
                  Set.fromList (listify isTypeName ctx) <>
                  Set.fromList (listify isTypeName cons)
            modify (M.insert name children)
            traverse_ nameEdges (Set.toList children))
    NewtypeD ctx name _tyvars _kind cons _deriv -> do
      seen <- get
      unless
        (M.member name seen)
        (do let children =
                  Set.fromList (listify isTypeName ctx) <>
                  Set.fromList (listify isTypeName cons)
            modify (M.insert name children)
            traverse_ nameEdges (Set.toList children))
    TySynD name _tyvars typ -> do
      seen <- get
      unless
        (M.member name seen)
        (do let children =
                  Set.fromList (listify isTypeName typ)
            modify (M.insert name children)
            traverse_ nameEdges (Set.toList children))
    _ -> pure ()

isTypeName :: Name -> Bool
isTypeName name = nameSpace name == Just TcClsName
