{-# LANGUAGE FlexibleContexts, RecordWildCards, LambdaCase, NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

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
import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (NameSpace(..))
import           System.IO

data Config = Config
  { unqualified :: Set String
  , ignore :: Set String
  , noqualified :: Bool
  }

data Data = Data
  { graph :: Map Name (Set Name)
  , groups :: [Set Name]
  }

defaultConfig :: Config
defaultConfig =
  Config
    { unqualified = Set.fromList ["base:GHC.Base", "ghc-prim:GHC.Types"]
    , ignore = Set.fromList ["integer-wired-in", "ghc-prim:GHC.Prim"]
    , noqualified = False
    }

-- TODO: Add writeD3
-- https://bl.ocks.org/heybignick/3faf257bbbbc7743bb72310d03b86ee8

writeGraphviz :: Name -> FilePath -> Config -> Q [Dec]
writeGraphviz name fp config = do
  Data {graph = edges, groups} <-
    execStateT (nameEdges name) Data {graph = mempty, groups = mempty}
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
                  (zipWith (printGroup config) [0 ..] groups <>
                   map (printNode config) (M.keys edges) <>
                   map (printEdge config) (M.toList edges))) <>
             "\n}")))
  pure []

printGroup :: Config -> Int -> Set Name -> SB.Builder
printGroup config@Config {ignore} index names =
  "subgraph cluster_" <> fromString (show index) <> "{\n" <>
  "peripheries=0\n" <>
  mconcat
    (List.intersperse
       "\n"
       (map
          (quoted . printName config)
          (filter (not . isIgnored ignore) (toList names)))) <>
  "\n}"

printNode :: Config -> Name -> SB.Builder
printNode config@Config {ignore} name =
  if not (isIgnored ignore name)
    then quoted (printName config name) <> " [shape=plain, fontcolor=" <> color <>
         "]"
    else ""
  where
    color =
      case nameSpace name of
        Just VarName -> quoted "#e300ff"
        Just DataName -> quoted "#005aff"
        _ -> quoted "#333333"

printEdge :: Config -> (Name,Set Name) -> SB.Builder
printEdge config@Config {ignore} (name, names) =
  mconcat
    (List.intersperse
       "\n"
       [ quoted (printName config name) <> " -> " <>
       quoted (printName config dep)
       | dep <- toList names
       , not (isIgnored ignore name)
       , not (isIgnored ignore dep)
       ])

quoted :: (Semigroup a, IsString a) => a -> a
quoted x = "\"" <> x <> "\""

isIgnored :: Set String -> Name -> Bool
isIgnored ignore name' =
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
    base = fromString (nameBase name) <> typ
      where typ = case nameSpace name of
                    Just VarName -> "  "
                    Just DataName -> " "
                    _ -> ""

nameEdges :: Name -> StateT Data Q ()
nameEdges name =
  when
    (isTypeName name)
    (do info <- lift (reify name)
        case info of
          TyConI dec -> do
            decEdges dec
          _ -> pure ())

decEdges :: Dec -> StateT Data Q ()
decEdges =
  \case
    DataD ctx name _tyvars _kind cons _deriv -> do
      seen <- gets graph
      unless
        (M.member name seen)
        (do let children = Set.fromList (listify isTypeName ctx)
            modifyGraph (M.insertWith (<>) name children)
            names <-
              for
                cons
                (\con -> do
                   conNames <- conEdges mempty con
                   modifyGraph (M.insertWith (<>) name (Set.fromList conNames))
                   pure conNames)
            modifyGroups (Set.fromList (mconcat names) :)
            traverse_ nameEdges (Set.toList children))
    NewtypeD ctx name _tyvars _kind cons _deriv -> do
      seen <- gets graph
      unless
        (M.member name seen)
        (do let children =
                  Set.fromList (listify isTypeName ctx) <>
                  Set.fromList (listify isTypeName cons)
            modifyGraph (M.insertWith (<>) name children)
            traverse_ nameEdges (Set.toList children))
    TySynD name _tyvars typ -> do
      seen <- gets graph
      unless
        (M.member name seen)
        (do let children = Set.fromList (listify isTypeName typ)
            modifyGraph (M.insertWith (<>) name children)
            traverse_ nameEdges (Set.toList children))
    _ -> pure ()

conEdges :: Set Name -> Con -> StateT Data Q [Name]
conEdges extras =
  \case
    NormalC name tys -> do
      let deps = extras <> Set.fromList (listify isTypeName tys)
      modifyGraph (M.insertWith (<>) name deps)
      traverse_ nameEdges (Set.toList deps)
      pure (pure name)
    RecC name tys -> do
      fieldnames <-
        for
          tys
          (\(fieldname, _, typ) -> do
             let deps = Set.fromList (listify isTypeName typ)
             modifyGraph (M.insertWith (<>) fieldname deps)
             traverse_ nameEdges (Set.toList deps)
             pure fieldname)
      modifyGraph (M.insertWith (<>) name (extras <> Set.fromList fieldnames))
      modifyGroups (Set.fromList fieldnames :)
      traverse_ nameEdges extras
      pure (pure name)
    InfixC _ name tys -> do
      let deps = extras <> Set.fromList (listify isTypeName tys)
      modifyGraph (M.insertWith (<>) name deps)
      traverse_ nameEdges (Set.toList deps)
      pure (pure name)
    ForallC tyvars cxt' con ->
      conEdges
        (Set.fromList (listify isTypeName tyvars) <>
         Set.fromList (listify isTypeName cxt'))
        con
    GadtC names tys ty -> do
      let deps =
            Set.fromList (listify isTypeName tys) <>
            Set.fromList (listify isTypeName ty)
      for_ names (\name -> modifyGraph (M.insertWith (<>) name deps))
      traverse_ nameEdges (Set.toList deps)
      pure names
    RecGadtC names tys ty -> do
      let deps =
            Set.fromList (listify isTypeName tys) <>
            Set.fromList (listify isTypeName ty)
      for_ names (\name -> modifyGraph (M.insertWith (<>) name deps))
      traverse_ nameEdges (Set.toList deps)
      pure names

isTypeName :: Name -> Bool
isTypeName name =
  nameSpace name == Just TcClsName

modifyGraph ::
     MonadState Data m => (Map Name (Set Name) -> Map Name (Set Name)) -> m ()
modifyGraph f = modify (\Data {..} -> Data {graph = f graph, ..})

modifyGroups ::
     MonadState Data m => ([Set Name] -> [Set Name]) -> m ()
modifyGroups f = modify (\Data {..} -> Data {groups = f groups, ..})
