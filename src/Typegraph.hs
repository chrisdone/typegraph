{-# LANGUAGE LambdaCase, NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

module Typegraph (writeGraphviz) where

import           Control.Monad.State.Strict
import qualified Data.ByteString.Lazy.Builder as SB
import           Data.Foldable
import           Data.Generics.Schemes
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (NameSpace(..))
import           System.IO

writeGraphviz :: Name -> FilePath -> [String] -> Q [Dec]
writeGraphviz name fp (Set.fromList -> unqualified) = do
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
                  (map (printEdge unqualified) (M.toList edges))) <>
             "\n}")))
  pure []

printEdge :: Set String -> (Name,Set Name) -> SB.Builder
printEdge unqualified (name, names) =
  mconcat
    (List.intersperse
       "\n"
       [ quoted (printName unqualified name) <> " -> " <>
       quoted (printName unqualified dep)
       | dep <- toList names
       ])
  where
    quoted x = "\"" <> x <> "\""

printName :: Set String -> Name -> SB.Builder
printName unqualified name = packageModule <> base
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
    _ -> pure ()

isTypeName :: Name -> Bool
isTypeName name = nameSpace name == Just TcClsName
