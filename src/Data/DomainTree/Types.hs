{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DomainTree.Types where

import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (partition,sortBy)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Word (Word8)

class (Eq s, Eq (Elem s)) => Sequence s where
  type Elem s
  split :: Elem s -> s -> [s]

instance Eq a => Sequence [a] where
  type Elem [a] = a
  split e v | null v = []
            | otherwise = cons (case break (== e) v of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> split e s''))
      where
        cons ~(h, t) = h:t

instance Sequence ByteString.ByteString where
  type Elem ByteString.ByteString = Word8
  split = ByteString.split

instance Sequence LazyByteString.ByteString where
  type Elem LazyByteString.ByteString = Word8
  split = LazyByteString.split

instance Sequence Text.Text where
  type Elem Text.Text = Char
  split e = Text.split (== e)

instance Sequence LazyText.Text where
  type Elem LazyText.Text = Char
  split e = LazyText.split (== e)

data DomainTree k a = DomainTree { sepElem :: Elem k
                                 , wildCardChunk :: Maybe k
                                 , domainTrees :: [Tree k a]
                                 } deriving (Functor, Foldable, Traversable)

deriving instance (Show (Elem k), Show k, Show a) => Show (DomainTree k a)

data Tree k a = Tree { treeKey :: !(Maybe k)
                     , treeVal :: !(Maybe a)
                     , isWildcard :: Bool
                     , treeChildren :: [Tree k a]
                     } deriving (Functor, Foldable, Traversable, Show)

empty :: Sequence k => Elem k -> Maybe k -> DomainTree k a
empty s wcc = DomainTree s wcc []

insertChunks :: Eq k => [k] -> a -> DomainTree k a -> DomainTree k a
insertChunks ks v (DomainTree sep wcc ts) = DomainTree sep wcc $ go ks ts
  where
    go [] ts = [Tree Nothing (pure v) False ts]
    go (c:cs) ts = let (m,o) = partition (\n -> if pure c == wcc then isWildcard n else pure c == treeKey n) ts
                   in case m of
                        [Tree k' v' wc children] | null cs -> Tree k' (pure v) wc children:o
                                                 | otherwise -> Tree k' v' wc (go cs children):o
                        [] | null cs -> Tree (pure c) (pure v) (pure c == wcc) []:o
                           | otherwise -> Tree (pure c) Nothing (pure c == wcc) (go cs []):o
                        _ -> error "multiple nodes with the same key found"
{-# INLINE insertChunks #-}

insert :: Sequence k => k -> a -> DomainTree k a -> DomainTree k a
insert k v t@(DomainTree sep _ _) = insertChunks (split sep k) v t
{-# INLINE insert #-}

fromList :: Sequence k => Elem k -> Maybe k -> [(k,a)] -> DomainTree k a
fromList sep wcc = foldr (\(k,v) -> insert k v) e
  where e = empty sep wcc
{-# INLINE fromList #-}

lookup :: Sequence k => k -> DomainTree k a -> Maybe a
lookup key (DomainTree sep wcc trees) = go kseq trees
  where
    kseq = split sep key
    go [] [Tree Nothing v _ _] = v
    go [] _ = Nothing
    go (c:cs) ts = case filter (\t -> isWildcard t || treeKey t == pure c) ts of
                     [] -> Nothing
                     [Tree k v wc ts'] | null cs -> v
                                       | otherwise -> go cs ts'
                     rs -> let n = head $ sortBy (compare `on` isWildcard) rs
                               r | null cs = treeVal n
                                 | otherwise = go cs $ treeChildren n
                           in r
{-# INLINE lookup #-}
