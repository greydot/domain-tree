{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Data.DomainTree.Types where

import Data.List (partition)

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

data DomainTree k a = DomainTree { sepElem :: Elem k
                                 , domainTrees :: [Tree k a]
                                 } deriving (Functor, Foldable, Traversable)

deriving instance (Show (Elem k), Show k, Show a) => Show (DomainTree k a)

data Tree k a = Tree { treeKey :: !k
                     , treeVal :: !(Maybe a)
                     , treeChildren :: [Tree k a]
                     } deriving (Functor, Foldable, Traversable, Show)

empty :: Sequence k => Elem k -> DomainTree k a
empty s = DomainTree s []

insert :: Sequence k => k -> a -> DomainTree k a -> DomainTree k a
insert k v (DomainTree sep ts) = DomainTree sep $ go kseq ts
  where
    kseq = split sep k
    go [] _ = error "empty key sequence"
    go (c:cs) ts = let (m,o) = partition (\n -> treeKey n == c) ts
                   in case m of
                        [Tree k' v' children] | null cs -> Tree k' (pure v) children:o
                                              | otherwise -> Tree k' v' (go cs children):o
                        [] | null cs -> Tree c (pure v) []:o
                           | otherwise -> Tree c Nothing (go cs []):o
                        _ -> error "multiple nodes with the same key found"

fromList :: Sequence k => Elem k -> [(k,a)] -> DomainTree k a
fromList sep = foldr (\(k,v) -> insert k v) e
  where e = empty sep

lookup :: Sequence k => k -> DomainTree k a -> Maybe a
lookup key (DomainTree sep trees) = go kseq trees
  where
    kseq = split sep key
    go [] _ = Nothing
    go (c:cs) ts = case filter (\t -> treeKey t == c) ts of
                     [] -> Nothing
                     [Tree k v ts'] | null cs -> v
                                    | otherwise -> go cs ts'
                     _ -> error "multiple nodes with the same key found"
