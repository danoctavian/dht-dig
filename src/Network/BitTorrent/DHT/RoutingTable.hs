{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module Network.BitTorrent.DHT.RoutingTable where

import Data.ByteString
import Data.Functor
import Prelude as P hiding (lookup)
import Data.Bits
import Network.BitTorrent.DHT.Utils 
import Data.Traversable
import Data.Foldable as DF
import Control.Applicative
import Data.Monoid
import Data.Either.Unwrap
{- bit-wise trie
KEYS are expected to be all of the same length-}
data BitTrie a = Node {leftT :: BitTrie a, rightT :: BitTrie a}
               | Leaf {key :: Key, value :: a}
               | Empty
                deriving (Show, Eq)
type Key = ByteString

empty = Empty

insert k v bt
  = rezip $ leaf {focus = newNode}
    where
      newNode = case focus leaf of 
        Leaf oldK oldV -> if' (oldK == k)
                              (Leaf k v)
                              (mergeNodes (k,v) (oldK, oldV) $ depth leaf)
        Empty -> Leaf k v
      leaf = goDownWithKey k (zipper bt)

lookup k bt
  = case focus $ goDownWithKey k (zipper bt) of
    Leaf leafKey val -> if' (leafKey == k) (Just val) Nothing
    Empty -> Nothing

delete k bt
  = let leaf = goDownWithKey k (zipper bt) in case focus leaf of
    Leaf leafKey val -> if' (leafKey == k) (chop leaf) bt
    Empty -> bt 
        
-- closest n values to the given key
-- relying on lazy eval (take n) to go through as much as needed
closest k n bt = P.take n $ go (goDownWithKey k (zipper bt))
  where
    go z
      | isTop z = focusVal
      | otherwise = focusVal P.++ (DF.foldr (:) [] $ focus $ brother z) P.++  (go (goUp z))
        where
          focusVal = val . focus $ z
          val (Leaf k v) = [v]
          val _ = []

size bt = DF.foldr (\e s -> s + 1) 0 bt

-- removes the focus and all nodes above that don't have other children
chop z 
  | childlessFocus && (not $ isTop z) = chop $ goUp $ z {focus = Empty}
  | childlessFocus && isTop z = Empty
  | otherwise = rezip z 
  where
    childlessFocus = childless . focus $ z
    childless (Node l r) = [l, r] == [Empty, Empty]
    childless _ = True

mergeNodes n1@(k1, v1) n2@(k2, v2) depth
  | testKeyBit k1 depth /= testKeyBit k2 depth = childFlip (Leaf k1 v1) (Leaf k2 v2)
  | otherwise = childFlip (mergeNodes n1 n2 (depth + 1))  Empty
    where
      childFlip = if' (testKeyBit k1 depth) P.flip P.id $ Node

goDownWithKey :: Key -> BTZipper a -> BTZipper a
goDownWithKey k zipper = case focus zipper of
    Empty -> zipper
    Leaf {..} -> zipper
    Node left right ->
      (goDownWithKey k $ goDown (if' (testKeyBit k (depth zipper)) R L) $ zipper)

-- TODO; need to double check that the bit order here is right..
testKeyBit :: Key -> Int -> Bool
testKeyBit k n = let sz = 8 in testBit (index k (n `div` sz)) (sz - n `mod` sz - 1)

-- instances
instance Foldable BitTrie where
   foldMap f Empty = mempty
   foldMap f (Leaf k v) = f v
   foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

-- ZIPPER
-- for the tree above; custom zipper since using lens is such
-- a pain in the ass and it's just ~20 lines anyway
data BTZipper a = BTZipper {focus :: BitTrie a, trail :: [Crumb a], depth :: Int}
data Crumb a = LeftCrumb {crumbT :: (BitTrie a)} | RightCrumb {crumbT :: (BitTrie a)}
  deriving (Show, Eq)
data Dir = L | R deriving (Show, Eq)

zipper :: BitTrie a -> BTZipper a
zipper t = BTZipper t [] 0
-- Note that all functions DO NOT handle failure 
goDown dir z@(BTZipper (Node left right) trail depth) = let t = dir == R in
  BTZipper (if' t right left)
            ((if' t (RightCrumb left) (LeftCrumb right)) : trail)
            (depth + 1)
goUp z@(BTZipper f (c : cs) d) =
  BTZipper ((if' (isLeftChild z) P.id P.flip Node) f (crumbT c)) cs (d - 1)
-- WARNING: sexist code ahead
brother z = (goDown $ if' (isLeftChild z) R L) . goUp $ z
rezip = focus . rz where rz z = if' (isTop z) z (rz $ goUp z)

isTop = (== []) . trail 
isLeftChild zipper = case P.head $ trail zipper of
  LeftCrumb {..} -> True
  RightCrumb {..} -> False  

testedBit = P.map (testKeyBit "\1\2") [0..15]

testTree = insert "\255\0" 4 $ delete "\0\0" $ delete "\2\0" $ insert "\2\0" 3 $ insert "\0\0" 2 $ insert "\1\0" 1 Empty


inf = Node inf inf