{-# LANGUAGE RankNTypes, OverloadedStrings #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -Weverything -Wsafe #-}
module Main
  ( testtree
  , main
  )
where

import Data.List hiding (insert, delete) 
import           Data.Ord                       ( comparing )
import           Data.Tree                      ( Tree(Node)
                                                , rootLabel
                                                , flatten
                                                , drawTree
                                                )
import           Data.Tree.Zipper               

import           Control.Monad.Writer

import qualified Test.QuickCheck               as QC
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Instances.Containers


testtree :: Tree String
testtree = Node
  "Top"
  [ Node "A" [Node "AE" [], Node "AF" []]
  , Node "B" [Node "BE" []]
  , Node "X" [Node "X1" []]
  ]

testtree1 :: Tree String
testtree1 = Node "C" []

testtree2 :: Tree String
testtree2 =
  Node "C" [Node "1" [Node "11" []], Node "2" [], Node "3" [Node "33" []]]

testtree3 :: Tree String
testtree3 = Node
  "0"
  [Node "1" [Node "11" [Node "111" []]], Node "2" [Node "22" [Node "222" []]]]



data Navigation = Down | Next | Up

-- traversal in Monad m for each node ...only with cursor-functions 'parent', 'next' and 'firstChild'
_traverseM :: (Show a, Monad m) => (TreePos Full a -> m b) -> Tree a -> m b
_traverseM f = go f Down . fromTree
 where
  go
    :: (Show a, Monad m)
    => (TreePos Full a -> m b)
    -> Navigation
    -> TreePos Full a
    -> m b
  go fn nav z = case nav of
    Down -> case firstChild z of
      Nothing   -> go fn Next z
      Just node -> fn node >> go fn Down node
    Next -> case next z of
      Nothing   -> go fn Up z
      Just node -> fn node >> go fn Down node
    Up -> case parent z of
      Nothing   -> fn z
      Just node -> go fn Next node

-- TODO with type variable
_traverseBuild :: Tree String -> Tree String
_traverseBuild = toTree . (go (fromTree (Node "NEW TREE" [])) Down) . fromTree
 where
  go
    :: TreePos Full String
    -> Navigation
    -> TreePos Full String
    -> TreePos Full String
  go n nav z = case nav of
    Down -> case firstChild z of
      Nothing   -> go n Next z
      Just node -> go (insert (Node (label node) []) (children n)) Down node
    Next -> case next z of
      Nothing   -> go n Up z
      Just node -> go (insert (Node (label node) []) (children n)) Down node
    Up -> case parent z of
      Nothing   -> n
      Just node -> go n Next node


collectNode :: TreePos Full a -> Writer [a] ()
collectNode z = let str = label z in tell [str]

debugNode :: Show a => TreePos Full a -> IO ()
debugNode = print . label


-- tests

-- TODO compare tree from _traverseBuild

_prop_traverses_all :: (Eq a, Show a) => Tree a -> QC.Property
_prop_traverses_all tree =
  QC.label ("tree size " ++ show (length $ flatten tree))
    $ compList (flatten tree) (execWriter $ _traverseM collectNode tree)

compList :: Eq a => [a] -> [a] -> Bool
compList x y = unique x && unique y && null (x \\ y)

unique :: Eq a => [a] -> Bool
unique ls = length ls == length (nub ls)


-- main

-- QC.quickCheck $ QC.mapSize ((*) 1000) $ QC.withMaxSuccess 50 _prop_traverses_all

main :: IO ()
main =
  let alltrees = [testtree, testtree1, testtree2, testtree3]
  in
  -- IO Monad
    mapM_
        (\t ->
          print "------------------------------------" >> _traverseM debugNode t
        )
        alltrees
      >>

  -- Writer Monad
         let result = map (execWriter . _traverseM collectNode)
                          [testtree, testtree1, testtree2, testtree3]
                  in  print "-------------------------Writer Monad--------------"
                      >> mapM_ print result
                      >> let alltreesFlattened = map flatten alltrees
                             zipped            = zip alltreesFlattened result
                         in  print $ map (\(t, r) -> compList t r) zipped


-- other stuff

-- childrenAsList :: TreePos Full a -> [TreePos Full a]
-- childrenAsList = go . children
--  where
--   go :: forall a . TreePos Empty a -> [TreePos Full a]
--   go z = case nextTree z of
--     Nothing -> []
--     Just t  -> t : go (nextSpace t)

-- minZipper :: Ord a => Tree a -> TreePos Full a
-- minZipper = go . fromTree
--  where
--   go :: forall a . Ord a => TreePos Full a -> TreePos Full a
--   go z =
--     minimumBy (comparing (rootLabel . tree)) (z : map go (childrenAsList z))
