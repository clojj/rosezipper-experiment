{-# LANGUAGE RankNTypes, OverloadedStrings #-}
-- {-# OPTIONS -Wall #-}
-- {-# OPTIONS -Weverything -Wsafe #-}
module Main
  ( testtree
  , main
  )
where

import           Data.List                      ( minimumBy )
import           Data.Ord                       ( comparing )
import           Data.Tree                      ( Tree(Node)
                                                , rootLabel
                                                , flatten
                                                )
import           Data.Tree.Zipper               ( TreePos
                                                , Full
                                                , Empty
                                                , nextTree
                                                , nextSpace
                                                , fromTree
                                                , tree
                                                , children
                                                , label
                                                , firstChild
                                                , next
                                                , parent
                                                )


import Control.Monad.Writer

import Data.List

testtree :: Tree String
testtree = Node
  "C"
  [ Node "A" [Node "E" [], Node "F" []]
  , Node "B" [Node "E" []]
  , Node "X" [Node "X1" []]
  ]

testtree1 :: Tree String
testtree1 = Node "C" []

testtree2 :: Tree String
testtree2 = Node "C" [Node "1" [Node "11" []], Node "2" [], Node "3" [Node "33" []]]

testtree3 :: Tree String
testtree3 = Node "0" [Node "1" [Node "11" [Node "111" []]], Node "2" [Node "22" [Node "222" []]]]



data Navigation = Down | Next | Up

-- traversal in Monad m for each node ...only with cursor-functions 'parent', 'next' and 'firstChild'
_traverseM :: (Show a, Monad m) => (TreePos Full a -> m b) -> Tree a -> m b
_traverseM f = go f Down . fromTree
  where
    go :: (Show a, Monad m) => (TreePos Full a -> m b) -> Navigation -> TreePos Full a -> m b
    go fn nav z = 
      case nav of 
        Down -> case firstChild z of
                  Nothing   -> go fn Next z
                  Just node -> fn node >> go fn Down node
        Next -> case next z of
                  Nothing   -> go fn Up z
                  Just node -> fn node >> go fn Down node
        Up ->   case parent z of
                  Nothing   -> fn z
                  Just node -> go fn Next node

collectNode :: TreePos Full a -> Writer [a] ()
collectNode z = 
  let str = label z
    in
    tell [str]

debugNode :: Show a => TreePos Full a -> IO ()
debugNode = print . label


main :: IO ()
main = 
  let alltrees = [testtree, testtree1, testtree2, testtree3]
  in
  -- IO Monad
  mapM_ (\t -> print "------------------------------------" >> _traverseM debugNode t) alltrees >>

  -- Writer Monad
  let result = map (\t -> execWriter $ _traverseM collectNode t) [testtree, testtree1, testtree2, testtree3]
  in
    print "-------------------------Writer Monad--------------" >> 
    mapM_ print result 
    -- TODO hspec, QuickCheck !
    >>
    let alltreesFlattened = map (\t -> flatten t) alltrees
        zipped = zip alltreesFlattened result
    in
      print $ map (\(t, r) -> compList t r) zipped


compList :: (Eq a) => [a] -> [a] -> Bool
compList x y = null (x \\ y) && null (y \\ x)


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
