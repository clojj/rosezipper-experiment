{-# LANGUAGE RankNTypes #-}
{-# OPTIONS -Wall #-}
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
testtree2 = Node "C" [Node "1" [Node "11" []], Node "2" []]

testtree3 :: Tree String
testtree3 = Node "0" [Node "1" [Node "11" [Node "111" []]], Node "2" [Node "22" [Node "222" []]]]


main :: IO ()
main =
  mapM_ (\t -> print "------------------------------------" >> _traverse t) [testtree, testtree1, testtree2, testtree3]

data Navigation = Down | Next | Up

-- traversal (showing each node) only with cursor-functions 'parent', 'next' and 'firstChild'
_traverse :: Show a => Tree a -> IO ()
_traverse = go Down . fromTree
  where
    go :: forall a . Show a => Navigation -> TreePos Full a -> IO ()
    go nav z = do
      case nav of 
        Down -> case firstChild z of
                  Nothing   -> go Next z
                  Just node -> debugNode node >> go Down node
        Next -> case next z of
                  Nothing   -> go Up z
                  Just node -> debugNode node >> go Down node
        Up ->   case parent z of
                  Nothing   -> debugNode z
                  Just node -> go Next node

debugNode :: Show a => TreePos Full a -> IO ()
debugNode = print . label


-- other stuff

childrenAsList :: TreePos Full a -> [TreePos Full a]
childrenAsList = go . children
 where
  go :: forall a . TreePos Empty a -> [TreePos Full a]
  go z = case nextTree z of
    Nothing -> []
    Just t  -> t : go (nextSpace t)

minZipper :: Ord a => Tree a -> TreePos Full a
minZipper = go . fromTree
 where
  go :: forall a . Ord a => TreePos Full a -> TreePos Full a
  go z =
    minimumBy (comparing (rootLabel . tree)) (z : map go (childrenAsList z))
