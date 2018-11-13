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
  [Node "A" [Node "E" [], Node "F" []], Node "B" [Node "E" []], Node "X" [Node "X1" []]]

testtree' :: Tree String
testtree' = Node "C" []

testtree'' :: Tree String
testtree'' = Node "C" [Node "1" [Node "11" []], Node "2" []]


main :: IO ()
main = mapM_ (\t -> print "---" >> _traverse t) [testtree, testtree', testtree'']

-- traversal (showing each node) only with cursor-functions 'parent', 'next' and 'firstChild'
_traverse :: Show a => Tree a -> IO ()
_traverse = go . fromTree
 where
  go :: forall a . Show a => TreePos Full a -> IO ()
  go z = do
    debugNode z
    case firstChild z of
      Nothing -> case next z of
        Nothing -> case parent z of
          Nothing -> print "END"
          Just p  -> case next p of
            Nothing         -> print "END next"
            Just nxt_parent -> go nxt_parent
        Just nxt -> go nxt
      Just child -> go child

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
