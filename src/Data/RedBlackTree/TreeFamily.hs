{-# LANGUAGE
    NamedFieldPuns
  #-}

module Data.RedBlackTree.TreeFamily (
  getTreeFamily,
  TreeFamily (..)
) where

import Data.RedBlackTree.BinaryTree
  ( TreeBranch, TreeDirection, TreeDirections, BranchZipper
  , reconstructAncestor)


-- | This type is used to answer questions about the current size of the tree and
-- how deep can we go up in the tree. This is only used internally for the insert
-- algorithm. Does it only have one element (IsRoot),
-- Does it have less than 3 elements? HasParent or is it way bigger? (HasGrandparent)
data TreeFamily a
  = -- | The 'IsRoot' constructor has only one parameter: the only existing node as a 'TreeBranch'.
    IsRoot
    { rootBranch :: TreeBranch a
    }
  | -- | The 'HasParent' constructor has two parameters, a 'TreeBranch' and a
    -- 'TreeDirection' that can be used to get a reference to the parent.
    HasParent
    { parentDirection :: TreeDirection a
    , parentBranch :: TreeBranch a
    }
  | -- | The 'HasGrandparent' constructor has 4 parameters, the 4th parameter is a
    -- 'TreeBranch'. 3rd parameter is a 'TreeDirection' to reconstruct the parent.
    -- 2nd paramenter is a 'TreeDirection' to reconstruct the grandparent. the first
    -- argument is a list of 'TreeDirection' to reconstruct the rest of the ancestors.
    HasGrandparent
    { directions :: TreeDirections a
    , grandparentDirection :: TreeDirection a
    , parentDirection :: TreeDirection a
    , grandparentBranch :: TreeBranch a
    }

getTreeFamily' :: BranchZipper a -> TreeDirection a -> TreeBranch a -> TreeFamily a
getTreeFamily' (_, []) parentDirection parentBranch =
  HasParent{parentDirection,parentBranch}
getTreeFamily' (_, grandparentDirection:directions) parentDirection grandparentBranch =
  HasGrandparent
  { directions
  , grandparentDirection
  , parentDirection
  , grandparentBranch
  }

getTreeFamily :: BranchZipper a -> TreeFamily a
getTreeFamily (rootBranch, []) = IsRoot{rootBranch}
getTreeFamily (branch, direction:xs) =
  getTreeFamily' parentZipper direction branch
  where
    parentBranch = reconstructAncestor branch direction
    parentZipper = (parentBranch, xs)
