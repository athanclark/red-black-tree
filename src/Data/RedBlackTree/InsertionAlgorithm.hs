{-# LANGUAGE
    RecordWildCards
  #-}

module Data.RedBlackTree.InsertionAlgorithm (
  identifyRBTCase,
  insert,
  RBTCase (..)
) where

import Data.RedBlackTree.BinaryTree
  ( BinaryTreeNode
  , BinaryTree (..)
  , TreeBranch (..)
  , BranchType (..)
  , TreeDirection (..)
  , getTreeRoot
  , binaryTreeInsert
  , reconstructAncestor
  )
import Data.RedBlackTree.TreeFamily
  ( TreeFamily (..)
  , getTreeFamily
  )
import Data.RedBlackTree.Internal
  ( WhiteBranch (..)
  , RedBlackBranch
  , RedBlackTree
  , RedBlackNode (..)
  , RedBlackDirection
  , RedBlackDirections
  , RedBlack (..)
  , removeBranchColor
  , whiteBranch2Tree
  , isColor
  , branchIsColor
  )

-- | The 5 possible cases of red–black tree insertion to handle:
data RBTCase a
  = -- | 1) inserted node is the root node, i.e., first node of red–black tree.
    Case1
    { case1RootBranch :: WhiteBranch a
      -- ^ Stored as a 'WhiteBranch'without because it should always be black.
    }
  | -- | 2) inserted node has a parent, and it is black.
    Case2
    { case2Directions :: RedBlackDirections a
      -- ^ 'RedBlackDirections' to reconstruct all of the ancestors
    , case2Branch     :: RedBlackBranch a
      -- ^ The inserted node is stored as a 'RedBlackBranch'
    }
  | -- | 3) inserted node has a parent and an uncle, and they are both red.
    Case3
    { case3Directions :: RedBlackDirections a
      -- ^ 1st parameter is a 'RedBlackDirections' to reconstruct all of the remaining
      -- ancestors
    , case3GrandparentValue :: a
      -- ^ 2nd parameter is the node content of the grandparent.
    , case3Uncle :: WhiteBranch a
      -- ^ 3rd parameter is the uncle as 'WhiteBranch' because it is also
      -- assumed to be red.
    , case3Branch :: WhiteBranch a
      -- ^ 4th parameter is the inserted node as a 'WhiteBranch' because it is assumed
      -- to be red.
    }
  | -- | 4) inserted node is placed to right of left child of grandparent, or to left
    -- of right child of grandparent.
    Case4
    { case4Directions :: RedBlackDirections a
      -- ^ 1st parameter is a 'RedBlackDirections' to reconstruct all of the remaining
      -- ancestors
    , case4GrandparentDirection :: RedBlackDirection a
      -- ^ 2nd parameter is a 'RedBlackDirection' used to reconstruct the grandparent.
    , case4ParentValue :: RedBlackNode a
      -- ^ 3rd parameter is the parent as a 'RedBlackNode'.
    , case4Sibling :: RedBlackTree a
      -- ^ 4th parameter is the sibling of the inserted node as a 'RedBlackTree'.
    , case4Branch :: RedBlackBranch a
      -- ^ 5th parameter is the inserted node as a  'RedBlackBranch' because it is
      -- assumed to be red but we don't care about it right now.
    }
  | -- | 5) inserted node is placed to left of left child of grandparent, or to right
    -- of right child of grandparent.
    Case5
    { case5Directions :: RedBlackDirections a
      -- ^ 1st parameter is a 'RedBlackDirections' to reconstruct all of the remaining
      -- ancestors.
    , case5GrandparentDirection :: RedBlackDirection a
      -- ^ 2nd parameter is a 'RedBlackDirection' used to reconstruct the grandparent.
    , case5ParentValue :: a
      -- ^ 3rd parameter is content of the parent.
    , case5Sibling :: RedBlackTree a
      -- ^ 4th parameter is the sibling of the inserted node as a 'RedBlackTree'.
    , case5Branch :: RedBlackBranch a
      -- ^ 5th parameter is the inserted node as a 'RedBlackBranch' because it is
      -- assumed to be red but we don't care about it right now.
    }
  deriving (Eq, Ord, Show)


identifyCases345 :: BinaryTreeNode a => RedBlackDirections a -> RedBlackDirection a -> RedBlackDirection a -> RedBlackBranch a -> RBTCase a
identifyCases345 directions
  (TreeDirection grandparentBranchType grandparentNode
  (Branch (TreeBranch leftCousin (RedBlackNode Red uncleContent) rightCousin)))
  parentDirection
  newBranch =
    case grandparentBranchType of
      LeftBranch ->
        Case3 directions grandparentContent whiteParent whiteUncle
      RightBranch ->
        Case3 directions grandparentContent whiteUncle whiteParent
  where uncleNode = RedBlackNode Red uncleContent
        uncleBranch = TreeBranch leftCousin uncleNode rightCousin
        parentBranch = reconstructAncestor newBranch parentDirection
        grandparentDirection = TreeDirection grandparentBranchType
          grandparentNode (Branch (TreeBranch leftCousin uncleNode rightCousin))
        RedBlackNode _ grandparentContent = grandparentNode
        whiteUncle = removeBranchColor uncleBranch
        whiteParent = removeBranchColor parentBranch
identifyCases345 directions grandparentDirection parentDirection newBranch
  | grandparentBranchType /= parentBranchType =
    Case4 directions grandparentDirection parentNode siblingTree newBranch
  | grandparentBranchType == parentBranchType =
    Case5 directions grandparentDirection parentContent siblingTree newBranch
  where TreeDirection grandparentBranchType _ _ = grandparentDirection
        TreeDirection parentBranchType parentNode siblingTree = parentDirection
        RedBlackNode _ parentContent =  parentNode


identifyRBTCase :: (BinaryTreeNode a) => TreeFamily (RedBlackNode a) ->
  RBTCase a
identifyRBTCase (IsRoot rootBranch) = Case1 (removeBranchColor rootBranch)
identifyRBTCase (HasParent direction insertedBranch) = Case2 [] parentBranch
  where parentBranch = reconstructAncestor insertedBranch direction
identifyRBTCase (HasGrandparent directions grandparentDirection
  parentDirection insertedBranch) =
    if parentBranch `branchIsColor` Black
      then Case2 (grandparentDirection:directions) parentBranch
      else identifyCases345 directions grandparentDirection parentDirection
      insertedBranch
  where parentBranch = reconstructAncestor insertedBranch parentDirection
        grandparentBranch = reconstructAncestor parentBranch
          grandparentDirection
        TreeDirection _ _ uncleTree = parentDirection
        TreeBranch _ parentContent _ = parentBranch
        TreeBranch _ grandparentContent _ = grandparentBranch





handleRBTCase :: (BinaryTreeNode a) => RBTCase a -> RedBlackTree a
handleRBTCase (Case1 WhiteBranch{..}) =
  Branch TreeBranch
    { leftBranch = leftWhiteBranch
    , rightBranch = rightWhiteBranch
    , branchValue = RedBlackNode{nodeColor = Black, content = whiteBranchValue}
    }
handleRBTCase (Case2 directionsFromRoot newBranch) =
  Branch rootBranch
  where
    (rootBranch, _) = getTreeRoot (newBranch, directionsFromRoot)
handleRBTCase
  Case3
  { case3Directions = directionsFromRoot
  , case3GrandparentValue = grandparentContent
  , case3Uncle = leftWBranch
  , case3Branch = rightWBranch
  } =
  handleRBTCase . identifyRBTCase . getTreeFamily $ repaintedGrandparentZipper
  where
    newBranch = TreeBranch
      { leftBranch = whiteBranch2Tree leftWBranch Black
      , rightBranch = whiteBranch2Tree rightWBranch Black
      , branchValue =
          RedBlackNode
            { nodeColor = Red
            , content = grandparentContent
            }
      }
    repaintedGrandparentZipper = (newBranch, directionsFromRoot)
handleRBTCase
  Case4
  { case4Directions = directions
  , case4GrandparentDirection = grandparentDirection@(TreeDirection grandparentDirectionType _ _)
  , case4ParentValue = newLatestNode
  , case4Sibling = siblingTree
  , case4Branch = TreeBranch latestLeftTree (RedBlackNode _ newParentContent) latestRightTree
  } =
  handleRBTCase (Case5 directions grandparentDirection newParentContent newSiblingTree newLatestBranch)
  where
    newLatestBranch = case grandparentDirectionType of
      LeftBranch -> TreeBranch siblingTree newLatestNode latestLeftTree
      RightBranch -> TreeBranch latestRightTree newLatestNode siblingTree
    newSiblingTree = case grandparentDirectionType of
      LeftBranch -> latestRightTree
      RightBranch -> latestLeftTree
handleRBTCase
  Case5
  { case5Directions = directions
  , case5GrandparentDirection = TreeDirection grandparentDirectionType (RedBlackNode _ grandparentContent) uncleTree
  , case5ParentValue = parentContent
  , case5Sibling = siblingTree
  , case5Branch = latestBranch
  } =
  Branch postRotationRootBranch
  where
    newTopNode = RedBlackNode Black parentContent
    rotatedGrandparentNode = RedBlackNode Red grandparentContent
    latestTree = Branch latestBranch
    needsRightRotation = grandparentDirectionType == LeftBranch
    newSiblingTree = if needsRightRotation
      then Branch (TreeBranch siblingTree rotatedGrandparentNode uncleTree)
      else Branch (TreeBranch uncleTree rotatedGrandparentNode siblingTree)
    rotatedBranch = if needsRightRotation
      then TreeBranch latestTree newTopNode newSiblingTree
      else TreeBranch newSiblingTree newTopNode latestTree
    (postRotationRootBranch, _) = getTreeRoot (rotatedBranch, directions)


-- | inserts a new node to the tree, performing the necessary rotations to
-- guarantee that the red black properties are kept after the insertion.
insert :: (BinaryTreeNode a) => RedBlackTree a -> a -> RedBlackTree a
insert tree newItem = case insertedNodeColor of
  Black ->
    let (rootBranch, _) = getTreeRoot insertedBranchZipper
    in  Branch rootBranch
  Red ->
    handleRBTCase (identifyRBTCase (getTreeFamily insertedBranchZipper))
  where
    insertedBranchZipper@
      (TreeBranch{branchValue = RedBlackNode{nodeColor=insertedNodeColor}},_) =
        binaryTreeInsert tree RedBlackNode{nodeColor=Red,content=newItem}
