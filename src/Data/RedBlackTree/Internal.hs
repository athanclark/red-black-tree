{-# LANGUAGE
    NamedFieldPuns
  , RecordWildCards
  #-}

module Data.RedBlackTree.Internal (
  branchIsColor,
  getBlackHeight,
  isColor,
  emptyRedBlackTree,
  find,
  paintItBlack,
  removeBranchColor,
  whiteBranch2Tree,

  RedBlack (..),
  RedBlackNode (..),
  RedBlackBranch,
  RedBlackTree,
  RedBlackDirection,
  RedBlackDirections,
  WhiteBranch (..)
) where

import Data.RedBlackTree.BinaryTree
  ( BinaryTreeNode (mergeNodes)
  , BinaryTree (..)
  , TreeBranch (..)
  , TreeDirection (..)
  , binaryTreeFind
  )

-- | Red black trees can only have two types of nodes: Red and Black
data RedBlack = Red | Black deriving (Show, Eq, Ord)

-- | a @RedBlackNode@ contains only two elements, the color of the node and the
-- actual content.
data RedBlackNode a = RedBlackNode
  { nodeColor :: RedBlack
  , content :: a
  } deriving (Show)

instance (BinaryTreeNode a) => BinaryTreeNode (RedBlackNode a)  where
  mergeNodes
    RedBlackNode
    { nodeColor
    , content = leftContent
    }
    RedBlackNode
    { content = rightContent
    } = RedBlackNode { nodeColor, content = mergedContent }
    where
      mergedContent = leftContent `mergeNodes` rightContent

instance (BinaryTreeNode a) => Ord (RedBlackNode a) where
  (RedBlackNode _ lcontent) <= (RedBlackNode _ rcontent) =
    lcontent <= rcontent

instance (BinaryTreeNode a) => Eq (RedBlackNode a) where
  (RedBlackNode _ lcontent) == (RedBlackNode _ rcontent) =
    lcontent == rcontent



-- | A @BinaryTree@ with only nodes of type @RedBlackNode. is either a leaf
-- (empty) or a @RedBlackNode@ with 2 @RedBlackTree@ children, left and right
type RedBlackTree a = BinaryTree (RedBlackNode a)

-- A @TreeBranch@ with only nodes of type @RedBlackNode. Holds the data of a
-- @RedBlackTree@ created with the @Branch@ constructor. Useful
-- type when you want to guarantee that the element is not a @Leaf@
type RedBlackBranch a = TreeBranch (RedBlackNode a)

-- @TreeDirection@ for trees of type @RedBlackTree@. Minimum necessary to
-- reconstruct the parent of any focused node. First argument is the @BranchType@
-- of the focused node relative to the parent. Second argument is the parent's
-- node. The third argument is the sibling tree of the focused node.
type RedBlackDirection a = TreeDirection (RedBlackNode a)

-- List of @RedBlackDirection@
type RedBlackDirections a = [ RedBlackDirection a ]

-- Holds all the data of a @RedBlackBranch@ except for the color of the node
-- at the top of the branch
data WhiteBranch a = WhiteBranch
  { leftWhiteBranch  :: RedBlackTree a
  , whiteBranchValue :: a
  , rightWhiteBranch :: RedBlackTree a
  } deriving (Eq, Ord, Show)


isColor :: RedBlackNode a -> RedBlack -> Bool
isColor RedBlackNode{nodeColor} expectedColor = nodeColor == expectedColor

branchIsColor :: TreeBranch (RedBlackNode a) -> RedBlack -> Bool
branchIsColor TreeBranch{branchValue} = isColor branchValue

treeIsColor :: RedBlackTree a -> RedBlack -> Bool
treeIsColor tree expectedColor = case tree of
  Leaf -> expectedColor == Black
  Branch TreeBranch{branchValue} -> isColor branchValue expectedColor

paintItBlack :: RedBlackNode a -> RedBlackNode a
paintItBlack node = node{nodeColor=Black}

removeBranchColor :: RedBlackBranch a -> WhiteBranch a
removeBranchColor TreeBranch{leftBranch,rightBranch,branchValue = RedBlackNode{content}} =
  WhiteBranch
  { leftWhiteBranch = leftBranch
  , rightWhiteBranch = rightBranch
  , whiteBranchValue = content
  }

whiteBranch2Tree :: WhiteBranch a -> RedBlack -> RedBlackTree a
whiteBranch2Tree WhiteBranch{..} nodeColor =
  Branch $
    TreeBranch
    { leftBranch = leftWhiteBranch
    , rightBranch = rightWhiteBranch
    , branchValue
    }
  where
    branchValue = RedBlackNode{nodeColor,content = whiteBranchValue}

getBlackHeight :: RedBlackTree a -> Int
getBlackHeight tree = case tree of
  Leaf -> 1
  Branch TreeBranch{rightBranch,branchValue = RedBlackNode{nodeColor}} -> case nodeColor of
    Black -> 1 + getBlackHeight rightBranch
    Red -> getBlackHeight rightBranch

-- | Lookup a target node in the tree. The target value doesn't need to be the
-- exact same value that is already in the tree. It only needs to satisfy the
-- @Eq@ instance
find :: Ord a => RedBlackTree a -> a -> Maybe a
find tree target = fmap content (binaryTreeFind tree RedBlackNode{nodeColor=Black,content=target})

-- | Convenient function to "create" a new empty tree.
emptyRedBlackTree :: RedBlackTree a
emptyRedBlackTree = Leaf
