module Data.RedBlackTree.InsertionAlgorithmSpec (spec) where

import Test.Hspec
import Data.RedBlackTree.BinaryTree
import Data.RedBlackTree.TreeFamily
import Data.RedBlackTree.Internal
import Data.RedBlackTree.InsertionAlgorithm
import Data.RedBlackTree.RedBlackTreeAssertions
import Data.TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

getLeftTree :: (BinaryTreeNode a) => RedBlackTree a -> RedBlackTree a
getLeftTree (Branch (TreeBranch leftChild content _)) = leftChild

getRightTree :: (BinaryTreeNode a) => RedBlackTree a -> RedBlackTree a
getRightTree (Branch (TreeBranch _ content rightChild)) = rightChild

case3FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case3FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Red 7
        newNode = RedBlackNode Red 1
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = Branch newBranch
        parentBranch = TreeBranch newTree parentNode Leaf
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
        granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteUncle = WhiteBranch Leaf 7 Leaf
        whiteParent = WhiteBranch newTree 3 Leaf
        expectedCase = Case3 [ rootDirection ] 5 whiteParent whiteUncle

case4FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case4FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Black 7
        newNode = RedBlackNode Red 4
        newTree = Branch newBranch
        uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
        granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
        parentDirection = TreeDirection RightBranch parentNode Leaf
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch Leaf parentNode newTree
        newBranch = TreeBranch Leaf newNode Leaf
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        expectedCase = Case4 [ rootDirection ] grandparentDirection
                       parentNode Leaf newBranch

invertedCase4FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
invertedCase4FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 7
        uncleNode = RedBlackNode Black 3
        newNode = RedBlackNode Red 6
        newTree = Branch newBranch
        uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
        granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        grandparentDirection = TreeDirection RightBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch newTree parentNode Leaf
        newBranch = TreeBranch Leaf newNode Leaf
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        expectedCase = Case4 [ rootDirection ] grandparentDirection
                       parentNode Leaf newBranch

case5FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
case5FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 3
        uncleNode = RedBlackNode Black 7
        newNode = RedBlackNode Red 1
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = Branch newBranch
        uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
        granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
        parentDirection = TreeDirection LeftBranch parentNode Leaf
        grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch newTree parentNode Leaf
        parentTree = Branch parentBranch
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteParent = WhiteBranch newTree 3 Leaf
        expectedCase = Case5 [ rootDirection ] grandparentDirection 3 Leaf
                       newBranch

invertedCase5FamilyAndExpectation :: (TreeFamily (RedBlackNode Int), RBTCase Int)
invertedCase5FamilyAndExpectation = (treeFamily, expectedCase)
  where rootNode = RedBlackNode Black 10
        granduncleNode = RedBlackNode Red 15
        grandparentNode = RedBlackNode Black 5
        parentNode = RedBlackNode Red 7
        uncleNode = RedBlackNode Black 3
        newNode = RedBlackNode Red 8
        newBranch = TreeBranch Leaf newNode Leaf
        newTree = Branch newBranch
        uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
        granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
        parentDirection = TreeDirection RightBranch parentNode Leaf
        grandparentDirection = TreeDirection RightBranch grandparentNode
                                 uncleTree
        rootDirection = TreeDirection LeftBranch rootNode granduncleTree
        parentBranch = TreeBranch Leaf parentNode newTree
        parentTree = Branch parentBranch
        treeFamily = HasGrandparent [ rootDirection ] grandparentDirection
                       parentDirection newBranch
        whiteParent = WhiteBranch Leaf 7 newTree
        expectedCase = Case5 [ rootDirection ] grandparentDirection 7 Leaf
                       newBranch

spec :: Spec
spec = do
  describe "identifyRBTCase" $ do
    it "identifies insertion case #1" $ do
      let rootNode = RedBlackNode Red 1 :: RedBlackNode Int
      let rootBranch = TreeBranch Leaf rootNode Leaf
      let treeFamily = IsRoot rootBranch
      let expectedCase = Case1 (WhiteBranch Leaf 1 Leaf)

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #2 in a family that only has a parent" $ do
      let rootNode = RedBlackNode Black 3 :: RedBlackNode Int
      let leftNode = RedBlackNode Red 2
      let directionToChild = TreeDirection LeftBranch rootNode Leaf
      let leftChildBranch = TreeBranch Leaf leftNode Leaf
      let leftChild = Branch leftChildBranch
      let treeFamily = HasParent directionToChild leftChildBranch
      let expectedCase = Case2 [] (TreeBranch leftChild rootNode Leaf)

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #2 in a larger family" $ do
      let rootNode = RedBlackNode Black 10 :: RedBlackNode Int
      let granduncleNode = RedBlackNode Red 15
      let grandparentNode = RedBlackNode Red 5
      let parentNode = RedBlackNode Black 3
      let uncleNode = RedBlackNode Black 7
      let newNode = RedBlackNode Red 2
      let newBranch = TreeBranch Leaf newNode Leaf
      let newTree = Branch newBranch
      let parentBranch = TreeBranch newTree parentNode Leaf
      let parentDirection = TreeDirection LeftBranch parentNode Leaf
      let uncleTree = Branch (TreeBranch Leaf uncleNode Leaf)
      let granduncleTree = Branch (TreeBranch Leaf granduncleNode Leaf)
      let grandparentDirection = TreeDirection LeftBranch grandparentNode
                                 uncleTree
      let rootDirection = TreeDirection LeftBranch rootNode granduncleTree
      let treeFamily = HasGrandparent [rootDirection ] grandparentDirection
                       parentDirection newBranch
      let expectedCase = Case2 [ grandparentDirection, rootDirection ]
                         parentBranch

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #3" $ do
      let (treeFamily, expectedCase) = case3FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #4" $ do
      let (treeFamily, expectedCase) = case4FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #4 (inverted example)" $ do
      let (treeFamily, expectedCase) = invertedCase4FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #5" $ do
      let (treeFamily, expectedCase) = case5FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

    it "identifies insertion case #5 (inverted example)" $ do
      let (treeFamily, expectedCase) = invertedCase5FamilyAndExpectation

      let newCase = identifyRBTCase treeFamily

      newCase `shouldBe` expectedCase

  describe "insert" $ do
    it "if node is inserted at root, it is painted black" $ do
      let tree = Leaf :: RedBlackTree Int
      let newItem = 1
      let expectedTree = Branch (TreeBranch Leaf (RedBlackNode Black 1) Leaf)
      let modifiedTree = insert tree newItem

      modifiedTree `shouldBe` expectedTree
      modifiedTree `shouldBeColor` Black

    it "if inserted node lacks grandparent but parent is black, returns the root tree" $ do
      let rootNode = RedBlackNode Black 2 :: RedBlackNode Int
      let rootTree = Branch (TreeBranch Leaf rootNode Leaf)
      let newItem = 1
      let expectedInsertedTree = Branch (TreeBranch Leaf (RedBlackNode Red 1) Leaf)
      let expectedTree = Branch (TreeBranch expectedInsertedTree rootNode Leaf)

      let newTree = insert rootTree newItem
      let newTreeLeftChild = getLeftTree newTree

      newTree `shouldBe` expectedTree
      newTreeLeftChild `shouldBeColor` Red

    it "if inserted node has grandparent but parent is black, returns the root tree" $ do
      let rootNode = RedBlackNode Black 4 :: RedBlackNode Int
      let parentNode = RedBlackNode Black 3
      let parentTree = Branch (TreeBranch Leaf parentNode Leaf)
      let rootTree = Branch (TreeBranch parentTree rootNode Leaf)
      let newItem = 2
      let expectedInsertedTree = Branch (TreeBranch Leaf (RedBlackNode Red 2) Leaf)
      let expectedParentTree = Branch (TreeBranch expectedInsertedTree parentNode Leaf)
      let expectedTree = Branch (TreeBranch expectedParentTree rootNode Leaf)

      let newTree = insert rootTree newItem
      let insertedTree = (getLeftTree . getLeftTree) newTree

      newTree `shouldBe` expectedTree
      insertedTree `shouldBeColor` Red

    it "if inserted node is case 4, rotates tree correctly and returns the root" $ do
      let grandparentNode = RedBlackNode Black 5 :: RedBlackNode Int
      let parentNode = RedBlackNode Red 3
      let uncleNode = RedBlackNode Black 7
      let siblingNode = RedBlackNode Black 1

      let siblingTree = Branch (TreeBranch Leaf siblingNode Leaf)
      let leftSubtree = Branch (TreeBranch siblingTree parentNode Leaf)
      let rightSubtree = Branch (TreeBranch Leaf uncleNode Leaf)

      let case4Tree = Branch (TreeBranch leftSubtree grandparentNode rightSubtree)

      let expectedLeftSubtree = Branch (TreeBranch siblingTree (RedBlackNode Red 3) Leaf)
      let expectedRightSubtree = Branch (TreeBranch Leaf (RedBlackNode Red 5) rightSubtree)
      let expectedTree = Branch (TreeBranch expectedLeftSubtree (RedBlackNode Black 4) expectedRightSubtree)

      let newTree = insert case4Tree 4

      newTree `shouldBe` expectedTree
      -- assert colors since Eq instance is color blind
      getLeftTree newTree `shouldBeColor` Red
      getRightTree newTree `shouldBeColor` Red
      (getRightTree . getRightTree) newTree `shouldBeColor` Black
      (getLeftTree . getLeftTree) newTree `shouldBeColor` Black

    it "if inserted node is inverted case 4, rotates tree correctly and returns the root" $ do
        let grandparentNode = RedBlackNode Black 5 :: RedBlackNode Int
        let parentNode = RedBlackNode Red 7
        let uncleNode = RedBlackNode Black 3
        let siblingNode = RedBlackNode Black 8

        let siblingTree = Branch (TreeBranch Leaf siblingNode Leaf)
        let leftSubtree = Branch (TreeBranch Leaf uncleNode Leaf)
        let rightSubtree = Branch (TreeBranch Leaf parentNode siblingTree)

        let invertedCase4Tree = Branch (TreeBranch leftSubtree grandparentNode rightSubtree)

        let expectedLeftSubtree = Branch (TreeBranch leftSubtree (RedBlackNode Red 5) Leaf)
        let expectedRightSubtree = Branch (TreeBranch Leaf (RedBlackNode Red 7) siblingTree)
        let expectedTree = Branch (TreeBranch expectedLeftSubtree (RedBlackNode Black 6) expectedRightSubtree)

        let newTree = insert invertedCase4Tree 6

        newTree `shouldBe` expectedTree
        -- assert colors since Eq instance is color blind
        getLeftTree newTree `shouldBeColor` Red
        getRightTree newTree `shouldBeColor` Red
        (getRightTree . getRightTree) newTree `shouldBeColor` Black
        (getLeftTree . getLeftTree) newTree `shouldBeColor` Black

    it "if inserted node is case 5, rotates tree correctly and returns the root" $ do
        let grandparentNode = RedBlackNode Black 5 :: RedBlackNode Int
        let parentNode = RedBlackNode Red 3
        let uncleNode = RedBlackNode Black 7

        let leftSubtree = Branch (TreeBranch Leaf parentNode Leaf)
        let rightSubtree = Branch (TreeBranch Leaf uncleNode Leaf)

        let case5Tree = Branch (TreeBranch leftSubtree grandparentNode rightSubtree)

        let expectedLeftSubtree = Branch (TreeBranch Leaf (RedBlackNode Red 1) Leaf)
        let expectedRightSubtree = Branch (TreeBranch Leaf (RedBlackNode Red 5) rightSubtree)
        let expectedTree = Branch (TreeBranch expectedLeftSubtree (RedBlackNode Black 3) expectedRightSubtree)

        let newTree = insert case5Tree 1

        newTree `shouldBe` expectedTree
        -- assert colors since Eq instance is color blind
        getLeftTree newTree `shouldBeColor` Red
        getRightTree newTree `shouldBeColor` Red
        (getRightTree . getRightTree) newTree `shouldBeColor` Black
        (getLeftTree . getLeftTree) newTree `shouldBeColor` Black

    it "if inserted node is inverted case 5, rotates tree correctly and returns the root" $ do
        let grandparentNode = RedBlackNode Black 5 :: RedBlackNode Int
        let parentNode = RedBlackNode Red 7
        let uncleNode = RedBlackNode Black 3

        let leftSubtree = Branch (TreeBranch Leaf uncleNode Leaf)
        let rightSubtree = Branch (TreeBranch Leaf parentNode Leaf)

        let invertedCase5Tree = Branch (TreeBranch leftSubtree grandparentNode rightSubtree)

        let expectedLeftSubtree = Branch (TreeBranch leftSubtree (RedBlackNode Red 5) Leaf)
        let expectedRightSubtree = Branch (TreeBranch Leaf (RedBlackNode Red 8) Leaf)
        let expectedTree = Branch (TreeBranch expectedLeftSubtree (RedBlackNode Black 7) expectedRightSubtree)

        let newTree = insert invertedCase5Tree 8

        newTree `shouldBe` expectedTree
        -- assert colors since Eq instance is color blind
        getLeftTree newTree `shouldBeColor` Red
        getRightTree newTree `shouldBeColor` Red
        (getRightTree . getRightTree) newTree `shouldBeColor` Black
        (getLeftTree . getLeftTree) newTree `shouldBeColor` Black
