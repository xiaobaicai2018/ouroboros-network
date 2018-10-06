module Test.Chain
  ( tests
  , TestBlockChainAndUpdates(..)
  , TestBlockChain(..)
  , TestChainFork(..)
  , mkRollbackPoint
  ) where

import           Block
import           Chain ( Chain (..), Point (..), ChainUpdate (..), genesisPoint)
import qualified Chain

import qualified Data.List as L
import Data.Maybe (listToMaybe)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Chain"
  [ testGroup "generators"
    [ testProperty "arbitrary for TestBlockChain"  prop_arbitrary_TestBlockChain
    , testProperty "shrink for TestBlockChain"     prop_shrink_TestBlockChain

    , testProperty "arbitrary for TestHeaderChain" prop_arbitrary_TestHeaderChain
    , testProperty "shrink for TestHeaderChain"    prop_shrink_TestHeaderChain

    , testProperty "arbitrary for TestAddBlock" prop_arbitrary_TestAddBlock
    , testProperty "shrink for TestAddBlock"    prop_shrink_TestAddBlock

    , testProperty "arbitrary for TestChainAndPoint" prop_arbitrary_TestChainAndPoint
    , testProperty "shrink for TestChainAndPoint"    prop_shrink_TestChainAndPoint

    , testProperty "arbitrary for TestChainFork" prop_arbitrary_TestChainFork
    , testProperty "shrink for TestChainFork"
                               (mapSize (min 40) prop_shrink_TestChainFork)
    ]

  , testProperty "length/Genesis"  prop_length_genesis
  , testProperty "drop/Genesis"    prop_drop_genesis
  , testProperty "fromList/toList" prop_fromList_toList
  , testProperty "toList/head"     prop_toList_head
  , testProperty "drop"            prop_drop
  , testProperty "addBlock"        prop_addBlock
  , testProperty "rollback"        prop_rollback
  , testProperty "successorBlock"  prop_successorBlock
  , testProperty "lookupBySlot"    prop_lookupBySlot
  , testProperty "intersectChains" prop_intersectChains
  ]


genPoint :: Gen Point
genPoint = (\s h -> Point (Slot s) (HeaderHash h)) <$> arbitrary <*> arbitrary

--
-- Properties
--

prop_length_genesis :: Bool
prop_length_genesis = Chain.length Genesis == 0

prop_drop_genesis :: TestBlockChain -> Bool
prop_drop_genesis (TestBlockChain chain) =
    Chain.drop (Chain.length chain) chain == Genesis

prop_fromList_toList :: TestBlockChain -> Bool
prop_fromList_toList (TestBlockChain chain) =
    (Chain.fromList . Chain.toList) chain == chain

-- The list comes out in reverse order, most recent block at the head
prop_toList_head :: TestBlockChain -> Bool
prop_toList_head (TestBlockChain chain) =
    (listToMaybe . Chain.toList) chain == Chain.head chain

prop_drop :: TestBlockChain -> Bool
prop_drop (TestBlockChain chain) =
    and [ Chain.drop n chain == Chain.fromList (L.drop n blocks)
        | n <- [0..Prelude.length blocks] ]
  where
    blocks = Chain.toList chain

prop_addBlock :: TestAddBlock -> Bool
prop_addBlock (TestAddBlock c b) =
    -- after adding a block, that block is at the head
    Chain.headPoint c' == Chain.blockPoint b
    -- chain is still valid
 && Chain.valid c'
    -- removing the block gives the original
 && Chain.rollback (Chain.headPoint c) c' == Just c
 && Chain.drop 1 c' == c
    -- chain is one longer
 && Chain.length c' == Chain.length c + 1
  where
    c' = Chain.addBlock b c

prop_rollback :: TestChainAndPoint -> Property
prop_rollback (TestChainAndPoint c p) =
    case Chain.rollback p c of
      Nothing -> property True
      Just c' ->
        -- chain is a prefix of original
             Chain.isPrefixOf c' c
        -- chain head point is the rollback point
        .&&. Chain.headPoint c' === p

prop_successorBlock :: TestChainAndPoint -> Property
prop_successorBlock (TestChainAndPoint c p) =
  Chain.pointOnChain p c ==>
  case Chain.successorBlock p c of
    Nothing -> Chain.headPoint c === p
    Just b  -> property $ Chain.pointOnChain (Chain.blockPoint b) c

prop_lookupBySlot :: TestChainAndPoint -> Bool
prop_lookupBySlot (TestChainAndPoint c p) =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.pointOnChain (Chain.blockPoint b) c
    Nothing | p == genesisPoint -> True
            | otherwise         -> not (Chain.pointOnChain p c)

prop_intersectChains :: TestChainFork -> Bool
prop_intersectChains (TestChainFork c l r) =
  case Chain.intersectChains l r of
    Nothing -> c == Genesis && L.intersect (Chain.toList l) (Chain.toList r) == []
    Just p  -> Chain.headPoint c == p
            && Chain.pointOnChain p l
            && Chain.pointOnChain p r


--
-- Generators for chains
--

-- | A test generator for a valid chain of blocks.
--
newtype TestBlockChain = TestBlockChain (Chain Block)
    deriving (Eq, Show)

-- | A test generator for a valid chain of block headers.
--
newtype TestHeaderChain = TestHeaderChain (Chain BlockHeader)
    deriving (Eq, Show)

instance Arbitrary TestBlockChain where
    arbitrary = do
        NonNegative n <- arbitrary
        TestBlockChain <$> genBlockChain n

    shrink (TestBlockChain c) =
        [ TestBlockChain (fromListFixupBlocks c')
        | c' <- shrinkList (const []) (Chain.toList c) ]

instance Arbitrary TestHeaderChain where
    arbitrary = do
        NonNegative n <- arbitrary
        TestHeaderChain <$> genHeaderChain n

    shrink (TestHeaderChain c) =
        [ TestHeaderChain (fromListFixupHeaders c')
        | c' <- shrinkList (const []) (Chain.toList c) ]

prop_arbitrary_TestBlockChain :: TestBlockChain -> Bool
prop_arbitrary_TestBlockChain (TestBlockChain c) = Chain.valid c

prop_arbitrary_TestHeaderChain :: TestHeaderChain -> Bool
prop_arbitrary_TestHeaderChain (TestHeaderChain c) = Chain.valid c

prop_shrink_TestBlockChain :: TestBlockChain -> Bool
prop_shrink_TestBlockChain c =
    and [ Chain.valid c' | TestBlockChain c' <- shrink c ]

prop_shrink_TestHeaderChain :: TestHeaderChain -> Bool
prop_shrink_TestHeaderChain c =
    and [ Chain.valid c' | TestHeaderChain c' <- shrink c ]

genBlockChain :: Int -> Gen (Chain Block)
genBlockChain n = do
    bodies <- vector n
    slots  <- mkSlots <$> vectorOf n genSlotGap
    return (mkChain slots bodies)
  where
    mkSlots :: [Int] -> [Slot]
    mkSlots = map toEnum . tail . scanl (+) 0

    mkChain :: [Slot] -> [BlockBody] -> Chain Block
    mkChain slots bodies =
        fromListFixupBlocks
      . reverse
      $ zipWith mkPartialBlock slots bodies

genSlotGap :: Gen Int
genSlotGap = frequency [(25, pure 1), (5, pure 2), (1, pure 3)]

addSlotGap :: Int -> Slot -> Slot
addSlotGap g (Slot n) = Slot (n + fromIntegral g)

genHeaderChain :: Int -> Gen (Chain BlockHeader)
genHeaderChain = fmap (fmap blockHeader) . genBlockChain

mkPartialBlock :: Slot -> BlockBody -> Block
mkPartialBlock sl body =
    Block {
      blockHeader = BlockHeader {
        headerSlot     = sl,
        headerSigner   = expectedBFTSigner sl,
        headerHash     = partialField "headerHash",
        headerPrevHash = partialField "headerPrevHash",
        headerBlockNo  = partialField "headerBlockNo",
        headerBodyHash = hashBody body
      }
    , blockBody = body
    }
  where
    partialField n = error ("mkPartialBlock: you didn't fill in field " ++ n)

expectedBFTSigner :: Slot -> BlockSigner
expectedBFTSigner (Slot n) = BlockSigner (n `mod` 7)


-- | To help with chain construction and shrinking it's handy to recalculate
-- all the hashes.
--
fromListFixupBlocks :: [Block] -> Chain Block
fromListFixupBlocks []      = Genesis
fromListFixupBlocks (b : c) = c' :> b'
  where
    c' = fromListFixupBlocks c
    b' = Chain.fixupBlock (Chain.headPoint c') (Chain.headBlockNo c') b

fromListFixupHeaders :: [BlockHeader] -> Chain BlockHeader
fromListFixupHeaders []      = Genesis
fromListFixupHeaders (b : c) = c' :> b'
  where
    c' = fromListFixupHeaders c
    b' = Chain.fixupBlockHeader (Chain.headPoint c') (Chain.headBlockNo c')
                          (headerBodyHash b) b

-- | The Ouroboros K paramater. This is also the maximum rollback length.
--
k :: Int
k = 5

--
-- Generator for chain and single block
--

-- | A test generator for a chain and a block that can be appended to it.
--
data TestAddBlock = TestAddBlock (Chain Block) Block
  deriving Show

instance Arbitrary TestAddBlock where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    block <- genAddBlock chain
    return (TestAddBlock chain block)

  shrink (TestAddBlock c b) =
    [ TestAddBlock c' b'
    | TestBlockChain c' <- shrink (TestBlockChain c)
    , let b' = Chain.fixupBlock (Chain.headPoint c') (Chain.headBlockNo c') b
    ]

genAddBlock :: HasHeader block => Chain block -> Gen Block
genAddBlock chain = do
    slotGap <- genSlotGap
    body    <- arbitrary
    let pb = mkPartialBlock (addSlotGap slotGap (Chain.headSlot chain)) body
        b  = Chain.fixupBlock (Chain.headPoint chain) (Chain.headBlockNo chain) pb
    return b

prop_arbitrary_TestAddBlock :: TestAddBlock -> Bool
prop_arbitrary_TestAddBlock (TestAddBlock c b) = Chain.valid (c :> b)

prop_shrink_TestAddBlock :: TestAddBlock -> Bool
prop_shrink_TestAddBlock t =
    and [ Chain.valid (c :> b) | TestAddBlock c b <- shrink t ]


--
-- Generator for chain updates
--

-- | A test generator for a chain and a sequence of updates that can be applied
-- to it.
--
data TestBlockChainAndUpdates =
       TestBlockChainAndUpdates (Chain Block) [ChainUpdate Block]
  deriving Show

instance Arbitrary TestBlockChainAndUpdates where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    NonNegative m <- arbitrary
    updates <- genChainUpdates chain m
    return (TestBlockChainAndUpdates chain updates)

genChainUpdate :: Chain Block -> Gen (ChainUpdate Block)
genChainUpdate chain = do
    let maxRollback = Chain.length chain `min` k
    n <- choose (-10, maxRollback)
    if n <= 0
      then AddBlock <$> genAddBlock chain
      else pure (RollBack (mkRollbackPoint chain n))

mkRollbackPoint :: HasHeader block => Chain block -> Int -> Point
mkRollbackPoint chain n = Chain.headPoint $ Chain.drop n chain

genChainUpdates :: Chain Block -> Int -> Gen [ChainUpdate Block]
genChainUpdates _     0 = return []
genChainUpdates chain n = do
    update  <- genChainUpdate chain
    let chain' = Chain.applyChainUpdate update chain
    updates <- genChainUpdates chain' (n-1)
    return (update : updates)


--
-- Generator for chain and single point on the chain
--

-- | A test generator for a chain and a points. In most cases the point is
-- on the chain, but it also covers at least 5% of cases where the point is
-- not on the chain.
--
data TestChainAndPoint = TestChainAndPoint (Chain Block) Point
  deriving Show

instance Arbitrary TestChainAndPoint where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    let len = Chain.length chain
    -- either choose point from the chain
    point <- frequency
      [ (2, return (Chain.headPoint chain))
      , (2, return (mkRollbackPoint chain len))
      , (8, mkRollbackPoint chain <$> choose (1, len - 1))
      -- or a few off the chain!
      , (1, genPoint)
      ]
    return (TestChainAndPoint chain point)

  shrink (TestChainAndPoint c p)
    | Chain.pointOnChain p c
    = [ TestChainAndPoint c' (fixupPoint c' p)
    | TestBlockChain c' <- shrink (TestBlockChain c)]
    | otherwise
    = [ TestChainAndPoint c' p
      | TestBlockChain c' <- shrink (TestBlockChain c) ]

fixupPoint :: HasHeader block => Chain block -> Point -> Point
fixupPoint c p =
  case Chain.lookupBySlot c (pointSlot p) of
    Just b  -> Chain.blockPoint b
    Nothing -> Chain.headPoint c

prop_arbitrary_TestChainAndPoint :: TestChainAndPoint -> Property
prop_arbitrary_TestChainAndPoint (TestChainAndPoint c p) =
  cover (85/100) onChain       "point on chain" $
  cover ( 5/100) (not onChain) "point not on chain" $
    Chain.valid c
  where
    onChain = Chain.pointOnChain p c

prop_shrink_TestChainAndPoint :: TestChainAndPoint -> Bool
prop_shrink_TestChainAndPoint cp@(TestChainAndPoint c _) =
  and [ Chain.valid c' && (not (Chain.pointOnChain p c) || Chain.pointOnChain p c')
      | TestChainAndPoint c' p <- shrink cp ]


--
-- Generator for chain forks sharing a common prefix
--

-- | A test generator for two chains sharing a common prefix.
--
data TestChainFork = TestChainFork (Chain Block) -- common prefix
                                   (Chain Block) -- left fork
                                   (Chain Block) -- right fork
  deriving Show

instance Arbitrary TestChainFork where
  arbitrary = do
    TestBlockChain chain <- arbitrary
    -- at least 5% of forks should be equal
    equalChains <- frequency [(1, pure True), (19, pure False)]
    if equalChains
      then return (TestChainFork chain chain chain)
      else do
        (NonNegative l, NonNegative r) <- arbitrary
        chainL <- genAddBlocks l chain
        chainR <- genAddBlocks r chain
        return (TestChainFork chain chainL chainR)

    where
      genAddBlocks :: Int -> Chain Block -> Gen (Chain Block)
      genAddBlocks 0 c = return c
      genAddBlocks n c = do
          b <- genAddBlock c
          genAddBlocks (n-1) (Chain.addBlock b c)


  shrink (TestChainFork common l r) =
        -- shrink the common prefix
      [ TestChainFork (fromListFixupBlocks common')
                      (fromListFixupBlocks (exl ++ common'))
                      (fromListFixupBlocks (exr ++ common'))
      | let exl = extensionFragment common l
            exr = extensionFragment common r
      , common' <- shrinkList (const []) (Chain.toList common)
      ]
        -- shrink the left fork
   ++ [ TestChainFork common l' r
      | let exl = extensionFragment common l
      , exl' <- shrinkList (const []) exl
      , let l' = fromListFixupBlocks (exl' ++ Chain.toList common)
      ]
        -- shrink the right fork
   ++ [ TestChainFork common l r'
      | let exr = extensionFragment common r
      , exr' <- shrinkList (const []) exr
      , let r' = fromListFixupBlocks (exr' ++ Chain.toList common)
      ]
    where
      extensionFragment :: Chain Block -> Chain Block -> [Block]
      extensionFragment c = reverse . L.drop (Chain.length c) . reverse . Chain.toList

prop_arbitrary_TestChainFork :: TestChainFork -> Bool
prop_arbitrary_TestChainFork (TestChainFork c l r) =
    Chain.valid c && Chain.valid l && Chain.valid r
 && c `Chain.isPrefixOf` l
 && c `Chain.isPrefixOf` r

prop_shrink_TestChainFork :: TestChainFork -> Bool
prop_shrink_TestChainFork forks =
  and [    prop_arbitrary_TestChainFork forks'
        && measure forks' < mforks
      | let mforks = measure forks
      , forks' <- shrink forks ]
  where
    measure (TestChainFork c l r) = Chain.length c
                                  + Chain.length l
                                  + Chain.length r