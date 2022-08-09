{-# LANGUAGE CPP #-}

-- | Properties.
--
-- @since 0.1
module Props.Data.Bounds
  ( props,
  )
where

import Data.Bounds (LowerBounded (..), UpperBounded (..))
import Data.Typeable (Proxy, Typeable, typeOf)
import Hedgehog
  ( Property,
    PropertyName,
    annotate,
    annotateShow,
    assert,
    forAll,
    property,
    withTests,
    (===),
  )
import Props.Generators (BoundedType (..), genBounded)
import Props.MaxRuns (MaxRuns (..))
import Test.Tasty (TestName, TestTree, askOption, testGroup)
#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
import Test.Tasty.Hedgehog (testPropertyNamed)
#else
import Test.Tasty.Hedgehog (testProperty)
#endif

-- | Entry-point for property tests.
--
-- @since 0.1
props :: TestTree
props =
  testGroup
    "Data.Bounds"
    [ minBoundSynced,
      maxBoundSynced
    ]

-- | maxBound === upperBound
--
-- @since 0.1
maxBoundSynced :: TestTree
maxBoundSynced = askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat "maxBound === upperBound" "maxBoundSynced" $
    withTests limit $
      property $
        forAll genBounded >>= \case
          MkBoundedType p@(_ :: Proxy a) -> do
            annotate $ showTy p
            annotateShow $ maxBound @a
            annotateShow $ upperBound @a
            maxBound @a === upperBound
          MkBoundedTypeNoEq p@(_ :: Proxy a) eqFn -> do
            annotate $ showTy p
            annotateShow $ maxBound @a
            annotateShow $ upperBound @a
            assert $ maxBound @a `eqFn` upperBound

-- | minBound === lowerBound
--
-- @since 0.1
minBoundSynced :: TestTree
minBoundSynced = askOption $ \(MkMaxRuns limit) ->
  testPropertyCompat "minBound === lowerBound" "minBoundSynced" $
    withTests limit $
      property $
        forAll genBounded >>= \case
          MkBoundedType p@(_ :: Proxy a) -> do
            annotate $ showTy p
            annotateShow $ minBound @a
            annotateShow $ lowerBound @a
            minBound @a === lowerBound
          MkBoundedTypeNoEq p@(_ :: Proxy a) eqFn -> do
            annotate $ showTy p
            annotateShow $ minBound @a
            annotateShow $ lowerBound @a
            assert $ minBound @a `eqFn` lowerBound

showTy :: Typeable a => Proxy a -> String
showTy = show . typeOf

#if MIN_VERSION_tasty_hedgehog(1, 2, 0)
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat = testPropertyNamed
#else
testPropertyCompat :: TestName -> PropertyName -> Property -> TestTree
testPropertyCompat tn _ = testProperty tn
#endif
