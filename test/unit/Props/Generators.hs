{-# LANGUAGE CPP #-}

-- | Generators.
--
-- @since 0.1
module Props.Generators
  ( BoundedType (..),
    genBounded,
  )
where

import Control.Applicative (Const)
import Data.Bounds
  ( LowerBounded,
    MaybeLowerBounded,
    MaybeUpperBounded,
    UpperBounded,
  )
#if MIN_VERSION_base(4, 16, 0)
import Data.Bits (And, Iff, Ior, Xor)
#endif
import Data.Char (GeneralCategory)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Ap)
import Data.Ord (Down)
import Data.Semigroup
  ( All,
    Any,
    Dual,
    First,
    Last,
    Max,
    Min,
    Product,
    Sum,
    WrappedMonoid,
  )
#if MIN_VERSION_base(4, 16, 0)
import Data.Tuple (Solo)
#endif
import Data.Type.Equality (type (:~:))
import Data.Typeable (Proxy (Proxy), Typeable, type (:~~:))
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types
  ( CBool,
    CChar,
    CInt,
    CIntMax,
    CIntPtr,
    CLLong,
    CLong,
    CPtrdiff,
    CSChar,
    CShort,
    CSigAtomic,
    CSize,
    CUChar,
    CUInt,
    CUIntMax,
    CUIntPtr,
    CULLong,
    CULong,
    CUShort,
    CWchar,
  )
import Foreign.Ptr (IntPtr, WordPtr)
import GHC.ByteOrder (ByteOrder)
#if MIN_VERSION_base(4, 16, 0)
import GHC.Exts (Levity, VecCount, VecElem)
#else
import GHC.Exts (VecCount, VecElem)
#endif
import GHC.Generics
  ( Associativity,
    DecidedStrictness,
    SourceStrictness,
    SourceUnpackedness,
  )
import Hedgehog (Gen)
import Hedgehog.Gen (choice)
import System.Posix.Types qualified as Posix

-- | Holds the type we want to test along with required constraints.
--
-- @since 0.1
data BoundedType where
  -- | Prefer this when we have an Eq instance.
  --
  -- @since 0.1
  MkBoundedType ::
    ( Bounded a,
      Eq a,
      LowerBounded a,
      MaybeLowerBounded a,
      MaybeUpperBounded a,
      Show a,
      Typeable a,
      UpperBounded a
    ) =>
    Proxy a ->
    BoundedType
  -- | Manually provide eq function when we do not have an Eq instance.
  --
  -- @since 0.1
  MkBoundedTypeNoEq ::
    ( Bounded a,
      LowerBounded a,
      MaybeLowerBounded a,
      MaybeUpperBounded a,
      Show a,
      Typeable a,
      UpperBounded a
    ) =>
    Proxy a ->
    (a -> a -> Bool) ->
    BoundedType

-- | @since 0.1
instance Show BoundedType where
  show (MkBoundedType p) = "MkBoundedType " <> show p
  show (MkBoundedTypeNoEq p _) = "MkBoundedTypeNoEq " <> show p

{- ORMOLU_DISABLE -}

-- | Randomly generates a type that has Bounded and Lower/UpperBounded
-- instances.
--
-- @since 0.1
genBounded :: Gen BoundedType
genBounded =
  choice $
    [ pure $ MkBoundedType @All Proxy,
      pure $ MkBoundedType @Any Proxy,
      pure $ MkBoundedType @CBool Proxy,
      pure $ MkBoundedType @CChar Proxy,
      pure $ MkBoundedType @CInt Proxy,
      pure $ MkBoundedType @CIntMax Proxy,
      pure $ MkBoundedType @CIntPtr Proxy,
      pure $ MkBoundedType @CLLong Proxy,
      pure $ MkBoundedType @CLong Proxy,
      pure $ MkBoundedType @CPtrdiff Proxy,
      pure $ MkBoundedType @CSChar Proxy,
      pure $ MkBoundedType @CShort Proxy,
      pure $ MkBoundedType @CSigAtomic Proxy,
      pure $ MkBoundedType @CSize Proxy,
      pure $ MkBoundedType @CUChar Proxy,
      pure $ MkBoundedType @CUInt Proxy,
      pure $ MkBoundedType @CUIntMax Proxy,
      pure $ MkBoundedType @CUIntPtr Proxy,
      pure $ MkBoundedType @CULLong Proxy,
      pure $ MkBoundedType @CULong Proxy,
      pure $ MkBoundedType @CUShort Proxy,
      pure $ MkBoundedType @CWchar Proxy,
      pure $ MkBoundedType @IntPtr Proxy,
      pure $ MkBoundedType @WordPtr Proxy,
      pure $ MkBoundedType @ByteOrder Proxy,
      pure $ MkBoundedType @Associativity Proxy,
      pure $ MkBoundedType @DecidedStrictness Proxy,
      pure $ MkBoundedType @SourceStrictness Proxy,
      pure $ MkBoundedType @SourceUnpackedness Proxy,
      pure $ MkBoundedType @Int16 Proxy,
      pure $ MkBoundedType @Int32 Proxy,
      pure $ MkBoundedType @Int64 Proxy,
      pure $ MkBoundedType @Int8 Proxy,
      pure $ MkBoundedType @GeneralCategory Proxy,
      pure $ MkBoundedType @Word16 Proxy,
      pure $ MkBoundedType @Word32 Proxy,
      pure $ MkBoundedType @Word64 Proxy,
      pure $ MkBoundedType @Word8 Proxy,
#if !WINDOWS
      pure $ MkBoundedType @Posix.CBlkCnt Proxy,
      pure $ MkBoundedType @Posix.CBlkSize Proxy,
      pure $ MkBoundedType @Posix.CClockId Proxy,
      pure $ MkBoundedType @Posix.CDev Proxy,
      pure $ MkBoundedType @Posix.CFsBlkCnt Proxy,
      pure $ MkBoundedType @Posix.CFsFilCnt Proxy,
      pure $ MkBoundedType @Posix.CGid Proxy,
      pure $ MkBoundedType @Posix.CId Proxy,
      pure $ MkBoundedType @Posix.CIno Proxy,
      pure $ MkBoundedType @Posix.CKey Proxy,
      pure $ MkBoundedType @Posix.CMode Proxy,
      pure $ MkBoundedType @Posix.CNfds Proxy,
      pure $ MkBoundedType @Posix.CNlink Proxy,
      pure $ MkBoundedType @Posix.COff Proxy,
      pure $ MkBoundedType @Posix.CPid Proxy,
      pure $ MkBoundedType @Posix.CRLim Proxy,
      pure $ MkBoundedType @Posix.CSsize Proxy,
      pure $ MkBoundedType @Posix.CTcflag Proxy,
      pure $ MkBoundedType @Posix.CUid Proxy,
#endif
      pure $ MkBoundedType @Posix.Fd Proxy,
      pure $ MkBoundedType @Ordering Proxy,
      pure $ MkBoundedType @() Proxy,
      pure $ MkBoundedType @Bool Proxy,
      pure $ MkBoundedType @Char Proxy,
      pure $ MkBoundedType @Int Proxy,
      pure $ MkBoundedTypeNoEq @VecCount Proxy eqFromShow,
      pure $ MkBoundedTypeNoEq @VecElem Proxy eqFromShow,
      pure $ MkBoundedType @Word Proxy,
      pure $ MkBoundedType @(Identity Bool) Proxy,
      pure $ MkBoundedType @(Down Bool) Proxy,
      pure $ MkBoundedType @(First Bool) Proxy,
      pure $ MkBoundedType @(Last Bool) Proxy,
      pure $ MkBoundedType @(Max Bool) Proxy,
      pure $ MkBoundedType @(Min Bool) Proxy,
      pure $ MkBoundedType @(WrappedMonoid Bool) Proxy,
      pure $ MkBoundedType @(Dual Bool) Proxy,
      pure $ MkBoundedType @(Product Bool) Proxy,
      pure $ MkBoundedType @(Sum Bool) Proxy,
      pure $ MkBoundedType @(Proxy Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool) Proxy,
      pure $ MkBoundedType @(Const Bool Bool) Proxy,
      pure $ MkBoundedType @(Ap Identity Bool) Proxy,
      pure $ MkBoundedType @(Bool :~: Bool) Proxy,
      pure $ MkBoundedType @(Bool :~~: Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy,
      pure $ MkBoundedType @(Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool, Bool) Proxy
    ]
      <> bounded_4_16

{- ORMOLU_ENABLE -}

-- | Types that are only in base 4.16+
--
-- @since 0.1
bounded_4_16 :: [Gen BoundedType]
#if MIN_VERSION_base(4, 16, 0)
bounded_4_16 =
  [ pure $ MkBoundedType @(And Bool) Proxy,
    pure $ MkBoundedType @(Iff Bool) Proxy,
    pure $ MkBoundedType @(Ior Bool) Proxy,
    pure $ MkBoundedType @(Xor Bool) Proxy,
    pure $ MkBoundedType @(Solo Bool) Proxy,
    pure $ MkBoundedTypeNoEq @Levity Proxy eqFromShow
  ]
#else
bounded_4_16 = []
#endif

eqFromShow :: (Show a, Show b) => a -> b -> Bool
eqFromShow x y = show x == show y
