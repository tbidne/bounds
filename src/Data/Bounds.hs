{-# LANGUAGE CPP #-}

-- | Provides various typeclasses for dealing with bounded and unbounded
-- types.
module Data.Bounds
  ( -- * Bounded
    LowerBounded (..),
    UpperBounded (..),
    Bounds (..),

    -- * Boundless
    LowerBoundless,
    UpperBoundless,
    Boundless,
  )
where

import Control.Applicative (Const (Const))
#if MIN_VERSION_base(4, 16, 0)
import Data.Bits (And (And), Iff (Iff), Ior (Ior), Xor (Xor))
#endif
import Data.Char (GeneralCategory)
import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity (Identity))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Ap (Ap))
import Data.Ord (Down (Down))
import Data.Proxy (Proxy)
import Data.Semigroup
  ( All,
    Any,
    Dual (Dual),
    First (First),
    Last (Last),
    Max (Max),
    Min (Min),
    Product (Product),
    Sum (Sum),
    WrappedMonoid (WrapMonoid),
  )
#if MIN_VERSION_base(4, 16, 0)
import Data.Tuple (Solo (Solo))
#endif
import Data.Type.Coercion (Coercion)
import Data.Type.Equality ((:~:) (Refl), (:~~:) (HRefl), type (:~:), type (~~))
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
  )
import GHC.Natural (Natural)
#ifndef mingw32_HOST_OS
import System.Posix.Types
  ( CBlkCnt,
    CBlkSize,
    CClockId,
    CDev,
    CFsBlkCnt,
    CFsFilCnt,
    CGid,
    CId,
    CIno,
    CKey,
    CMode,
    CNfds,
    CNlink,
    COff,
    CPid,
    CRLim,
    CSsize,
    CTcflag,
    CUid,
    Fd,
  )
#endif

-- | Names the lower limit of a type. Types that also have a 'Bounded'
-- instance should define @lowerBound === minBound@. This can be derived
-- with the anyclass strategy.
--
-- ==== __Examples__
--
-- >>> -- -XDeriveAnyClass, -XDerivingStrategies
-- >>> data Foo = Foo1 | Foo2 deriving stock (Bounded, Show) deriving anyclass LowerBounded
-- >>> lowerBound @Foo
-- Foo1
--
-- @since 0.1
class LowerBounded a where
  -- | @since 0.1
  lowerBound :: a
  default lowerBound :: (Bounded a) => a
  lowerBound = minBound

-- | @since 0.1
instance LowerBounded Natural where
  lowerBound = 0
  {-# INLINE lowerBound #-}

-- | Names the upper limit of a type. Types that also have a 'Bounded'
-- instance should define @upperBound === maxBound@. This can be derived
-- with the anyclass strategy.
--
-- ==== __Examples__
--
-- >>> -- -XDeriveAnyClass, -XDerivingStrategies
-- >>> data Foo = Foo1 | Foo2 deriving stock (Bounded, Show) deriving anyclass UpperBounded
-- >>> upperBound @Foo
-- Foo2
--
-- @since 0.1
class UpperBounded a where
  -- | @since 0.1
  upperBound :: a
  default upperBound :: (Bounded a) => a
  upperBound = maxBound

-- | Types that have no lower bound.
--
-- @since 0.1
class LowerBoundless a

-- | @since 0.1
instance LowerBoundless Integer

-- | Types that have no upper bound.
--
-- @since 0.1
class UpperBoundless a

-- | @since 0.1
instance UpperBoundless Integer

-- | @since 0.1
instance UpperBoundless Natural

-- | Types that are unbounded above and below.
--
-- @since 0.1
class (LowerBoundless a, UpperBoundless a) => Boundless a

-- | @since 0.1
instance Boundless Integer

-- | Newtype that gives 'UpperBounded' and 'LowerBounded' instances to types
-- that only have a @Bounded@ instance.
--
-- ==== __Examples__
-- >>> newtype Foo = MkFoo Int8 deriving stock (Bounded, Show)
-- >>> upperBound @(Bounds Foo)
-- MkBounds {unBounds = MkFoo 127}
--
-- >>> lowerBound @(Bounds Foo)
-- MkBounds {unBounds = MkFoo (-128)}
--
-- @since 0.1
newtype Bounds a = MkBounds
  { -- | @since 0.1
    unBounds :: a
  }
  deriving stock
    ( -- | @since 0.1
      Bounded,
      -- | @since 0.1
      Eq,
      -- | @since 0.1
      Ord,
      -- | @since 0.1
      Show
    )
  deriving anyclass
    ( -- | @since 0.1
      LowerBounded,
      -- | @since 0.1
      UpperBounded
    )

--------------------
-- BASE INSTANCES --
--------------------

-- | @since 0.1
instance LowerBounded All

-- | @since 0.1
instance UpperBounded All

-- | @since 0.1
instance LowerBounded Any

-- | @since 0.1
instance UpperBounded Any

-- | @since 0.1
instance LowerBounded CBool

-- | @since 0.1
instance UpperBounded CBool

-- | @since 0.1
instance LowerBounded CChar

-- | @since 0.1
instance UpperBounded CChar

-- | @since 0.1
instance LowerBounded CInt

-- | @since 0.1
instance UpperBounded CInt

-- | @since 0.1
instance LowerBounded CIntMax

-- | @since 0.1
instance UpperBounded CIntMax

-- | @since 0.1
instance UpperBounded CIntPtr

-- | @since 0.1
instance LowerBounded CIntPtr

-- | @since 0.1
instance LowerBounded CLLong

-- | @since 0.1
instance UpperBounded CLLong

-- | @since 0.1
instance LowerBounded CLong

-- | @since 0.1
instance UpperBounded CLong

-- | @since 0.1
instance LowerBounded CPtrdiff

-- | @since 0.1
instance UpperBounded CPtrdiff

-- | @since 0.1
instance LowerBounded CSChar

-- | @since 0.1
instance UpperBounded CSChar

-- | @since 0.1
instance LowerBounded CShort

-- | @since 0.1
instance UpperBounded CShort

-- | @since 0.1
instance LowerBounded CSigAtomic

-- | @since 0.1
instance UpperBounded CSigAtomic

-- | @since 0.1
instance LowerBounded CSize

-- | @since 0.1
instance UpperBounded CSize

-- | @since 0.1
instance LowerBounded CUChar

-- | @since 0.1
instance UpperBounded CUChar

-- | @since 0.1
instance LowerBounded CUInt

-- | @since 0.1
instance UpperBounded CUInt

-- | @since 0.1
instance LowerBounded CUIntMax

-- | @since 0.1
instance UpperBounded CUIntMax

-- | @since 0.1
instance LowerBounded CUIntPtr

-- | @since 0.1
instance UpperBounded CUIntPtr

-- | @since 0.1
instance LowerBounded CULLong

-- | @since 0.1
instance UpperBounded CULLong

-- | @since 0.1
instance LowerBounded CULong

-- | @since 0.1
instance UpperBounded CULong

-- | @since 0.1
instance LowerBounded CUShort

-- | @since 0.1
instance UpperBounded CUShort

-- | @since 0.1
instance LowerBounded CWchar

-- | @since 0.1
instance UpperBounded CWchar

-- | @since 0.1
instance LowerBounded IntPtr

-- | @since 0.1
instance UpperBounded IntPtr

-- | @since 0.1
instance LowerBounded WordPtr

-- | @since 0.1
instance UpperBounded WordPtr

-- | @since 0.1
instance LowerBounded ByteOrder

-- | @since 0.1
instance UpperBounded ByteOrder

-- | @since 0.1
instance LowerBounded Associativity

-- | @since 0.1
instance UpperBounded Associativity

-- | @since 0.1
instance LowerBounded DecidedStrictness

-- | @since 0.1
instance UpperBounded DecidedStrictness

-- | @since 0.1
instance LowerBounded SourceStrictness

-- | @since 0.1
instance UpperBounded SourceStrictness

-- | @since 0.1
instance LowerBounded Int16

-- | @since 0.1
instance UpperBounded Int16

-- | @since 0.1
instance LowerBounded Int32

-- | @since 0.1
instance UpperBounded Int32

-- | @since 0.1
instance LowerBounded Int64

-- | @since 0.1
instance UpperBounded Int64

-- | @since 0.1
instance LowerBounded Int8

-- | @since 0.1
instance UpperBounded Int8

-- | @since 0.1
instance LowerBounded GeneralCategory

-- | @since 0.1
instance UpperBounded GeneralCategory

-- | @since 0.1
instance LowerBounded Word16

-- | @since 0.1
instance UpperBounded Word16

-- | @since 0.1
instance LowerBounded Word32

-- | @since 0.1
instance UpperBounded Word32

-- | @since 0.1
instance LowerBounded Word64

-- | @since 0.1
instance UpperBounded Word64

-- | @since 0.1
instance LowerBounded Word8

-- | @since 0.1
instance UpperBounded Word8

-- NOTE: GHC hides these types in System.Posix.Types behind flags that are
-- specific to each type e.g. CBlkCnt is behind HTYPE_BLKCNT_T.
-- These didn't appear to work here, so we just use a windows/no-windows check
-- and hope that is good enough.

#ifndef mingw32_HOST_OS
-- | @since 0.1
instance LowerBounded CBlkCnt

-- | @since 0.1
instance UpperBounded CBlkCnt

-- | @since 0.1
instance LowerBounded CBlkSize

-- | @since 0.1
instance UpperBounded CBlkSize

-- | @since 0.1
instance LowerBounded CClockId

-- | @since 0.1
instance UpperBounded CClockId

-- | @since 0.1
instance LowerBounded CDev

-- | @since 0.1
instance UpperBounded CDev

-- | @since 0.1
instance LowerBounded CFsBlkCnt

-- | @since 0.1
instance UpperBounded CFsBlkCnt

-- | @since 0.1
instance LowerBounded CFsFilCnt

-- | @since 0.1
instance UpperBounded CFsFilCnt

-- | @since 0.1
instance LowerBounded CGid

-- | @since 0.1
instance UpperBounded CGid

-- | @since 0.1
instance LowerBounded CId

-- | @since 0.1
instance UpperBounded CId

-- | @since 0.1
instance LowerBounded CIno

-- | @since 0.1
instance UpperBounded CIno

-- | @since 0.1
instance LowerBounded CKey

-- | @since 0.1
instance UpperBounded CKey

-- | @since 0.1
instance LowerBounded CMode

-- | @since 0.1
instance UpperBounded CMode

-- | @since 0.1
instance LowerBounded CNfds

-- | @since 0.1
instance UpperBounded CNfds

-- | @since 0.1
instance LowerBounded CNlink

-- | @since 0.1
instance UpperBounded CNlink

-- | @since 0.1
instance LowerBounded COff

-- | @since 0.1
instance UpperBounded COff

-- | @since 0.1
instance LowerBounded CPid

-- | @since 0.1
instance UpperBounded CPid

-- | @since 0.1
instance LowerBounded CRLim

-- | @since 0.1
instance UpperBounded CRLim

-- | @since 0.1
instance LowerBounded CSsize

-- | @since 0.1
instance UpperBounded CSsize

-- | @since 0.1
instance LowerBounded CTcflag

-- | @since 0.1
instance UpperBounded CTcflag

-- | @since 0.1
instance LowerBounded CUid

-- | @since 0.1
instance UpperBounded CUid
#endif

-- | @since 0.1
instance LowerBounded Fd

-- | @since 0.1
instance UpperBounded Fd

-- | @since 0.1
instance LowerBounded Ordering

-- | @since 0.1
instance UpperBounded Ordering

-- | @since 0.1
instance LowerBounded ()

-- | @since 0.1
instance UpperBounded ()

-- | @since 0.1
instance LowerBounded Bool

-- | @since 0.1
instance UpperBounded Bool

-- | @since 0.1
instance LowerBounded Char

-- | @since 0.1
instance UpperBounded Char

-- | @since 0.1
instance LowerBounded Int

-- | @since 0.1
instance UpperBounded Int

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance LowerBounded Levity

-- | @since 0.1
instance UpperBounded Levity

#endif

-- | @since 0.1
instance LowerBounded VecCount

-- | @since 0.1
instance UpperBounded VecCount

-- | @since 0.1
instance LowerBounded VecElem

-- | @since 0.1
instance UpperBounded VecElem

-- | @since 0.1
instance LowerBounded Word

-- | @since 0.1
instance UpperBounded Word

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (And a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (And a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Iff a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Iff a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Ior a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Ior a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Xor a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Xor a)

#endif

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Identity a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Identity a)

#if !MIN_VERSION_base(4, 15, 0)

-- NB. For GHC < 9, Bounded Down is derived. This means that min/maxBound
-- are not flipped, which is presumably not what we want. However,
-- we want Lower/UpperBounded to agree w/ Bounded wherever applicable,
-- thus we do the same thing here.

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Down a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Down a)

#else

-- For GHC >= 9, Bounded Down correctly flipped

-- | @since 0.1
instance UpperBounded a => LowerBounded (Down a) where
  lowerBound = Down upperBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded a => UpperBounded (Down a) where
  upperBound = Down lowerBound
  {-# INLINE upperBound #-}

#endif

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (First a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (First a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Last a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Last a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Max a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Max a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Min a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Min a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Dual a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Dual a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Product a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Product a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Sum a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Sum a)

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance LowerBounded a => LowerBounded (Solo a) where
  lowerBound = Solo lowerBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance UpperBounded a => UpperBounded (Solo a) where
  upperBound = Solo upperBound
  {-# INLINE upperBound #-}

#endif

-- | @since 0.1
instance LowerBounded (Proxy t)

-- | @since 0.1
instance UpperBounded (Proxy t)

-- | @since 0.1
instance (LowerBounded a, LowerBounded b) => LowerBounded (a, b) where
  lowerBound = (lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (UpperBounded a, UpperBounded b) => UpperBounded (a, b) where
  upperBound = (upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Const a b)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Const a b)

-- | @since 0.1
instance (Applicative f, LowerBounded a) => LowerBounded (Ap f a) where
  lowerBound = Ap (pure lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Applicative f, UpperBounded a) => UpperBounded (Ap f a) where
  upperBound = Ap (pure upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (Coercible a b) => LowerBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => UpperBounded (Coercion a b)

-- | @since 0.1
instance (a ~ b) => LowerBounded (a :~: b) where
  lowerBound = Refl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~ b) => UpperBounded (a :~: b) where
  upperBound = Refl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (a ~~ b) => LowerBounded (a :~~: b) where
  lowerBound = HRefl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~~ b) => UpperBounded (a :~~: b) where
  upperBound = HRefl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (LowerBounded a, LowerBounded b, LowerBounded c) => LowerBounded (a, b, c) where
  lowerBound = (lowerBound, lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (UpperBounded a, UpperBounded b, UpperBounded c) => UpperBounded (a, b, c) where
  upperBound = (upperBound, upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d
  ) =>
  LowerBounded (a, b, c, d)
  where
  lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d
  ) =>
  UpperBounded (a, b, c, d)
  where
  upperBound = (upperBound, upperBound, upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e
  ) =>
  LowerBounded (a, b, c, d, e)
  where
  lowerBound = (lowerBound, lowerBound, lowerBound, lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e
  ) =>
  UpperBounded (a, b, c, d, e)
  where
  upperBound = (upperBound, upperBound, upperBound, upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f
  ) =>
  LowerBounded (a, b, c, d, e, f)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f
  ) =>
  UpperBounded (a, b, c, d, e, f)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g
  ) =>
  LowerBounded (a, b, c, d, e, f, g)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g
  ) =>
  UpperBounded (a, b, c, d, e, f, g)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j,
    LowerBounded k
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j,
    UpperBounded k
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j,
    LowerBounded k,
    LowerBounded l
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j,
    UpperBounded k,
    UpperBounded l
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j,
    LowerBounded k,
    LowerBounded l,
    LowerBounded m
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j,
    UpperBounded k,
    UpperBounded l,
    UpperBounded m
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j,
    LowerBounded k,
    LowerBounded l,
    LowerBounded m,
    LowerBounded n
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j,
    UpperBounded k,
    UpperBounded l,
    UpperBounded m,
    UpperBounded n
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( LowerBounded a,
    LowerBounded b,
    LowerBounded c,
    LowerBounded d,
    LowerBounded e,
    LowerBounded f,
    LowerBounded g,
    LowerBounded h,
    LowerBounded i,
    LowerBounded j,
    LowerBounded k,
    LowerBounded l,
    LowerBounded m,
    LowerBounded n,
    LowerBounded o
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  lowerBound =
    ( lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound,
      lowerBound
    )
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f,
    UpperBounded g,
    UpperBounded h,
    UpperBounded i,
    UpperBounded j,
    UpperBounded k,
    UpperBounded l,
    UpperBounded m,
    UpperBounded n,
    UpperBounded o
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  upperBound =
    ( upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound,
      upperBound
    )
  {-# INLINE upperBound #-}
