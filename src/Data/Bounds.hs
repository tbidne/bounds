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

    -- * AnyBounded
    AnyLowerBounded (..),
    AnyUpperBounded (..),
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
#if MIN_VERSION_base(4, 18, 0)
import Data.Tuple (Solo (MkSolo))
#elif MIN_VERSION_base(4, 16, 0)
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
    SourceUnpackedness,
  )
import GHC.Natural (Natural)
import System.Posix.Types qualified as Posix

-- | Class for types that may have a lower bound. The intention is that
-- bounded types (e.g. Int8) return @Just minBound@ whereas unbounded types
-- (e.g. Integer) return Nothing.
--
-- @since 0.1
class AnyLowerBounded a where
  -- | Retrieves the lower bound.
  --
  -- @since 0.1
  someLowerBound :: Maybe a
  default someLowerBound :: (LowerBounded a) => Maybe a
  someLowerBound = Just lowerBound
  {-# INLINE someLowerBound #-}

-- | @since 0.1
instance AnyLowerBounded Integer where
  someLowerBound = Nothing
  {-# INLINE someLowerBound #-}

-- | @since 0.1
instance AnyLowerBounded Natural where
  someLowerBound = Just 0
  {-# INLINE someLowerBound #-}

-- | Class for types that may have an upper bound. The intention is that
-- bounded types (e.g. Int8) return @Just maxBound@ whereas unbounded types
-- (e.g. Integer) return Nothing.
--
-- @since 0.1
class AnyUpperBounded a where
  -- | Retrieves the upper bound.
  --
  -- @since 0.1
  someUpperBound :: Maybe a
  default someUpperBound :: (UpperBounded a) => Maybe a
  someUpperBound = Just upperBound
  {-# INLINE someUpperBound #-}

-- | @since 0.1
instance AnyUpperBounded Integer where
  someUpperBound = Nothing
  {-# INLINE someUpperBound #-}

-- | @since 0.1
instance AnyUpperBounded Natural where
  someUpperBound = Nothing
  {-# INLINE someUpperBound #-}

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
  {-# INLINE lowerBound #-}

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
  {-# INLINE upperBound #-}

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
      AnyLowerBounded,
      -- | @since 0.1
      AnyUpperBounded,
      -- | @since 0.1
      LowerBounded,
      -- | @since 0.1
      UpperBounded
    )

--------------------
-- BASE INSTANCES --
--------------------

-- | @since 0.1
instance AnyLowerBounded All

-- | @since 0.1
instance AnyUpperBounded All

-- | @since 0.1
instance LowerBounded All

-- | @since 0.1
instance UpperBounded All

-- | @since 0.1
instance LowerBounded Any

-- | @since 0.1
instance UpperBounded Any

-- | @since 0.1
instance AnyLowerBounded Any

-- | @since 0.1
instance AnyUpperBounded Any

-- | @since 0.1
instance AnyLowerBounded CBool

-- | @since 0.1
instance AnyUpperBounded CBool

-- | @since 0.1
instance LowerBounded CBool

-- | @since 0.1
instance UpperBounded CBool

-- | @since 0.1
instance AnyLowerBounded CChar

-- | @since 0.1
instance AnyUpperBounded CChar

-- | @since 0.1
instance LowerBounded CChar

-- | @since 0.1
instance UpperBounded CChar

-- | @since 0.1
instance AnyLowerBounded CInt

-- | @since 0.1
instance AnyUpperBounded CInt

-- | @since 0.1
instance LowerBounded CInt

-- | @since 0.1
instance UpperBounded CInt

-- | @since 0.1
instance AnyLowerBounded CIntMax

-- | @since 0.1
instance AnyUpperBounded CIntMax

-- | @since 0.1
instance LowerBounded CIntMax

-- | @since 0.1
instance UpperBounded CIntMax

-- | @since 0.1
instance AnyUpperBounded CIntPtr

-- | @since 0.1
instance AnyLowerBounded CIntPtr

-- | @since 0.1
instance UpperBounded CIntPtr

-- | @since 0.1
instance LowerBounded CIntPtr

-- | @since 0.1
instance AnyLowerBounded CLLong

-- | @since 0.1
instance AnyUpperBounded CLLong

-- | @since 0.1
instance LowerBounded CLLong

-- | @since 0.1
instance UpperBounded CLLong

-- | @since 0.1
instance AnyLowerBounded CLong

-- | @since 0.1
instance AnyUpperBounded CLong

-- | @since 0.1
instance LowerBounded CLong

-- | @since 0.1
instance UpperBounded CLong

-- | @since 0.1
instance AnyLowerBounded CPtrdiff

-- | @since 0.1
instance AnyUpperBounded CPtrdiff

-- | @since 0.1
instance LowerBounded CPtrdiff

-- | @since 0.1
instance UpperBounded CPtrdiff

-- | @since 0.1
instance AnyLowerBounded CSChar

-- | @since 0.1
instance AnyUpperBounded CSChar

-- | @since 0.1
instance LowerBounded CSChar

-- | @since 0.1
instance UpperBounded CSChar

-- | @since 0.1
instance AnyLowerBounded CShort

-- | @since 0.1
instance AnyUpperBounded CShort

-- | @since 0.1
instance LowerBounded CShort

-- | @since 0.1
instance UpperBounded CShort

-- | @since 0.1
instance AnyLowerBounded CSigAtomic

-- | @since 0.1
instance AnyUpperBounded CSigAtomic

-- | @since 0.1
instance LowerBounded CSigAtomic

-- | @since 0.1
instance UpperBounded CSigAtomic

-- | @since 0.1
instance AnyLowerBounded CSize

-- | @since 0.1
instance AnyUpperBounded CSize

-- | @since 0.1
instance LowerBounded CSize

-- | @since 0.1
instance UpperBounded CSize

-- | @since 0.1
instance AnyLowerBounded CUChar

-- | @since 0.1
instance AnyUpperBounded CUChar

-- | @since 0.1
instance LowerBounded CUChar

-- | @since 0.1
instance UpperBounded CUChar

-- | @since 0.1
instance AnyLowerBounded CUInt

-- | @since 0.1
instance AnyUpperBounded CUInt

-- | @since 0.1
instance LowerBounded CUInt

-- | @since 0.1
instance UpperBounded CUInt

-- | @since 0.1
instance AnyLowerBounded CUIntMax

-- | @since 0.1
instance AnyUpperBounded CUIntMax

-- | @since 0.1
instance LowerBounded CUIntMax

-- | @since 0.1
instance UpperBounded CUIntMax

-- | @since 0.1
instance AnyLowerBounded CUIntPtr

-- | @since 0.1
instance AnyUpperBounded CUIntPtr

-- | @since 0.1
instance LowerBounded CUIntPtr

-- | @since 0.1
instance UpperBounded CUIntPtr

-- | @since 0.1
instance AnyLowerBounded CULLong

-- | @since 0.1
instance AnyUpperBounded CULLong

-- | @since 0.1
instance LowerBounded CULLong

-- | @since 0.1
instance UpperBounded CULLong

-- | @since 0.1
instance AnyLowerBounded CULong

-- | @since 0.1
instance AnyUpperBounded CULong

-- | @since 0.1
instance LowerBounded CULong

-- | @since 0.1
instance UpperBounded CULong

-- | @since 0.1
instance AnyLowerBounded CUShort

-- | @since 0.1
instance AnyUpperBounded CUShort

-- | @since 0.1
instance LowerBounded CUShort

-- | @since 0.1
instance UpperBounded CUShort

-- | @since 0.1
instance AnyLowerBounded CWchar

-- | @since 0.1
instance AnyUpperBounded CWchar

-- | @since 0.1
instance LowerBounded CWchar

-- | @since 0.1
instance UpperBounded CWchar

-- | @since 0.1
instance AnyLowerBounded IntPtr

-- | @since 0.1
instance AnyUpperBounded IntPtr

-- | @since 0.1
instance LowerBounded IntPtr

-- | @since 0.1
instance UpperBounded IntPtr

-- | @since 0.1
instance AnyLowerBounded WordPtr

-- | @since 0.1
instance AnyUpperBounded WordPtr

-- | @since 0.1
instance LowerBounded WordPtr

-- | @since 0.1
instance UpperBounded WordPtr

-- | @since 0.1
instance AnyLowerBounded ByteOrder

-- | @since 0.1
instance AnyUpperBounded ByteOrder

-- | @since 0.1
instance LowerBounded ByteOrder

-- | @since 0.1
instance UpperBounded ByteOrder

-- | @since 0.1
instance AnyLowerBounded Associativity

-- | @since 0.1
instance AnyUpperBounded Associativity

-- | @since 0.1
instance LowerBounded Associativity

-- | @since 0.1
instance UpperBounded Associativity

-- | @since 0.1
instance AnyLowerBounded DecidedStrictness

-- | @since 0.1
instance AnyUpperBounded DecidedStrictness

-- | @since 0.1
instance LowerBounded DecidedStrictness

-- | @since 0.1
instance UpperBounded DecidedStrictness

-- | @since 0.1
instance AnyLowerBounded SourceStrictness

-- | @since 0.1
instance AnyUpperBounded SourceStrictness

-- | @since 0.1
instance LowerBounded SourceStrictness

-- | @since 0.1
instance UpperBounded SourceStrictness

-- | @since 0.1
instance AnyLowerBounded SourceUnpackedness

-- | @since 0.1
instance AnyUpperBounded SourceUnpackedness

-- | @since 0.1
instance LowerBounded SourceUnpackedness

-- | @since 0.1
instance UpperBounded SourceUnpackedness

-- | @since 0.1
instance AnyLowerBounded Int16

-- | @since 0.1
instance AnyUpperBounded Int16

-- | @since 0.1
instance LowerBounded Int16

-- | @since 0.1
instance UpperBounded Int16

-- | @since 0.1
instance AnyLowerBounded Int32

-- | @since 0.1
instance AnyUpperBounded Int32

-- | @since 0.1
instance LowerBounded Int32

-- | @since 0.1
instance UpperBounded Int32

-- | @since 0.1
instance AnyLowerBounded Int64

-- | @since 0.1
instance AnyUpperBounded Int64

-- | @since 0.1
instance LowerBounded Int64

-- | @since 0.1
instance UpperBounded Int64

-- | @since 0.1
instance AnyLowerBounded Int8

-- | @since 0.1
instance AnyUpperBounded Int8

-- | @since 0.1
instance LowerBounded Int8

-- | @since 0.1
instance UpperBounded Int8

-- | @since 0.1
instance AnyLowerBounded GeneralCategory

-- | @since 0.1
instance AnyUpperBounded GeneralCategory

-- | @since 0.1
instance LowerBounded GeneralCategory

-- | @since 0.1
instance UpperBounded GeneralCategory

-- | @since 0.1
instance AnyLowerBounded Word16

-- | @since 0.1
instance AnyUpperBounded Word16

-- | @since 0.1
instance LowerBounded Word16

-- | @since 0.1
instance UpperBounded Word16

-- | @since 0.1
instance AnyLowerBounded Word32

-- | @since 0.1
instance AnyUpperBounded Word32

-- | @since 0.1
instance LowerBounded Word32

-- | @since 0.1
instance UpperBounded Word32

-- | @since 0.1
instance AnyLowerBounded Word64

-- | @since 0.1
instance AnyUpperBounded Word64

-- | @since 0.1
instance LowerBounded Word64

-- | @since 0.1
instance UpperBounded Word64

-- | @since 0.1
instance AnyLowerBounded Word8

-- | @since 0.1
instance AnyUpperBounded Word8

-- | @since 0.1
instance LowerBounded Word8

-- | @since 0.1
instance UpperBounded Word8

-- NOTE: GHC hides these types in System.Posix.Types behind flags that are
-- specific to each type e.g. CBlkCnt is behind HTYPE_BLKCNT_T.
-- These didn't appear to work here, so we just use a windows/no-windows check
-- and hope that is good enough.

#if !WINDOWS
-- | @since 0.1
instance AnyLowerBounded Posix.CBlkCnt

-- | @since 0.1
instance AnyUpperBounded Posix.CBlkCnt

-- | @since 0.1
instance LowerBounded Posix.CBlkCnt

-- | @since 0.1
instance UpperBounded Posix.CBlkCnt

-- | @since 0.1
instance AnyLowerBounded Posix.CBlkSize

-- | @since 0.1
instance AnyUpperBounded Posix.CBlkSize

-- | @since 0.1
instance LowerBounded Posix.CBlkSize

-- | @since 0.1
instance UpperBounded Posix.CBlkSize

-- | @since 0.1
instance AnyLowerBounded Posix.CClockId

-- | @since 0.1
instance AnyUpperBounded Posix.CClockId

-- | @since 0.1
instance LowerBounded Posix.CClockId

-- | @since 0.1
instance UpperBounded Posix.CClockId

-- | @since 0.1
instance AnyLowerBounded Posix.CDev

-- | @since 0.1
instance AnyUpperBounded Posix.CDev

-- | @since 0.1
instance LowerBounded Posix.CDev

-- | @since 0.1
instance UpperBounded Posix.CDev

-- | @since 0.1
instance AnyLowerBounded Posix.CFsBlkCnt

-- | @since 0.1
instance AnyUpperBounded Posix.CFsBlkCnt

-- | @since 0.1
instance LowerBounded Posix.CFsBlkCnt

-- | @since 0.1
instance UpperBounded Posix.CFsBlkCnt

-- | @since 0.1
instance AnyLowerBounded Posix.CFsFilCnt

-- | @since 0.1
instance AnyUpperBounded Posix.CFsFilCnt

-- | @since 0.1
instance LowerBounded Posix.CFsFilCnt

-- | @since 0.1
instance UpperBounded Posix.CFsFilCnt

-- | @since 0.1
instance AnyLowerBounded Posix.CGid

-- | @since 0.1
instance AnyUpperBounded Posix.CGid

-- | @since 0.1
instance LowerBounded Posix.CGid

-- | @since 0.1
instance UpperBounded Posix.CGid

-- | @since 0.1
instance AnyLowerBounded Posix.CId

-- | @since 0.1
instance AnyUpperBounded Posix.CId

-- | @since 0.1
instance LowerBounded Posix.CId

-- | @since 0.1
instance UpperBounded Posix.CId

-- | @since 0.1
instance AnyLowerBounded Posix.CIno

-- | @since 0.1
instance AnyUpperBounded Posix.CIno

-- | @since 0.1
instance LowerBounded Posix.CIno

-- | @since 0.1
instance UpperBounded Posix.CIno

-- | @since 0.1
instance AnyLowerBounded Posix.CKey

-- | @since 0.1
instance AnyUpperBounded Posix.CKey

-- | @since 0.1
instance LowerBounded Posix.CKey

-- | @since 0.1
instance UpperBounded Posix.CKey

-- | @since 0.1
instance AnyLowerBounded Posix.CMode

-- | @since 0.1
instance AnyUpperBounded Posix.CMode

-- | @since 0.1
instance LowerBounded Posix.CMode

-- | @since 0.1
instance UpperBounded Posix.CMode

-- | @since 0.1
instance AnyLowerBounded Posix.CNfds

-- | @since 0.1
instance AnyUpperBounded Posix.CNfds

-- | @since 0.1
instance LowerBounded Posix.CNfds

-- | @since 0.1
instance UpperBounded Posix.CNfds

-- | @since 0.1
instance AnyLowerBounded Posix.CNlink

-- | @since 0.1
instance AnyUpperBounded Posix.CNlink

-- | @since 0.1
instance LowerBounded Posix.CNlink

-- | @since 0.1
instance UpperBounded Posix.CNlink

-- | @since 0.1
instance AnyLowerBounded Posix.COff

-- | @since 0.1
instance AnyUpperBounded Posix.COff

-- | @since 0.1
instance LowerBounded Posix.COff

-- | @since 0.1
instance UpperBounded Posix.COff

-- | @since 0.1
instance AnyLowerBounded Posix.CPid

-- | @since 0.1
instance AnyUpperBounded Posix.CPid

-- | @since 0.1
instance LowerBounded Posix.CPid

-- | @since 0.1
instance UpperBounded Posix.CPid

-- | @since 0.1
instance AnyLowerBounded Posix.CRLim

-- | @since 0.1
instance AnyUpperBounded Posix.CRLim

-- | @since 0.1
instance LowerBounded Posix.CRLim

-- | @since 0.1
instance UpperBounded Posix.CRLim

-- | @since 0.1
instance AnyLowerBounded Posix.CSsize

-- | @since 0.1
instance AnyUpperBounded Posix.CSsize

-- | @since 0.1
instance LowerBounded Posix.CSsize

-- | @since 0.1
instance UpperBounded Posix.CSsize

-- | @since 0.1
instance AnyLowerBounded Posix.CTcflag

-- | @since 0.1
instance AnyUpperBounded Posix.CTcflag

-- | @since 0.1
instance LowerBounded Posix.CTcflag

-- | @since 0.1
instance UpperBounded Posix.CTcflag

-- | @since 0.1
instance AnyLowerBounded Posix.CUid

-- | @since 0.1
instance AnyUpperBounded Posix.CUid

-- | @since 0.1
instance LowerBounded Posix.CUid

-- | @since 0.1
instance UpperBounded Posix.CUid
#endif

-- | @since 0.1
instance AnyLowerBounded Posix.Fd

-- | @since 0.1
instance AnyUpperBounded Posix.Fd

-- | @since 0.1
instance LowerBounded Posix.Fd

-- | @since 0.1
instance UpperBounded Posix.Fd

-- | @since 0.1
instance AnyLowerBounded Ordering

-- | @since 0.1
instance AnyUpperBounded Ordering

-- | @since 0.1
instance LowerBounded Ordering

-- | @since 0.1
instance UpperBounded Ordering

-- | @since 0.1
instance AnyLowerBounded ()

-- | @since 0.1
instance AnyUpperBounded ()

-- | @since 0.1
instance LowerBounded ()

-- | @since 0.1
instance UpperBounded ()

-- | @since 0.1
instance AnyLowerBounded Bool

-- | @since 0.1
instance AnyUpperBounded Bool

-- | @since 0.1
instance LowerBounded Bool

-- | @since 0.1
instance UpperBounded Bool

-- | @since 0.1
instance AnyLowerBounded Char

-- | @since 0.1
instance AnyUpperBounded Char

-- | @since 0.1
instance LowerBounded Char

-- | @since 0.1
instance UpperBounded Char

-- | @since 0.1
instance AnyLowerBounded Int

-- | @since 0.1
instance AnyUpperBounded Int

-- | @since 0.1
instance LowerBounded Int

-- | @since 0.1
instance UpperBounded Int

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance AnyLowerBounded Levity

-- | @since 0.1
instance AnyUpperBounded Levity

-- | @since 0.1
instance LowerBounded Levity

-- | @since 0.1
instance UpperBounded Levity

#endif

-- | @since 0.1
instance AnyLowerBounded VecCount

-- | @since 0.1
instance AnyUpperBounded VecCount

-- | @since 0.1
instance LowerBounded VecCount

-- | @since 0.1
instance UpperBounded VecCount

-- | @since 0.1
instance AnyLowerBounded VecElem

-- | @since 0.1
instance AnyUpperBounded VecElem

-- | @since 0.1
instance LowerBounded VecElem

-- | @since 0.1
instance UpperBounded VecElem

-- | @since 0.1
instance AnyLowerBounded Word

-- | @since 0.1
instance AnyUpperBounded Word

-- | @since 0.1
instance LowerBounded Word

-- | @since 0.1
instance UpperBounded Word

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance LowerBounded a => AnyLowerBounded (And a)

-- | @since 0.1
instance UpperBounded a => AnyUpperBounded (And a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (And a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (And a)

-- | @since 0.1
instance LowerBounded a => AnyLowerBounded (Iff a)

-- | @since 0.1
instance UpperBounded a => AnyUpperBounded (Iff a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Iff a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Iff a)

-- | @since 0.1
instance LowerBounded a => AnyLowerBounded (Ior a)

-- | @since 0.1
instance UpperBounded a => AnyUpperBounded (Ior a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Ior a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Ior a)

-- | @since 0.1
instance LowerBounded a => AnyLowerBounded (Xor a)

-- | @since 0.1
instance UpperBounded a => AnyUpperBounded (Xor a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Xor a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Xor a)

#endif

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Identity a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Identity a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Identity a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Identity a)

#if !MIN_VERSION_base(4, 15, 0)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Down a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Down a)

-- NB. For GHC < 9, Bounded Down is derived. This means that min/maxBound
-- are not flipped, which is presumably not what we want. However,
-- we want Lower/UpperBounded to agree w/ Bounded wherever applicable,
-- thus we do the same thing here.

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Down a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Down a)

#else

-- | @since 0.1
instance (UpperBounded a) => AnyLowerBounded (Down a)

-- | @since 0.1
instance (LowerBounded a) => AnyUpperBounded (Down a)

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
instance (LowerBounded a) => AnyLowerBounded (First a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (First a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (First a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (First a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Last a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Last a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Last a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Last a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Max a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Max a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Max a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Max a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Min a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Min a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Min a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Min a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (WrappedMonoid a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (WrappedMonoid a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Dual a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Dual a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Dual a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Dual a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Product a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Product a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Product a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Product a)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Sum a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Sum a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Sum a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Sum a)

#if MIN_VERSION_base(4, 18, 0)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Solo a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Solo a)

-- | @since 0.1
instance LowerBounded a => LowerBounded (Solo a) where
  lowerBound = MkSolo lowerBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance UpperBounded a => UpperBounded (Solo a) where
  upperBound = MkSolo upperBound
  {-# INLINE upperBound #-}

#elif MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Solo a)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Solo a)

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
instance AnyLowerBounded (Proxy t)

-- | @since 0.1
instance AnyUpperBounded (Proxy t)

-- | @since 0.1
instance (LowerBounded a, LowerBounded b) => AnyLowerBounded (a, b)

-- | @since 0.1
instance (UpperBounded a, UpperBounded b) => AnyUpperBounded (a, b)

-- | @since 0.1
instance (LowerBounded a, LowerBounded b) => LowerBounded (a, b) where
  lowerBound = (lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (UpperBounded a, UpperBounded b) => UpperBounded (a, b) where
  upperBound = (upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (LowerBounded a) => AnyLowerBounded (Const a b)

-- | @since 0.1
instance (UpperBounded a) => AnyUpperBounded (Const a b)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Const a b)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Const a b)

-- | @since 0.1
instance (Applicative f, LowerBounded a) => AnyLowerBounded (Ap f a)

-- | @since 0.1
instance (Applicative f, UpperBounded a) => AnyUpperBounded (Ap f a)

-- | @since 0.1
instance (Applicative f, LowerBounded a) => LowerBounded (Ap f a) where
  lowerBound = Ap (pure lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Applicative f, UpperBounded a) => UpperBounded (Ap f a) where
  upperBound = Ap (pure upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (Coercible a b) => AnyLowerBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => AnyUpperBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => LowerBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => UpperBounded (Coercion a b)

-- | @since 0.1
instance (a ~ b) => AnyLowerBounded (a :~: b)

-- | @since 0.1
instance (a ~ b) => AnyUpperBounded (a :~: b)

-- | @since 0.1
instance (a ~ b) => LowerBounded (a :~: b) where
  lowerBound = Refl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~ b) => UpperBounded (a :~: b) where
  upperBound = Refl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (a ~~ b) => AnyLowerBounded (a :~~: b)

-- | @since 0.1
instance (a ~~ b) => AnyUpperBounded (a :~~: b)

-- | @since 0.1
instance (a ~~ b) => LowerBounded (a :~~: b) where
  lowerBound = HRefl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~~ b) => UpperBounded (a :~~: b) where
  upperBound = HRefl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (LowerBounded a, LowerBounded b, LowerBounded c) => AnyLowerBounded (a, b, c)

-- | @since 0.1
instance (UpperBounded a, UpperBounded b, UpperBounded c) => AnyUpperBounded (a, b, c)

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
  AnyLowerBounded (a, b, c, d)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d
  ) =>
  AnyUpperBounded (a, b, c, d)

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
  AnyLowerBounded (a, b, c, d, e)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e
  ) =>
  AnyUpperBounded (a, b, c, d, e)

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
  AnyLowerBounded (a, b, c, d, e, f)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f
  ) =>
  AnyUpperBounded (a, b, c, d, e, f)

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
  AnyLowerBounded (a, b, c, d, e, f, g)

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
  AnyUpperBounded (a, b, c, d, e, f, g)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j, k)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j, k)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

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
  AnyLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

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
  AnyUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

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
