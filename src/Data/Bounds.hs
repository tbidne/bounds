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

    -- * MaybeBounded
    MaybeLowerBounded (..),
    MaybeUpperBounded (..),
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
class MaybeLowerBounded a where
  -- | Retrieves the lower bound.
  --
  -- @since 0.1
  maybeLowerBound :: Maybe a
  default maybeLowerBound :: (LowerBounded a) => Maybe a
  maybeLowerBound = Just lowerBound
  {-# INLINE maybeLowerBound #-}

-- | @since 0.1
instance MaybeLowerBounded Integer where
  maybeLowerBound = Nothing
  {-# INLINE maybeLowerBound #-}

-- | @since 0.1
instance MaybeLowerBounded Natural where
  maybeLowerBound = Just 0
  {-# INLINE maybeLowerBound #-}

-- | Class for types that may have an upper bound. The intention is that
-- bounded types (e.g. Int8) return @Just maxBound@ whereas unbounded types
-- (e.g. Integer) return Nothing.
--
-- @since 0.1
class MaybeUpperBounded a where
  -- | Retrieves the upper bound.
  --
  -- @since 0.1
  maybeUpperBound :: Maybe a
  default maybeUpperBound :: (UpperBounded a) => Maybe a
  maybeUpperBound = Just upperBound
  {-# INLINE maybeUpperBound #-}

-- | @since 0.1
instance MaybeUpperBounded Integer where
  maybeUpperBound = Nothing
  {-# INLINE maybeUpperBound #-}

-- | @since 0.1
instance MaybeUpperBounded Natural where
  maybeUpperBound = Nothing
  {-# INLINE maybeUpperBound #-}

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
      MaybeLowerBounded,
      -- | @since 0.1
      MaybeUpperBounded,
      -- | @since 0.1
      LowerBounded,
      -- | @since 0.1
      UpperBounded
    )

--------------------
-- BASE INSTANCES --
--------------------

-- | @since 0.1
instance MaybeLowerBounded All

-- | @since 0.1
instance MaybeUpperBounded All

-- | @since 0.1
instance LowerBounded All

-- | @since 0.1
instance UpperBounded All

-- | @since 0.1
instance LowerBounded Any

-- | @since 0.1
instance UpperBounded Any

-- | @since 0.1
instance MaybeLowerBounded Any

-- | @since 0.1
instance MaybeUpperBounded Any

-- | @since 0.1
instance MaybeLowerBounded CBool

-- | @since 0.1
instance MaybeUpperBounded CBool

-- | @since 0.1
instance LowerBounded CBool

-- | @since 0.1
instance UpperBounded CBool

-- | @since 0.1
instance MaybeLowerBounded CChar

-- | @since 0.1
instance MaybeUpperBounded CChar

-- | @since 0.1
instance LowerBounded CChar

-- | @since 0.1
instance UpperBounded CChar

-- | @since 0.1
instance MaybeLowerBounded CInt

-- | @since 0.1
instance MaybeUpperBounded CInt

-- | @since 0.1
instance LowerBounded CInt

-- | @since 0.1
instance UpperBounded CInt

-- | @since 0.1
instance MaybeLowerBounded CIntMax

-- | @since 0.1
instance MaybeUpperBounded CIntMax

-- | @since 0.1
instance LowerBounded CIntMax

-- | @since 0.1
instance UpperBounded CIntMax

-- | @since 0.1
instance MaybeUpperBounded CIntPtr

-- | @since 0.1
instance MaybeLowerBounded CIntPtr

-- | @since 0.1
instance UpperBounded CIntPtr

-- | @since 0.1
instance LowerBounded CIntPtr

-- | @since 0.1
instance MaybeLowerBounded CLLong

-- | @since 0.1
instance MaybeUpperBounded CLLong

-- | @since 0.1
instance LowerBounded CLLong

-- | @since 0.1
instance UpperBounded CLLong

-- | @since 0.1
instance MaybeLowerBounded CLong

-- | @since 0.1
instance MaybeUpperBounded CLong

-- | @since 0.1
instance LowerBounded CLong

-- | @since 0.1
instance UpperBounded CLong

-- | @since 0.1
instance MaybeLowerBounded CPtrdiff

-- | @since 0.1
instance MaybeUpperBounded CPtrdiff

-- | @since 0.1
instance LowerBounded CPtrdiff

-- | @since 0.1
instance UpperBounded CPtrdiff

-- | @since 0.1
instance MaybeLowerBounded CSChar

-- | @since 0.1
instance MaybeUpperBounded CSChar

-- | @since 0.1
instance LowerBounded CSChar

-- | @since 0.1
instance UpperBounded CSChar

-- | @since 0.1
instance MaybeLowerBounded CShort

-- | @since 0.1
instance MaybeUpperBounded CShort

-- | @since 0.1
instance LowerBounded CShort

-- | @since 0.1
instance UpperBounded CShort

-- | @since 0.1
instance MaybeLowerBounded CSigAtomic

-- | @since 0.1
instance MaybeUpperBounded CSigAtomic

-- | @since 0.1
instance LowerBounded CSigAtomic

-- | @since 0.1
instance UpperBounded CSigAtomic

-- | @since 0.1
instance MaybeLowerBounded CSize

-- | @since 0.1
instance MaybeUpperBounded CSize

-- | @since 0.1
instance LowerBounded CSize

-- | @since 0.1
instance UpperBounded CSize

-- | @since 0.1
instance MaybeLowerBounded CUChar

-- | @since 0.1
instance MaybeUpperBounded CUChar

-- | @since 0.1
instance LowerBounded CUChar

-- | @since 0.1
instance UpperBounded CUChar

-- | @since 0.1
instance MaybeLowerBounded CUInt

-- | @since 0.1
instance MaybeUpperBounded CUInt

-- | @since 0.1
instance LowerBounded CUInt

-- | @since 0.1
instance UpperBounded CUInt

-- | @since 0.1
instance MaybeLowerBounded CUIntMax

-- | @since 0.1
instance MaybeUpperBounded CUIntMax

-- | @since 0.1
instance LowerBounded CUIntMax

-- | @since 0.1
instance UpperBounded CUIntMax

-- | @since 0.1
instance MaybeLowerBounded CUIntPtr

-- | @since 0.1
instance MaybeUpperBounded CUIntPtr

-- | @since 0.1
instance LowerBounded CUIntPtr

-- | @since 0.1
instance UpperBounded CUIntPtr

-- | @since 0.1
instance MaybeLowerBounded CULLong

-- | @since 0.1
instance MaybeUpperBounded CULLong

-- | @since 0.1
instance LowerBounded CULLong

-- | @since 0.1
instance UpperBounded CULLong

-- | @since 0.1
instance MaybeLowerBounded CULong

-- | @since 0.1
instance MaybeUpperBounded CULong

-- | @since 0.1
instance LowerBounded CULong

-- | @since 0.1
instance UpperBounded CULong

-- | @since 0.1
instance MaybeLowerBounded CUShort

-- | @since 0.1
instance MaybeUpperBounded CUShort

-- | @since 0.1
instance LowerBounded CUShort

-- | @since 0.1
instance UpperBounded CUShort

-- | @since 0.1
instance MaybeLowerBounded CWchar

-- | @since 0.1
instance MaybeUpperBounded CWchar

-- | @since 0.1
instance LowerBounded CWchar

-- | @since 0.1
instance UpperBounded CWchar

-- | @since 0.1
instance MaybeLowerBounded IntPtr

-- | @since 0.1
instance MaybeUpperBounded IntPtr

-- | @since 0.1
instance LowerBounded IntPtr

-- | @since 0.1
instance UpperBounded IntPtr

-- | @since 0.1
instance MaybeLowerBounded WordPtr

-- | @since 0.1
instance MaybeUpperBounded WordPtr

-- | @since 0.1
instance LowerBounded WordPtr

-- | @since 0.1
instance UpperBounded WordPtr

-- | @since 0.1
instance MaybeLowerBounded ByteOrder

-- | @since 0.1
instance MaybeUpperBounded ByteOrder

-- | @since 0.1
instance LowerBounded ByteOrder

-- | @since 0.1
instance UpperBounded ByteOrder

-- | @since 0.1
instance MaybeLowerBounded Associativity

-- | @since 0.1
instance MaybeUpperBounded Associativity

-- | @since 0.1
instance LowerBounded Associativity

-- | @since 0.1
instance UpperBounded Associativity

-- | @since 0.1
instance MaybeLowerBounded DecidedStrictness

-- | @since 0.1
instance MaybeUpperBounded DecidedStrictness

-- | @since 0.1
instance LowerBounded DecidedStrictness

-- | @since 0.1
instance UpperBounded DecidedStrictness

-- | @since 0.1
instance MaybeLowerBounded SourceStrictness

-- | @since 0.1
instance MaybeUpperBounded SourceStrictness

-- | @since 0.1
instance LowerBounded SourceStrictness

-- | @since 0.1
instance UpperBounded SourceStrictness

-- | @since 0.1
instance MaybeLowerBounded SourceUnpackedness

-- | @since 0.1
instance MaybeUpperBounded SourceUnpackedness

-- | @since 0.1
instance LowerBounded SourceUnpackedness

-- | @since 0.1
instance UpperBounded SourceUnpackedness

-- | @since 0.1
instance MaybeLowerBounded Int16

-- | @since 0.1
instance MaybeUpperBounded Int16

-- | @since 0.1
instance LowerBounded Int16

-- | @since 0.1
instance UpperBounded Int16

-- | @since 0.1
instance MaybeLowerBounded Int32

-- | @since 0.1
instance MaybeUpperBounded Int32

-- | @since 0.1
instance LowerBounded Int32

-- | @since 0.1
instance UpperBounded Int32

-- | @since 0.1
instance MaybeLowerBounded Int64

-- | @since 0.1
instance MaybeUpperBounded Int64

-- | @since 0.1
instance LowerBounded Int64

-- | @since 0.1
instance UpperBounded Int64

-- | @since 0.1
instance MaybeLowerBounded Int8

-- | @since 0.1
instance MaybeUpperBounded Int8

-- | @since 0.1
instance LowerBounded Int8

-- | @since 0.1
instance UpperBounded Int8

-- | @since 0.1
instance MaybeLowerBounded GeneralCategory

-- | @since 0.1
instance MaybeUpperBounded GeneralCategory

-- | @since 0.1
instance LowerBounded GeneralCategory

-- | @since 0.1
instance UpperBounded GeneralCategory

-- | @since 0.1
instance MaybeLowerBounded Word16

-- | @since 0.1
instance MaybeUpperBounded Word16

-- | @since 0.1
instance LowerBounded Word16

-- | @since 0.1
instance UpperBounded Word16

-- | @since 0.1
instance MaybeLowerBounded Word32

-- | @since 0.1
instance MaybeUpperBounded Word32

-- | @since 0.1
instance LowerBounded Word32

-- | @since 0.1
instance UpperBounded Word32

-- | @since 0.1
instance MaybeLowerBounded Word64

-- | @since 0.1
instance MaybeUpperBounded Word64

-- | @since 0.1
instance LowerBounded Word64

-- | @since 0.1
instance UpperBounded Word64

-- | @since 0.1
instance MaybeLowerBounded Word8

-- | @since 0.1
instance MaybeUpperBounded Word8

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
instance MaybeLowerBounded Posix.CBlkCnt

-- | @since 0.1
instance MaybeUpperBounded Posix.CBlkCnt

-- | @since 0.1
instance LowerBounded Posix.CBlkCnt

-- | @since 0.1
instance UpperBounded Posix.CBlkCnt

-- | @since 0.1
instance MaybeLowerBounded Posix.CBlkSize

-- | @since 0.1
instance MaybeUpperBounded Posix.CBlkSize

-- | @since 0.1
instance LowerBounded Posix.CBlkSize

-- | @since 0.1
instance UpperBounded Posix.CBlkSize

-- | @since 0.1
instance MaybeLowerBounded Posix.CClockId

-- | @since 0.1
instance MaybeUpperBounded Posix.CClockId

-- | @since 0.1
instance LowerBounded Posix.CClockId

-- | @since 0.1
instance UpperBounded Posix.CClockId

-- | @since 0.1
instance MaybeLowerBounded Posix.CDev

-- | @since 0.1
instance MaybeUpperBounded Posix.CDev

-- | @since 0.1
instance LowerBounded Posix.CDev

-- | @since 0.1
instance UpperBounded Posix.CDev

-- | @since 0.1
instance MaybeLowerBounded Posix.CFsBlkCnt

-- | @since 0.1
instance MaybeUpperBounded Posix.CFsBlkCnt

-- | @since 0.1
instance LowerBounded Posix.CFsBlkCnt

-- | @since 0.1
instance UpperBounded Posix.CFsBlkCnt

-- | @since 0.1
instance MaybeLowerBounded Posix.CFsFilCnt

-- | @since 0.1
instance MaybeUpperBounded Posix.CFsFilCnt

-- | @since 0.1
instance LowerBounded Posix.CFsFilCnt

-- | @since 0.1
instance UpperBounded Posix.CFsFilCnt

-- | @since 0.1
instance MaybeLowerBounded Posix.CGid

-- | @since 0.1
instance MaybeUpperBounded Posix.CGid

-- | @since 0.1
instance LowerBounded Posix.CGid

-- | @since 0.1
instance UpperBounded Posix.CGid

-- | @since 0.1
instance MaybeLowerBounded Posix.CId

-- | @since 0.1
instance MaybeUpperBounded Posix.CId

-- | @since 0.1
instance LowerBounded Posix.CId

-- | @since 0.1
instance UpperBounded Posix.CId

-- | @since 0.1
instance MaybeLowerBounded Posix.CIno

-- | @since 0.1
instance MaybeUpperBounded Posix.CIno

-- | @since 0.1
instance LowerBounded Posix.CIno

-- | @since 0.1
instance UpperBounded Posix.CIno

-- | @since 0.1
instance MaybeLowerBounded Posix.CKey

-- | @since 0.1
instance MaybeUpperBounded Posix.CKey

-- | @since 0.1
instance LowerBounded Posix.CKey

-- | @since 0.1
instance UpperBounded Posix.CKey

-- | @since 0.1
instance MaybeLowerBounded Posix.CMode

-- | @since 0.1
instance MaybeUpperBounded Posix.CMode

-- | @since 0.1
instance LowerBounded Posix.CMode

-- | @since 0.1
instance UpperBounded Posix.CMode

-- | @since 0.1
instance MaybeLowerBounded Posix.CNfds

-- | @since 0.1
instance MaybeUpperBounded Posix.CNfds

-- | @since 0.1
instance LowerBounded Posix.CNfds

-- | @since 0.1
instance UpperBounded Posix.CNfds

-- | @since 0.1
instance MaybeLowerBounded Posix.CNlink

-- | @since 0.1
instance MaybeUpperBounded Posix.CNlink

-- | @since 0.1
instance LowerBounded Posix.CNlink

-- | @since 0.1
instance UpperBounded Posix.CNlink

-- | @since 0.1
instance MaybeLowerBounded Posix.COff

-- | @since 0.1
instance MaybeUpperBounded Posix.COff

-- | @since 0.1
instance LowerBounded Posix.COff

-- | @since 0.1
instance UpperBounded Posix.COff

-- | @since 0.1
instance MaybeLowerBounded Posix.CPid

-- | @since 0.1
instance MaybeUpperBounded Posix.CPid

-- | @since 0.1
instance LowerBounded Posix.CPid

-- | @since 0.1
instance UpperBounded Posix.CPid

-- | @since 0.1
instance MaybeLowerBounded Posix.CRLim

-- | @since 0.1
instance MaybeUpperBounded Posix.CRLim

-- | @since 0.1
instance LowerBounded Posix.CRLim

-- | @since 0.1
instance UpperBounded Posix.CRLim

-- | @since 0.1
instance MaybeLowerBounded Posix.CSsize

-- | @since 0.1
instance MaybeUpperBounded Posix.CSsize

-- | @since 0.1
instance LowerBounded Posix.CSsize

-- | @since 0.1
instance UpperBounded Posix.CSsize

-- | @since 0.1
instance MaybeLowerBounded Posix.CTcflag

-- | @since 0.1
instance MaybeUpperBounded Posix.CTcflag

-- | @since 0.1
instance LowerBounded Posix.CTcflag

-- | @since 0.1
instance UpperBounded Posix.CTcflag

-- | @since 0.1
instance MaybeLowerBounded Posix.CUid

-- | @since 0.1
instance MaybeUpperBounded Posix.CUid

-- | @since 0.1
instance LowerBounded Posix.CUid

-- | @since 0.1
instance UpperBounded Posix.CUid
#endif

-- | @since 0.1
instance MaybeLowerBounded Posix.Fd

-- | @since 0.1
instance MaybeUpperBounded Posix.Fd

-- | @since 0.1
instance LowerBounded Posix.Fd

-- | @since 0.1
instance UpperBounded Posix.Fd

-- | @since 0.1
instance MaybeLowerBounded Ordering

-- | @since 0.1
instance MaybeUpperBounded Ordering

-- | @since 0.1
instance LowerBounded Ordering

-- | @since 0.1
instance UpperBounded Ordering

-- | @since 0.1
instance MaybeLowerBounded ()

-- | @since 0.1
instance MaybeUpperBounded ()

-- | @since 0.1
instance LowerBounded ()

-- | @since 0.1
instance UpperBounded ()

-- | @since 0.1
instance MaybeLowerBounded Bool

-- | @since 0.1
instance MaybeUpperBounded Bool

-- | @since 0.1
instance LowerBounded Bool

-- | @since 0.1
instance UpperBounded Bool

-- | @since 0.1
instance MaybeLowerBounded Char

-- | @since 0.1
instance MaybeUpperBounded Char

-- | @since 0.1
instance LowerBounded Char

-- | @since 0.1
instance UpperBounded Char

-- | @since 0.1
instance MaybeLowerBounded Int

-- | @since 0.1
instance MaybeUpperBounded Int

-- | @since 0.1
instance LowerBounded Int

-- | @since 0.1
instance UpperBounded Int

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance MaybeLowerBounded Levity

-- | @since 0.1
instance MaybeUpperBounded Levity

-- | @since 0.1
instance LowerBounded Levity

-- | @since 0.1
instance UpperBounded Levity

#endif

-- | @since 0.1
instance MaybeLowerBounded VecCount

-- | @since 0.1
instance MaybeUpperBounded VecCount

-- | @since 0.1
instance LowerBounded VecCount

-- | @since 0.1
instance UpperBounded VecCount

-- | @since 0.1
instance MaybeLowerBounded VecElem

-- | @since 0.1
instance MaybeUpperBounded VecElem

-- | @since 0.1
instance LowerBounded VecElem

-- | @since 0.1
instance UpperBounded VecElem

-- | @since 0.1
instance MaybeLowerBounded Word

-- | @since 0.1
instance MaybeUpperBounded Word

-- | @since 0.1
instance LowerBounded Word

-- | @since 0.1
instance UpperBounded Word

#if MIN_VERSION_base(4, 16, 0)

-- | @since 0.1
instance LowerBounded a => MaybeLowerBounded (And a)

-- | @since 0.1
instance UpperBounded a => MaybeUpperBounded (And a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (And a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (And a)

-- | @since 0.1
instance LowerBounded a => MaybeLowerBounded (Iff a)

-- | @since 0.1
instance UpperBounded a => MaybeUpperBounded (Iff a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Iff a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Iff a)

-- | @since 0.1
instance LowerBounded a => MaybeLowerBounded (Ior a)

-- | @since 0.1
instance UpperBounded a => MaybeUpperBounded (Ior a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Ior a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Ior a)

-- | @since 0.1
instance LowerBounded a => MaybeLowerBounded (Xor a)

-- | @since 0.1
instance UpperBounded a => MaybeUpperBounded (Xor a)

-- | @since 0.1
deriving via a instance LowerBounded a => LowerBounded (Xor a)

-- | @since 0.1
deriving via a instance UpperBounded a => UpperBounded (Xor a)

#endif

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Identity a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Identity a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Identity a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Identity a)

#if !MIN_VERSION_base(4, 15, 0)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Down a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Down a)

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
instance (UpperBounded a) => MaybeLowerBounded (Down a)

-- | @since 0.1
instance (LowerBounded a) => MaybeUpperBounded (Down a)

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
instance (LowerBounded a) => MaybeLowerBounded (First a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (First a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (First a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (First a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Last a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Last a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Last a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Last a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Max a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Max a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Max a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Max a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Min a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Min a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Min a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Min a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (WrappedMonoid a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (WrappedMonoid a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (WrappedMonoid a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Dual a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Dual a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Dual a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Dual a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Product a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Product a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Product a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Product a)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Sum a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Sum a)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Sum a)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Sum a)

#if MIN_VERSION_base(4, 18, 0)

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Solo a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Solo a)

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
instance (LowerBounded a) => MaybeLowerBounded (Solo a)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Solo a)

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
instance MaybeLowerBounded (Proxy t)

-- | @since 0.1
instance MaybeUpperBounded (Proxy t)

-- | @since 0.1
instance (LowerBounded a, LowerBounded b) => MaybeLowerBounded (a, b)

-- | @since 0.1
instance (UpperBounded a, UpperBounded b) => MaybeUpperBounded (a, b)

-- | @since 0.1
instance (LowerBounded a, LowerBounded b) => LowerBounded (a, b) where
  lowerBound = (lowerBound, lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (UpperBounded a, UpperBounded b) => UpperBounded (a, b) where
  upperBound = (upperBound, upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (LowerBounded a) => MaybeLowerBounded (Const a b)

-- | @since 0.1
instance (UpperBounded a) => MaybeUpperBounded (Const a b)

-- | @since 0.1
deriving via a instance (LowerBounded a) => LowerBounded (Const a b)

-- | @since 0.1
deriving via a instance (UpperBounded a) => UpperBounded (Const a b)

-- | @since 0.1
instance (Applicative f, LowerBounded a) => MaybeLowerBounded (Ap f a)

-- | @since 0.1
instance (Applicative f, UpperBounded a) => MaybeUpperBounded (Ap f a)

-- | @since 0.1
instance (Applicative f, LowerBounded a) => LowerBounded (Ap f a) where
  lowerBound = Ap (pure lowerBound)
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Applicative f, UpperBounded a) => UpperBounded (Ap f a) where
  upperBound = Ap (pure upperBound)
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (Coercible a b) => MaybeLowerBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => MaybeUpperBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => LowerBounded (Coercion a b)

-- | @since 0.1
instance (Coercible a b) => UpperBounded (Coercion a b)

-- | @since 0.1
instance (a ~ b) => MaybeLowerBounded (a :~: b)

-- | @since 0.1
instance (a ~ b) => MaybeUpperBounded (a :~: b)

-- | @since 0.1
instance (a ~ b) => LowerBounded (a :~: b) where
  lowerBound = Refl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~ b) => UpperBounded (a :~: b) where
  upperBound = Refl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (a ~~ b) => MaybeLowerBounded (a :~~: b)

-- | @since 0.1
instance (a ~~ b) => MaybeUpperBounded (a :~~: b)

-- | @since 0.1
instance (a ~~ b) => LowerBounded (a :~~: b) where
  lowerBound = HRefl
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (a ~~ b) => UpperBounded (a :~~: b) where
  upperBound = HRefl
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (LowerBounded a, LowerBounded b, LowerBounded c) => MaybeLowerBounded (a, b, c)

-- | @since 0.1
instance (UpperBounded a, UpperBounded b, UpperBounded c) => MaybeUpperBounded (a, b, c)

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
  MaybeLowerBounded (a, b, c, d)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d
  ) =>
  MaybeUpperBounded (a, b, c, d)

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
  MaybeLowerBounded (a, b, c, d, e)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e
  ) =>
  MaybeUpperBounded (a, b, c, d, e)

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
  MaybeLowerBounded (a, b, c, d, e, f)

-- | @since 0.1
instance
  ( UpperBounded a,
    UpperBounded b,
    UpperBounded c,
    UpperBounded d,
    UpperBounded e,
    UpperBounded f
  ) =>
  MaybeUpperBounded (a, b, c, d, e, f)

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
  MaybeLowerBounded (a, b, c, d, e, f, g)

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
  MaybeUpperBounded (a, b, c, d, e, f, g)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j, k)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j, k)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

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
  MaybeLowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

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
  MaybeUpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

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
