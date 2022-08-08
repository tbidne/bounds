-- | Provides various typeclasses for dealing with bounded and unbounded
-- types.
module Data.Bounds
  ( -- * Bounded
    UpperBounded (..),
    LowerBounded (..),
    Bounds (..),

    -- * Boundless
    UpperBoundless,
    LowerBoundless,
  )
where

import Control.Applicative (Const)
import Data.Bits (And, Iff, Ior, Xor)
import Data.Char (GeneralCategory)
import Data.Coerce (Coercible)
import Data.Functor.Identity (Identity)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Monoid (Ap)
import Data.Ord (Down)
import Data.Proxy (Proxy)
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
import Data.Tuple (Solo)
import Data.Type.Coercion (Coercion)
import Data.Type.Equality (type (:~:), type (~~))
import Data.Typeable (type (:~~:))
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
import GHC.Exts (Levity, VecCount, VecElem)
import GHC.Generics
  ( Associativity,
    DecidedStrictness,
    SourceStrictness,
  )
import GHC.Natural (Natural)
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

-- | Newtype that gives @UpperBounded@, @LowerBounded@ instances to types
-- that only have a @Bounded@ instance.
--
-- ==== __Examples__
-- >>> newtype Foo = MkFoo Int8 deriving stock (Bounded, Eq, Ord, Show)
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

-- | Names the upper limit of a type. Types that also have a 'Bounded'
-- instance should define @upperBound === maxBound@.
--
-- @since 0.1
class UpperBounded a where
  -- | @since 0.1
  upperBound :: a

-- | @since 0.1
instance Bounded a => UpperBounded (Bounds a) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded All where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Any where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CBool where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CChar where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CInt where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CIntMax where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CIntPtr where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CLLong where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CLong where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CPtrdiff where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CSChar where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CShort where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CSigAtomic where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CSize where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUChar where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUInt where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUIntMax where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUIntPtr where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CULLong where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CULong where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUShort where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CWchar where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded IntPtr where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded WordPtr where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded ByteOrder where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Associativity where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded DecidedStrictness where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded SourceStrictness where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Int16 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Int32 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Int64 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Int8 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded GeneralCategory where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Word16 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Word32 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Word64 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Word8 where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CBlkCnt where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CBlkSize where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CClockId where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CDev where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CFsBlkCnt where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CFsFilCnt where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CGid where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CId where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CIno where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CKey where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CMode where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CNfds where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CNlink where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded COff where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CPid where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CRLim where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CSsize where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CTcflag where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded CUid where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Fd where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Ordering where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded () where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Bool where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Char where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Int where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Levity where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded VecCount where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded VecElem where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded Word where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (And a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Iff a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Ior a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Xor a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Identity a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Down a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (First a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Last a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Max a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Min a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (WrappedMonoid a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Dual a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Product a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Sum a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Solo a) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance UpperBounded (Proxy t) where upperBound = maxBound; {-# INLINE upperBound #-}

-- | @since 0.1
instance (Bounded a, Bounded b) => UpperBounded (a, b) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance Bounded a => UpperBounded (Const a b) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (Applicative f, Bounded a) => UpperBounded (Ap f a) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance Coercible a b => UpperBounded (Coercion a b) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance a ~ b => UpperBounded (a :~: b) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance a ~~ b => UpperBounded (a :~~: b) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance (Bounded a, Bounded b, Bounded c) => UpperBounded (a, b, c) where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d
  ) =>
  UpperBounded (a, b, c, d)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e
  ) =>
  UpperBounded (a, b, c, d, e)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f
  ) =>
  UpperBounded (a, b, c, d, e, f)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g
  ) =>
  UpperBounded (a, b, c, d, e, f, g)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m,
    Bounded n
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m,
    Bounded n,
    Bounded o
  ) =>
  UpperBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  upperBound = maxBound
  {-# INLINE upperBound #-}

-- | Names the lower limit of a type. Types that also have a 'Bounded'
-- instance should define @lowerBound === minBound@.
--
-- @since 0.1
class LowerBounded a where
  -- | @since 0.1
  lowerBound :: a

-- | @since 0.1
instance Bounded a => LowerBounded (Bounds a) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Natural where
  lowerBound = 0
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded All where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Any where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CBool where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CChar where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CInt where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CIntMax where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CIntPtr where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CLLong where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CLong where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CPtrdiff where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CSChar where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CShort where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CSigAtomic where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CSize where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUChar where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUInt where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUIntMax where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUIntPtr where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CULLong where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CULong where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUShort where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CWchar where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded IntPtr where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded WordPtr where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded ByteOrder where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Associativity where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded DecidedStrictness where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded SourceStrictness where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Int16 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Int32 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Int64 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Int8 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded GeneralCategory where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Word16 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Word32 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Word64 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Word8 where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CBlkCnt where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CBlkSize where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CClockId where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CDev where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CFsBlkCnt where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CFsFilCnt where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CGid where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CId where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CIno where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CKey where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CMode where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CNfds where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CNlink where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded COff where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CPid where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CRLim where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CSsize where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CTcflag where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded CUid where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Fd where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Ordering where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded () where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Bool where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Char where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Int where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Levity where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded VecCount where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded VecElem where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded Word where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (And a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Iff a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Ior a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Xor a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Identity a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Down a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (First a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Last a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Max a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Min a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (WrappedMonoid a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Dual a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Product a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Sum a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Solo a) where lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance LowerBounded (Proxy t) where; lowerBound = minBound; {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Bounded a, Bounded b) => LowerBounded (a, b) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance Bounded a => LowerBounded (Const a b) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Applicative f, Bounded a) => LowerBounded (Ap f a) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance Coercible a b => LowerBounded (Coercion a b) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance a ~ b => LowerBounded (a :~: b) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance a ~~ b => LowerBounded (a :~~: b) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance (Bounded a, Bounded b, Bounded c) => LowerBounded (a, b, c) where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d
  ) =>
  LowerBounded (a, b, c, d)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e
  ) =>
  LowerBounded (a, b, c, d, e)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f
  ) =>
  LowerBounded (a, b, c, d, e, f)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g
  ) =>
  LowerBounded (a, b, c, d, e, f, g)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m,
    Bounded n
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | @since 0.1
instance
  ( Bounded a,
    Bounded b,
    Bounded c,
    Bounded d,
    Bounded e,
    Bounded f,
    Bounded g,
    Bounded h,
    Bounded i,
    Bounded j,
    Bounded k,
    Bounded l,
    Bounded m,
    Bounded n,
    Bounded o
  ) =>
  LowerBounded (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
  where
  lowerBound = minBound
  {-# INLINE lowerBound #-}

-- | For types that have no upper bound.
--
-- @since 0.1
class UpperBoundless a

-- | @since 0.1
instance UpperBoundless Integer

-- | @since 0.1
instance UpperBoundless Natural

-- | For types that have no lower bound.
--
-- @since 0.1
class LowerBoundless a

-- | @since 0.1
instance LowerBoundless Integer
