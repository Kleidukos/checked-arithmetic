{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Checked where

import Data.Int
import Data.Kind
import Data.Word
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Prim
import System.IO.Unsafe

newtype Checked (overflowed :: Bool) (a :: Type) = Checked {bitPattern :: a}

checkedSignedAdd :: Int32 -> Int32 -> Checked overflowed Int32
checkedSignedAdd a b =
  unsafeDupablePerformIO $
    allocaBytes
      4
      ( \(resultPtr :: Ptr Int32) -> do
          let overflowed = builtin_sadd_overflow (coerce a) (coerce b) (castPtr resultPtr :: Ptr CInt)
          result <- peek resultPtr
          if overflowed
            then pure $ Checked @True result
            else pure $ Checked @False result
      )

foreign import capi "__builtin_add_overflow_p"
  build_add_overflow_p
    :: CChar
    -> CChar
    -> CChar
    -> Bool

foreign import capi "__builtin_sadd_overflow"
  builtin_sadd_overflow
    :: CInt
    -> CInt
    -> Ptr CInt
    -> Bool

-- For all 32-bit unsigned ints:

-- * CUInt

-- * Word32

-- * CUSeconds
foreign import capi "__builtin_uadd_overflow"
  builtin_uadd_overflow
    :: Word32
    -> Word32
    -> Ptr Word32
    -> Bool
