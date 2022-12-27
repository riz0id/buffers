{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns  #-}

-- |
-- Module      :  Data.Buffer.Unsafe
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Data.Buffer.Unsafe
  ( -- * Basic Operations
    unsafeShrink
    -- * Index
  , unsafeIndexWord8
  , unsafeIndexWord16
  , unsafeIndexWord32
    -- * Write
  , unsafeWriteWord8
  , unsafeWriteWord16
  , unsafeWriteWord32
  ) where

import Control.Monad.Primitive (RealWorld)

import Data.Buffer.Core (Buffer (..))
import Data.Primitive.ByteArray (MutableByteArray (..), shrinkMutableByteArray)

import GHC.Exts (Int (..), MutableByteArray#)
import GHC.Exts qualified as GHC
import GHC.IO (IO (..))
import GHC.Word (Word16 (..), Word32 (..), Word8 (..))

--------------------------------------------------------------------------------

getBuffer# :: Buffer -> MutableByteArray# RealWorld
getBuffer# (Buffer (MutableByteArray x#)) = x#

-- Basic Operations ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
unsafeShrink :: Buffer -> Int -> IO ()
unsafeShrink = shrinkMutableByteArray . getBuffer 

-- Index -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
unsafeIndexWord8 :: Buffer -> Int -> IO Word8
unsafeIndexWord8 (getBuffer# -> src#) (I# i#) =
  IO \st0# -> case GHC.readWord8Array# src# i# st0# of
    (# st1#, x# #) -> (# st1#, W8# x# #)

-- | TODO: docs
--
-- @since 1.0.0
unsafeIndexWord16 :: Buffer -> Int -> IO Word16
unsafeIndexWord16 (getBuffer# -> src#) (I# i#) =
  IO \st0# -> case GHC.readWord8ArrayAsWord16# src# i# st0# of
    (# st1#, x# #) -> (# st1#, W16# x# #)

-- | TODO: docs
--
-- @since 1.0.0
unsafeIndexWord32 :: Buffer -> Int -> IO Word32
unsafeIndexWord32 (getBuffer# -> src#) (I# i#) =
  IO \st0# -> case GHC.readWord8ArrayAsWord32# src# i# st0# of
    (# st1#, x# #) -> (# st1#, W32# x# #)

-- Write -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
unsafeWriteWord8 :: Buffer -> Int -> Word8 -> IO ()
unsafeWriteWord8 (getBuffer# -> src#) (I# i#) (W8# x#) = 
  IO \st# -> (# GHC.writeWord8Array# src# i# x# st#, () #)

-- | TODO: docs
--
-- @since 1.0.0
unsafeWriteWord16 :: Buffer -> Int -> Word16 -> IO ()
unsafeWriteWord16 (getBuffer# -> src#) (I# i#) (W16# x#) = 
  IO \st# -> (# GHC.writeWord8ArrayAsWord16# src# i# x# st#, () #)

-- | TODO: docs
--
-- @since 1.0.0
unsafeWriteWord32 :: Buffer -> Int -> Word32 -> IO ()
unsafeWriteWord32 (getBuffer# -> src#) (I# i#) (W32# x#) = 
  IO \st# -> (# GHC.writeWord8ArrayAsWord32# src# i# x# st#, () #)

