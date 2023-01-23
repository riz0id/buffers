{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Buffer.Prim
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
module Data.Buffer.Prim
  ( Buffer# (..),

    -- * Basic Operations
    allocate#,
    copy#,
    grow#,
    shrink#,

    -- * Query
    length#,
    null#,
    pointer#,

    -- * Index
    indexChar8#,
    indexUtf8#,
    indexWord8#,
    indexWord16#,
    indexWord32#,

    -- * Write 
    writeChar#,
    writeUtf8#,
    writeWord8#,
    writeWord16#,
    writeWord32#,

    -- * Fill
    fillWord8#,
  ) where

import Control.Monad.Primitive (RealWorld)

import Data.Coerce ( coerce )
import Data.Bool.Prim (Bool#)
import qualified Data.Bool.Prim as Bool
import qualified Data.Utf8.Prim as Utf8

import GHC.Exts (UnliftedType, MutableByteArray#, Int#, Word8#, Word16#, Word32#, State#, Addr#, Char#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Buffer# :: UnliftedType where
  Buffer# :: MutableByteArray# RealWorld -> Buffer#

-- Buffer# - Basic Operations --------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
allocate# :: Int# -> State# RealWorld -> (# State# RealWorld, Buffer# #)
allocate# = coerce GHC.newPinnedByteArray#

-- | TODO: docs
--
-- @since 1.0.0
copy# :: Buffer# -> Int# -> Buffer# -> Int# -> Int# -> State# RealWorld -> State# RealWorld
copy# = coerce GHC.copyMutableByteArray#

-- | TODO: docs
--
-- @since 1.0.0
grow# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Buffer# #)
grow# src# count# st0# =
  let !(# st1#, len# #) = length# src# st0#
      !(# st2#, dst# #) = allocate# (count# GHC.+# len#) st1#
   in (# copy# src# 0# dst# 0# len# st2#, dst# #)

-- | TODO: docs
--
-- @since 1.0.0
shrink# :: Buffer# -> Int# -> State# RealWorld -> State# RealWorld
shrink# = coerce GHC.shrinkMutableByteArray#

-- Buffer# - Query -------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of the 'Buffer#' in 8-bit words.
--
-- @since 1.0.0
length# :: Buffer# -> State# RealWorld -> (# State# RealWorld, Int# #)
length# = coerce GHC.getSizeofMutableByteArray#

-- | \(\mathcal{O}(1)\). Test if the given 'Buffer#' has a 'length#' of zero.
--
-- @since 1.0.0
null# :: Buffer# -> State# RealWorld -> (# State# RealWorld, Bool# #)
null# buf# st0# = case length# buf# st0# of
  (# st1#, n# #) -> (# st1#, Bool.unsafeFromInt# (n# GHC.==# 0#) #)

-- | TODO: docs
--
-- @since 1.0.0
pointer# :: Buffer# -> Addr#
pointer# = coerce GHC.mutableByteArrayContents#

-- Buffer# - Index# ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
indexChar8# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Char# #)
indexChar8# = coerce GHC.readCharArray#

-- | TODO: docs
--
-- @since 1.0.0
indexUtf8# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Char#, Int# #)
indexUtf8# = coerce Utf8.readUtf8Array#

-- | TODO: docs
--
-- @since 1.0.0
indexWord8# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Word8# #)
indexWord8# = coerce GHC.readWord8Array#

-- | TODO: docs
--
-- @since 1.0.0
indexWord16# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Word16# #)
indexWord16# = coerce GHC.readWord16Array#

-- | TODO: docs
--
-- @since 1.0.0
indexWord32# :: Buffer# -> Int# -> State# RealWorld -> (# State# RealWorld, Word32# #)
indexWord32# = coerce GHC.readWord32Array#

-- Buffer# - Write -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8# :: Buffer# -> Int# -> Char# -> State# RealWorld -> (# State# RealWorld, Int# #)
writeUtf8# = coerce Utf8.writeUtf8Array#

-- | TODO: docs
--
-- @since 1.0.0
writeChar# :: Buffer# -> Int# -> Char# -> State# RealWorld -> State# RealWorld
writeChar# = coerce GHC.writeCharArray#

-- | TODO: docs
--
-- @since 1.0.0
writeWord8# :: Buffer# -> Int# -> Word8# -> State# RealWorld -> State# RealWorld
writeWord8# = coerce GHC.writeWord8Array#

-- | TODO: docs
--
-- @since 1.0.0
writeWord16# :: Buffer# -> Int# -> Word16# -> State# RealWorld -> State# RealWorld
writeWord16# = coerce GHC.writeWord8ArrayAsWord16#

-- | TODO: docs
--
-- @since 1.0.0
writeWord32# :: Buffer# -> Int# -> Word32# -> State# RealWorld -> State# RealWorld
writeWord32# = coerce GHC.writeWord8ArrayAsWord32#

-- Buffer# - Fill --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
fillWord8# :: Buffer# -> Int# -> Int# -> Word8# -> State# RealWorld -> State# RealWorld
fillWord8# buffer# offset# count# byte# =
  let byteInt# :: Int#
      byteInt# = GHC.word2Int# (GHC.word8ToWord# byte#)
   in coerce GHC.setByteArray# buffer# offset# count# byteInt#
