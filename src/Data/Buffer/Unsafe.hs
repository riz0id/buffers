{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

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
    shrink
    -- * Index
  , indexChar
  , indexUtf8
  , indexWord8
  , indexWord16
  , indexWord32
    -- * Write
  , writeWord8
  , writeWord16
  , writeWord32
  ) where


import Data.Buffer.Core (Buffer (..))
import Data.Buffer.Prim qualified as Prim

import GHC.Exts (Char (..), Int (..))
import GHC.IO (IO (..))
import GHC.Word (Word16 (..), Word32 (..), Word8 (..))

--------------------------------------------------------------------------------


-- Basic Operations ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
shrink :: Buffer -> Int -> IO ()
shrink (B# buffer#) (I# i#) = IO \st# -> (# Prim.shrink# buffer# i# st#, () #)
{-# INLINE shrink #-}

-- Index -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
indexChar :: Buffer -> Int -> IO Char
indexChar (B# buffer#) (I# i#) = IO \st0# -> 
  case Prim.indexChar# buffer# i# st0# of 
    (# st1#, x# #) -> (# st1#, C# x# #)
{-# INLINE indexChar #-}

-- | TODO: docs
--
-- @since 1.0.0
indexUtf8 :: Buffer -> Int -> IO (Char, Int)
indexUtf8 (B# buffer#) (I# i#) = IO \st0# -> 
  case Prim.indexUtf8# buffer# i# st0# of 
    (# st1#, x#, n# #) -> (# st1#, (C# x#, I# n#) #)
{-# INLINE indexUtf8 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord8 :: Buffer -> Int -> IO Word8
indexWord8 (B# buffer#) (I# i#) = IO \st0# -> 
  case Prim.indexWord8# buffer# i# st0# of
    (# st1#, x# #) -> (# st1#, W8# x# #)
{-# INLINE indexWord8 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord16 :: Buffer -> Int -> IO Word16
indexWord16 (B# buffer#) (I# i#) = IO \st0# -> 
  case Prim.indexWord16# buffer# i# st0# of
    (# st1#, x# #) -> (# st1#, W16# x# #)
{-# INLINE indexWord16 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord32 :: Buffer -> Int -> IO Word32
indexWord32 (B# buffer#) (I# i#) = IO \st0# -> 
  case Prim.indexWord32# buffer# i# st0# of
    (# st1#, x# #) -> (# st1#, W32# x# #)
{-# INLINE indexWord32 #-}

-- Write -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeWord8 :: Buffer -> Int -> Word8 -> IO ()
writeWord8 (B# buffer#) (I# i#) (W8# x#) = IO \st# -> 
  (# Prim.writeWord8# buffer# i# x# st#, () #)
{-# INLINE writeWord8 #-}

-- | TODO: docs
--
-- @since 1.0.0
writeWord16 :: Buffer -> Int -> Word16 -> IO ()
writeWord16 (B# buffer#) (I# i#) (W16# x#) = IO \st# ->
  (# Prim.writeWord16# buffer# i# x# st#, () #)
{-# INLINE writeWord16 #-}

-- | TODO: docs
--
-- @since 1.0.0
writeWord32 :: Buffer -> Int -> Word32 -> IO ()
writeWord32 (B# buffer#) (I# i#) (W32# x#) = IO \st# -> 
  (# Prim.writeWord32# buffer# i# x# st#, () #)
{-# INLINE writeWord32 #-}

