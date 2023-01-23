{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Slice
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
module Data.Slice
  ( Slice (..)
    -- * Basic Operations
  , slice
  , toByteArray
  , toMutableByteArray
    -- * Query
  , length
  , null
    -- * Slicing
  , takeWhileUtf8
  ) where

import Control.Exception (throwIO)
import Control.Exception.RangeError
  (pattern EndingRangeError, pattern StartingRangeError)
import Control.Monad (unless, (>=>))

import Data.Buffer qualified as Buffer
import Data.Buffer.Core (Buffer (..), throwRangeErrorIO)
import Data.Buffer.Prim (Buffer# (..))
import Data.Buffer.Unsafe qualified as Buffer.Unsafe
import Data.Coerce (coerce)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray (MutableByteArray)
  , copyMutableByteArray
  , fillByteArray
  , newPinnedByteArray
  , unsafeFreezeByteArray
  )
import Data.Slice.Core (Slice (..))

import GHC.Exts (RealWorld)

import Prelude hiding (null, length)

--------------------------------------------------------------------------------

-- Slice - Basic Operations ----------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
slice ::
  -- | The source 'Buffer' to be sliced.
  Buffer ->
  -- | TODO: docs
  Int ->
  -- | The length of the 'Slice'.
  Int ->
  -- | TODO: docs
  IO Slice
slice buffer off n = do
  let end = off + n
  len <- Buffer.length buffer

  unless (0 <= off && off < len) do
    throwIO (StartingRangeError 'slice ''Buffer off 0 len)

  unless (0 <= end && end < len) do
    throwIO (EndingRangeError 'slice ''Buffer end 0 len)

  -- FIXME: assert that (off < off + n)

  pure (Slice buffer off end)

-- | TODO: docs
--
-- @since 1.0.0
toByteArray :: Slice -> IO ByteArray
toByteArray = toMutableByteArray >=> unsafeFreezeByteArray

-- | TODO: docs
--
-- @since 1.0.0
toMutableByteArray :: Slice -> IO (MutableByteArray RealWorld)
toMutableByteArray slx = do
  let src = getBuffer (slice_source slx)
  let off = slice_begin slx
  let len = length slx
  dst <- newPinnedByteArray len
  fillByteArray dst 0 len 0
  copyMutableByteArray dst 0 src off len
  pure dst
  where
    getBuffer :: Buffer -> MutableByteArray RealWorld
    getBuffer (B# buffer#) = MutableByteArray (coerce buffer#)

-- Slice - Query ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
length :: Slice -> Int
length slx = slice_end slx - slice_begin slx

-- | TODO: docs
--
-- @since 1.0.0
null :: Slice -> Bool
null slx = length slx == 0 

-- Slice - Slicing -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
takeWhileUtf8 :: 
  -- | TODO: docs
  (Char -> Bool) -> 
  -- | TODO: docs
  Buffer -> 
  -- | TODO: docs
  Int -> 
  -- | TODO: docs
  IO Slice
takeWhileUtf8 match buffer begin = do 
  len <- Buffer.length buffer
  if 0 <= begin && begin < len
    then 
      let run :: Int -> IO Int
          run i 
            | i < len = do 
              (char, n) <- Buffer.Unsafe.indexUtf8 buffer i
              if match char 
                then run (n + i)
                else pure i
            | otherwise = 
              pure i
       in fmap (Slice buffer begin) (run begin)
    else do 
      throwRangeErrorIO 'takeWhileUtf8 begin (len - 1)
