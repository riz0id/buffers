{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

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
  ) where

import Control.Exception (throwIO)
import Control.Exception.RangeError (RangeError (..), RangePrefix (..))
import Control.Monad (unless, (>=>))

import Data.Buffer qualified as Buffer
import Data.Buffer.Core (Buffer (..))
import Data.Buffer.Prim (Buffer# (..))
import Data.Coerce (coerce)
import Data.Primitive.ByteArray
  ( ByteArray
  , MutableByteArray (MutableByteArray)
  , copyMutableByteArray
  , newPinnedByteArray
  , unsafeFreezeByteArray
  )
import Data.Slice.Core (Slice (..))

import GHC.Exts (RealWorld)

import Prelude hiding (length)

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
  len <- Buffer.length buffer
  let end = off + n

  unless (0 <= off && off < len) do
    throwIO (RangeError 'slice ''Buffer (Just PrefixStarting) off 0 len)

  unless (0 <= end && end < len) do
    throwIO (RangeError 'slice ''Buffer (Just PrefixEnding) end 0 len)

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
