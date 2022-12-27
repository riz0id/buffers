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
  -- , slice
  ) where

-- import Control.Exception (throwIO)
-- import Control.Exception.RangeError (RangeError (..), RangePrefix (..))

-- import Control.Monad (when)
-- import Control.Monad.Primitive (RealWorld)

-- import Data.Buffer (Buffer (..))
-- import Data.Buffer qualified as Buffer
-- import Data.Primitive (MutableByteArray (..))
import Data.Slice.Core (Slice (..))

--------------------------------------------------------------------------------

-- Basic Operations ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
-- slice ::
--   -- | TODO: docs
--   Buffer ->
--   -- | TODO: docs
--   Int ->
--   -- | TODO: docs
--   Int ->
--   -- | TODO: docs
--   IO Slice
-- slice buffer i n = do
--   len <- Buffer.length buffer

--   when (i < 0 || i >= len) do
--     throwIO (RangeError 'slice ''Buffer (Just PrefixStarting) i 0 len)

--   _
