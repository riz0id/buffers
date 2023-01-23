
-- |
-- Module      :  Data.Slice.Core
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
module Data.Slice.Core
  ( Slice (..)
  ) where

import Data.Buffer (Buffer)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Slice = Slice 
  { slice_source :: {-# UNPACK #-} !Buffer 
  , slice_begin  :: {-# UNPACK #-} !Int
  , slice_end    :: {-# UNPACK #-} !Int
  }
  deriving (Show)
