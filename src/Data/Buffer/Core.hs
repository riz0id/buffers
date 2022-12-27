
-- |
-- Module      :  Data.Buffer.Core
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
module Data.Buffer.Core
  ( Buffer (..)
  ) where

import Control.Monad.Primitive (RealWorld)

import Data.Primitive (MutableByteArray (..), mutableByteArrayContents)
import Data.Primitive.Ptr (Ptr)
import Data.Word (Word8)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Buffer
  = Buffer { getBuffer :: MutableByteArray RealWorld }

-- | @since 1.0.0
instance Show Buffer where
  showsPrec p buffer =
    let ptr :: Ptr Word8
        ptr = mutableByteArrayContents (getBuffer buffer)
     in showString "<Buffer: " . showsPrec p ptr . showString ">"
  {-# INLINE showsPrec #-}
