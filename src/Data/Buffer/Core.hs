{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
  , pointer
  , throwRangeErrorIO
  ) where

import Control.Exception (throwIO)
import Control.Exception.RangeError (RangeError (..))

import Data.Buffer.Prim (Buffer#)
import Data.Buffer.Prim qualified as Prim
import Data.Word (Word8)

import GHC.Ptr (Ptr (..))

import Language.Haskell.TH (Name)

--------------------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Buffer = B# Buffer#

-- | @since 1.0.0
instance Show Buffer where
  show x = "<Buffer: " ++ show (pointer x) ++ ">"
  {-# INLINE show #-}

  showsPrec _ x = showString "<Buffer: " . shows (pointer x) . showString ">"
  {-# INLINE showsPrec #-}

-- | TODO: docs
--
-- @since 1.0.0
pointer :: Buffer -> Ptr Word8
pointer (B# buffer#) = Ptr (Prim.pointer# buffer#)
{-# INLINE pointer #-}

-- | TODO: docs
--
-- @since 1.0.0
throwRangeErrorIO :: Name -> Int -> Int -> IO a
throwRangeErrorIO fun idx len = throwIO (RangeError fun ''Buffer Nothing idx 0 len)
{-# INLINE CONLIKE throwRangeErrorIO #-}
