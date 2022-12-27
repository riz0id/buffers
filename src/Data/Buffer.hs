{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples         #-}

-- |
-- Module      :  Data.Buffer
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
module Data.Buffer
  ( Buffer (..)
    -- * Basic Operations
  , allocate
  , shrink
  , withBufferPtr
    -- * Query
  , length
  , null
  , pointer
    -- * Index
  , indexWord8
  , indexWord16
  , indexWord32
    -- * Write
  , writeWord8
  , writeWord16
  , writeWord32
  ) where

import Control.Exception (throwIO)
import Control.Exception.RangeError (RangeError (..))

import Data.Buffer.Core (Buffer (..))
import Data.Buffer.Unsafe
  ( unsafeIndexWord16
  , unsafeIndexWord32
  , unsafeIndexWord8
  , unsafeWriteWord16
  , unsafeWriteWord32
  , unsafeWriteWord8, unsafeShrink
  )
import Data.Primitive
  (getSizeofMutableByteArray, mutableByteArrayContents, newPinnedByteArray)
import Data.Primitive.Ptr (Ptr)
import Data.Word (Word16, Word32, Word8)

import Language.Haskell.TH (Name)

import Prelude hiding (length, null)

--------------------------------------------------------------------------------

throwRangeError :: Name -> Int -> Int -> Int -> IO ()
throwRangeError func idx low high = throwIO (RangeError func ''Buffer Nothing idx low high)

-- Buffer - Basic Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
allocate :: Int -> IO Buffer
allocate = fmap Buffer . newPinnedByteArray

-- | TODO: docs
--
-- @since 1.0.0
-- resize :: Buffer -> IO ()
-- resize = _

-- | TODO: docs
--
-- @since 1.0.0
shrink :: Buffer -> Int -> IO ()
shrink buffer n = do 
  len <- length buffer 
  if 0 <= n && n < len 
    then unsafeShrink buffer n 
    else throwRangeError 'shrink n 0 len

-- | TODO: docs
--
-- @since 1.0.0
withBufferPtr :: Buffer -> (Ptr Word8 -> IO a) -> IO a
withBufferPtr buffer k = k (pointer buffer)

-- Buffer - Comparison ---------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0


-- Buffer - Query --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
length :: Buffer -> IO Int
length = getSizeofMutableByteArray . getBuffer

-- | TODO: docs
--
-- @since 1.0.0
null :: Buffer -> IO Bool
null = fmap (0 ==) . length

-- | TODO: docs
--
-- @since 1.0.0
pointer :: Buffer -> Ptr Word8 
pointer = mutableByteArrayContents . getBuffer

-- Buffer - Index --------------------------------------------------------------

throwIndexErrorIO :: Name -> Int -> Int -> IO a
throwIndexErrorIO func i = throwIO . RangeError func ''Buffer Nothing i 0

-- | Read a 'Word8' from a
--
-- @since 1.0.0
indexWord8 :: Buffer -> Int -> IO Word8
indexWord8 buffer i = do
  len <- length buffer
  if 0 <= i && i < len
    then unsafeIndexWord8 buffer i
    else throwIndexErrorIO 'indexWord8 i (len - 1)

-- | TODO: docs
--
-- @since 1.0.0
indexWord16 :: Buffer -> Int -> IO Word16
indexWord16 buffer i = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then unsafeIndexWord16 buffer i
    else throwIndexErrorIO 'indexWord16 i (len - 2)

-- | TODO: docs
--
-- @since 1.0.0
indexWord32 :: Buffer -> Int -> IO Word32
indexWord32 buffer i = do
  len <- length buffer
  if 0 <= i && i < len - 3
    then unsafeIndexWord32 buffer i
    else throwIndexErrorIO 'indexWord32 i (len - 4)

-- Buffer - Write --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeWord8 :: Buffer -> Int -> Word8 -> IO ()
writeWord8 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len
    then unsafeWriteWord8 buffer i x
    else throwIndexErrorIO 'writeWord8 i (len - 1)

-- | TODO: docs
--
-- @since 1.0.0
writeWord16 :: Buffer -> Int -> Word16 -> IO ()
writeWord16 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then unsafeWriteWord16 buffer i x
    else throwIndexErrorIO 'writeWord16 i (len - 2)

-- | TODO: docs
--
-- @since 1.0.0
writeWord32 :: Buffer -> Int -> Word32 -> IO ()
writeWord32 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then unsafeWriteWord32 buffer i x
    else throwIndexErrorIO 'writeWord32 i (len - 4)

