{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_HADDOCK show-extensions #-}

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
  , fromString
  , grow
  , shrink
    -- * Query
  , length
  , null
  , pointer
    -- * Compare
  , compareString
  , compareStringUtf8
    -- * Index
  , indexChar
  , indexUtf8
  , indexWord8
  , indexWord16
  , indexWord32
    -- * Write
  , writeUtf8
  , writeWord8
  , writeWord16
  , writeWord32
  ) where

import Control.Exception (throwIO)
import Control.Exception.RangeError
  (pattern EndingRangeError, pattern StartingRangeError)
import Control.Monad (unless)

import Data.Bool.Prim qualified as Bool
import Data.Buffer.Core (Buffer (..), pointer, throwRangeErrorIO)
import Data.Buffer.Prim qualified as Prim
import Data.Buffer.Unsafe qualified as Unsafe
import Data.Foldable (foldr')
import Data.Utf8 qualified as Utf8
import Data.Word (Word16, Word32, Word8)

import GHC.Exts (Int (..))
import GHC.IO (IO (..))

import Prelude hiding (length, null)

-- Buffer - Basic Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
allocate :: Int -> IO Buffer
allocate count
  | 0 <= count = Unsafe.allocate count
  | otherwise  = errorWithoutStackTrace ("allocate: argument #1 must be a 'Int' greater than or equal to 0, got: " ++ show count) -- FIXME: canonicalize this error
{-# INLINE allocate #-}

-- | TODO: docs
--
-- @since 1.0.0
fromString :: String -> IO Buffer
fromString str = do
  let len = foldr' ((+) . Utf8.lengthUtf8Char) 0 str
  buffer <- allocate len

  let run :: Int -> String -> IO ()
      run _ ""       = pure ()
      run i (c : cs) = do
        n <- writeUtf8 buffer i c
        run (n + i) cs
   in run 0 str

  pure buffer

-- | TODO: docs
--
-- @since 1.0.0
grow :: Buffer -> Int -> IO Buffer
grow buffer count =
  case compare count 0 of
    GT -> Unsafe.grow buffer count
    EQ -> pure buffer
    LT -> errorWithoutStackTrace ("grow: argument #2 must be a 'Int' greater than or equal to 0, got: " ++ show count) -- FIXME: canonicalize this error
{-# INLINE grow #-}

-- | TODO: docs
--
-- @since 1.0.0
shrink :: Buffer -> Int -> IO ()
shrink buffer n = do
  len <- length buffer
  if 0 <= n && n < len
    then Unsafe.shrink buffer n
    else throwRangeErrorIO 'shrink n len
{-# INLINE shrink #-}

-- Buffer - Query --------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtain the length of the 'Buffer' in 8-bit words.
--
-- @since 1.0.0
length :: Buffer -> IO Int
length (B# buffer#) = IO \st0# ->
  case Prim.length# buffer# st0# of
    (# st1#, len# #) -> (# st1#, I# len# #)
{-# INLINE length #-}

-- | \(\mathcal{O}(1)\). Test if the given 'Buffer' has a 'length' of zero.
--
-- @since 1.0.0
null :: Buffer -> IO Bool
null (B# buffer#) = IO \st0# ->
  case Prim.null# buffer# st0# of
    (# st1#, x# #) -> (# st1#, Bool.toBool x# #)
{-# INLINE null #-}

-- Buffer - Compare ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
compareString ::
  -- | TODO: docs
  Buffer ->
  -- | TODO: docs
  String ->
  -- | TODO: docs
  Int ->
  -- | TODO: docs
  IO Bool
compareString buffer str off
  | off < 0   = pure False
  | otherwise = do
    len <- length buffer
    let iter :: Int -> String -> IO Bool
        iter _ "" = pure True
        iter i (ch0 : cs)
          | i > len   = pure False
          | otherwise = do
            ch1 <- indexChar buffer i
            if ch0 == ch1
              then iter (1 + i) cs
              else pure False
     in iter off str

-- | TODO: docs
--
-- @since 1.0.0
compareStringUtf8 ::
  -- | TODO: docs
  Buffer ->
  -- | TODO: docs
  String ->
  -- | TODO: docs
  Int ->
  -- | TODO: docs
  IO Bool
compareStringUtf8 buffer str off
  | off < 0   = pure False
  | otherwise = do
    len <- length buffer
    let iter :: Int -> String -> IO Bool
        iter _ "" = pure True
        iter i (ch0 : cs)
          | i > len   = pure False
          | otherwise = do
            (ch1, n) <- Unsafe.indexUtf8 buffer i
            if ch0 == ch1
              then iter (i + n) cs
              else pure False
     in iter off str

-- Buffer - Index --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
indexChar :: Buffer -> Int -> IO Char
indexChar buffer i = do
  len <- length buffer
  if 0 <= i && i < len
    then Unsafe.indexChar buffer i
    else throwRangeErrorIO 'indexChar i (len - 1)
{-# INLINE indexChar #-}

-- | TODO: docs
--
-- @since 1.0.0
indexUtf8 :: Buffer -> Int -> IO Char
indexUtf8 buffer i = do
  len <- length buffer
  if 0 <= i && i < len
    then fmap fst (Unsafe.indexUtf8 buffer i)
    else throwRangeErrorIO 'indexUtf8 i (len - 1)
{-# INLINE indexUtf8 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord8 :: Buffer -> Int -> IO Word8
indexWord8 buffer i = do
  len <- length buffer
  if 0 <= i && i < len
    then Unsafe.indexWord8 buffer i
    else throwRangeErrorIO 'indexWord16 i (len - 2)
{-# INLINE indexWord8 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord16 :: Buffer -> Int -> IO Word16
indexWord16 buffer i = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then Unsafe.indexWord16 buffer i
    else throwRangeErrorIO 'indexWord16 i (len - 2)
{-# INLINE indexWord16 #-}

-- | TODO: docs
--
-- @since 1.0.0
indexWord32 :: Buffer -> Int -> IO Word32
indexWord32 buffer i = do
  len <- length buffer
  if 0 <= i && i < len - 3
    then Unsafe.indexWord32 buffer i
    else throwRangeErrorIO 'indexWord32 i (len - 4)
{-# INLINE indexWord32 #-}

-- Buffer - Write --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
writeUtf8 :: Buffer -> Int -> Char -> IO Int
writeUtf8 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len
    then Unsafe.writeUtf8 buffer i x
    else throwRangeErrorIO 'writeWord8 i (len - 1)

-- | TODO: docs
--
-- @since 1.0.0
writeWord8 :: Buffer -> Int -> Word8 -> IO ()
writeWord8 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len
    then Unsafe.writeWord8 buffer i x
    else throwRangeErrorIO 'writeWord8 i (len - 1)
{-# INLINE writeWord8 #-}

-- | TODO: docs
--
-- @since 1.0.0
writeWord16 :: Buffer -> Int -> Word16 -> IO ()
writeWord16 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then Unsafe.writeWord16 buffer i x
    else throwRangeErrorIO 'writeWord16 i (len - 2)
{-# INLINE writeWord16 #-}

-- | TODO: docs
--
-- @since 1.0.0
writeWord32 :: Buffer -> Int -> Word32 -> IO ()
writeWord32 buffer i x = do
  len <- length buffer
  if 0 <= i && i < len - 1
    then Unsafe.writeWord32 buffer i x
    else throwRangeErrorIO 'writeWord32 i (len - 4)
{-# INLINE writeWord32 #-}

-- Buffer - Fill ---------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
fillWord8 :: Buffer -> Int -> Int -> Word8 -> IO ()
fillWord8 buffer offset count x = do
  let end = offset + count
  len <- length buffer

  unless (0 <= offset && offset < len) do
    throwIO (StartingRangeError 'fillWord8 ''Buffer offset 0 len)

  unless (0 <= end && end < len) do
    throwIO (EndingRangeError 'fillWord8 ''Buffer end 0 len)

  Unsafe.fillWord8 buffer offset count x
