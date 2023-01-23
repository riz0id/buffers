{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Buffer.IO
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
module Data.Buffer.IO
  ( Buffer (..)
    -- * Exceptions
  , FileSizeError (..)
    -- * Buffer I/O
  , fromFilePath
    -- ** Read
  , hGet
  , hGetBuf
    -- ** Write
  , putBuffer
  , hPut
  , hPutBuf
  ) where

import Control.Exception (Exception, throwIO)
import Control.Exception.RangeError 
  ( RangeError(..)
  , pattern EndingRangeError
  , pattern StartingRangeError
  )

import Control.Monad (when, unless)

import Data.Buffer (Buffer (..))
import Data.Buffer qualified as Buffer
import Data.Buffer.Unsafe qualified as Buffer.Unsafe

import GHC.IO.Handle qualified as GHC

import System.IO qualified as IO
import System.IO (Handle)
import qualified Foreign.Ptr as Ptr

-- Exceptions ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data FileSizeError = FileSizeError
  { -- | TODO: docs
    file_path :: FilePath
    -- | TODO: docs
  , file_size :: Integer
  }

-- | @since 1.0.0
instance Exception FileSizeError

-- | @since 1.0.0
instance Show FileSizeError where
  showsPrec p exn =
    showString "cannot read file (too large)\n  filepath: "
      . showsPrec p (file_path exn)
      . showString "\n  file size: "
      . showsPrec p (file_size exn)
      . showString " bytes\n  maximum file size: "
      . showsPrec p (maxBound :: Int)
      . showString " bytes"
  {-# INLINE showsPrec #-}

-- Buffer I/O ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
fromFilePath :: FilePath -> IO Buffer
fromFilePath filepath = do
  IO.withFile filepath IO.ReadMode \handle -> do
    -- TODO: docs
    size <- GHC.hFileSize handle
    assertFileSize size

    -- TODO: docs
    let len = fromInteger @Int size
    buffer <- Buffer.allocate (1 + len)
    count  <- hGetBuf handle buffer len 

    -- TODO: docs
    when (toInteger count < size) do
      Buffer.shrink buffer count

    -- TODO: docs
    Buffer.Unsafe.writeChar buffer len '\NUL' 

    pure buffer
  where 
    assertFileSize :: Integer -> IO ()
    assertFileSize size = 
      let maxFileSize :: Integer 
          maxFileSize = toInteger (maxBound :: Int)
       in when (maxFileSize <= 1 + size) do 
            throwIO (FileSizeError filepath size)

-- Buffer I/O - Read -----------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
hGet :: Handle -> Buffer -> IO Int
hGet handle buffer = do 
  len <- Buffer.length buffer
  IO.hGetBuf handle (Buffer.pointer buffer) len

-- | \(\mathcal{O}(1)\). The function @('hGetBuf' src dst n :: 'IO' 'Int')@ 
-- reads data from the 'Handle' @src@ into the 'Buffer' @dst@ until either:
--
--   * The @('\NUL' :: 'Char')@ terminator is reached. In this case, 'hGetBuf' 
--     will return some 'Int' greater-than or equal-to zero, but less than @n@. 
--
--   * Exactly @n@-many bytes have been read from @src@ into the target buffer 
--     @dst@. In this case, 'hGetBuf' will return an 'Int' equal-to @n@.
--
-- __NOTE__: This function performs bounds-checking.
--
-- @since 1.0.0
hGetBuf :: 
  -- | The source 'Handle'. 
  Handle -> 
  -- | The destination 'Buffer'.
  Buffer -> 
  -- | The maximum number of bytes to read. Must be greater-than or equal-to 
  -- @(0 :: Int)@ and less-than the length of the target 'Buffer'.
  Int -> 
  -- | The number of bytes read into the destination 'Buffer'. 
  IO Int
hGetBuf handle buffer n = do 
  len <- Buffer.length buffer 
  if 0 <= n && n <= len 
    then IO.hGetBuf handle (Buffer.pointer buffer) n
    else throwIO (RangeError 'hGetBuf ''Buffer Nothing n 0 len)

-- Buffer I/O - Write ----------------------------------------------------------

-- | \(\mathcal{O}(1)\). Write the contents of the 'Buffer' to 'IO.stdout'
--
-- @since 1.0.0
putBuffer :: Buffer -> IO ()
putBuffer = hPut IO.stdout
{-# INLINEABLE putBuffer #-}

-- | \(\mathcal{O}(1)\). The function @('hPut' dst src :: 'IO' ())@ writes the
-- entire contents of the 'Buffer' @src@ to the file 'Handle' @dst@.
--
-- @since 1.0.0
hPut :: 
  -- | The destination 'Handle'. 
  Handle -> 
  -- | The source 'Buffer'. 
  Buffer -> 
  IO ()
hPut handle buffer = do
  let ptr = Buffer.pointer buffer 
  len <- Buffer.length buffer
  IO.hPutBuf handle ptr len
{-# INLINEABLE hPut #-}

-- | \(\mathcal{O}(1)\). The function @('hPutBuf' dst src offset n :: 'IO' ())@ 
-- writes the contents of the 'Buffer' @src@ to the file 'Handle' @dst@.
--
-- __NOTE__: This function performs bounds-checking.
--
-- @since 1.0.0
hPutBuf :: 
  -- | The destination 'Handle'. 
  Handle -> 
  -- | The source 'Buffer'. 
  Buffer -> 
  -- | The beginning offset, in bytes. Must be greater-than or equal-to 
  -- @(0 :: Int)@ and less-than or equal-to the length of the buffer.
  Int -> 
  -- | The length of the region to fill, in bytes. Must be greater-than or equal 
  -- to @(0 :: Int)@ and less-than or equal-to the length of the buffer.
  Int -> 
  IO ()
hPutBuf dst src offset n = do 
  let ptr = Ptr.plusPtr (Buffer.pointer src) offset
  len <- Buffer.length src 

  unless (0 <= offset && offset <= len) do 
    throwIO (StartingRangeError 'hPutBuf ''Buffer offset 0 len)

  unless (0 <= n && n <= len - offset) do
    throwIO (EndingRangeError 'hPutBuf ''Buffer n 0 (len - offset))

  IO.hPutBuf dst ptr n
{-# INLINEABLE hPutBuf #-}



