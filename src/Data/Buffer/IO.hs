{-# LANGUAGE TemplateHaskellQuotes #-}

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
  , hGetBuf
  , hPut
  , hPutBuf
  ) where

import Control.Exception (Exception, throwIO)

import Control.Monad (when)

import Data.Buffer (Buffer (..))
import Data.Buffer qualified as Buffer

import GHC.IO.Handle qualified as GHC

import System.IO qualified as IO
import System.IO (Handle)
import Control.Exception.RangeError (RangeError(..))

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
    size <- GHC.hFileSize handle

    -- TODO: docs
    let maxFileSize :: Integer
        maxFileSize = toInteger (maxBound :: Int)
     in when (maxFileSize < size) do
          throwIO (FileSizeError filepath size)

    -- TODO: docs
    buffer <- Buffer.allocate (fromInteger size)

    -- TODO: docs
    count <- hGetBuf handle buffer (fromInteger size)

    -- TODO: docs
    when (toInteger count < size) do
      Buffer.shrink buffer count

    pure buffer

-- | TODO: docs
--
-- @since 1.0.0
hGetBuf :: 
  -- | TODO: docs
  Handle -> 
  -- | TODO: docs
  Buffer -> 
  -- | TODO: docs
  Int -> 
  -- | TODO: docs
  IO Int
hGetBuf handle buffer count = do 
  len <- Buffer.length buffer 
  if 0 <= count && count <= len 
    then IO.hGetBuf handle (Buffer.pointer buffer) count
    else throwIO (RangeError 'hGetBuf ''Buffer Nothing count 0 len)

-- | TODO: docs
--
-- @since 1.0.0
hPut :: Handle -> Buffer -> IO ()
hPut handle buffer = do
  let ptr = Buffer.pointer buffer 
  len <- Buffer.length buffer
  IO.hPutBuf handle ptr len

-- | TODO: docs
--
-- @since 1.0.0
hPutBuf :: 
  -- | TODO: docs
  Handle -> 
  -- | TODO: docs
  Buffer -> 
  -- | TODO: docs
  Int -> 
  -- | TODO: docs
  IO ()
hPutBuf handle buffer count = do 
  len <- Buffer.length buffer 
  if 0 <= count && count <= len 
    then IO.hPutBuf handle (Buffer.pointer buffer) count
    else throwIO (RangeError 'hGetBuf ''Buffer Nothing count 0 len)



