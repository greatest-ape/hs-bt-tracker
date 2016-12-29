{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- General data types and instances
module Types.Common where

import qualified Data.ByteString.Char8 as BS

import Data.Hashable (Hashable, hashWithSalt)
import Data.Int (Int32, Int64)
import Data.Word (Word16, Word32)


newtype Timestamp = Timestamp Integer
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


newtype InfoHash = InfoHash BS.ByteString
    deriving (Show, Eq, Ord)

instance Hashable InfoHash where
    hashWithSalt salt (InfoHash bs) = hashWithSalt salt bs

newtype ConnectionID = ConnectionID Int64
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


newtype TransactionID = TransactionID Int32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

instance Hashable TransactionID where
    hashWithSalt salt (TransactionID int) = hashWithSalt salt int


newtype PeerID = PeerID BS.ByteString
    deriving (Show, Eq, Ord)

newtype PeerKey = PeerKey Word32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)

newtype PeerPort = PeerPort Word16
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)


newtype IPAddress = IPAddress Word32
    deriving (Show, Eq, Ord, Num, Enum, Real, Integral)
