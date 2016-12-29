{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Handlers.Common where

import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket

import Data.Array.IO (readArray, writeArray, newListArray, IOArray)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Monad (forM)
import System.Random (randomIO, randomRIO)

import Types


countLeechers :: Sequence.Seq Peer -> Int
countLeechers peers = Sequence.length $ Sequence.filter (== PeerLeeching) $ fmap _status peers

countSeeders :: Sequence.Seq Peer -> Int
countSeeders peers = Sequence.length $ Sequence.filter (== PeerSeeding) $ fmap _status peers

getIPAddress :: Socket.SockAddr -> Maybe IPvXAddress
getIPAddress (Socket.SockAddrInet _ address) = Just $ IPv4Address address
getIPAddress (Socket.SockAddrInet6 _ _ address _) = Just $ IPv6Address address
getIPAddress _ = Nothing

getTimestamp :: IO TimeStamp
getTimestamp = TimeStamp . round <$> getPOSIXTime


-- Mix elements from two sequences
mixSequences (Sequence.viewl -> x Sequence.:< xs) (Sequence.viewl -> y Sequence.:< ys) =
     x Sequence.<| y Sequence.<| mixSequences xs ys
mixSequences (Sequence.viewl -> Sequence.EmptyL) ys = ys
mixSequences xs (Sequence.viewl -> Sequence.EmptyL) = xs


-- | Randomly shuffle a list
--   /O(N)/
--   from https://wiki.haskell.org/Random_shuffle
shuffleList :: [a] -> IO [a]
shuffleList xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArray :: Int -> [a] -> IO (IOArray Int a)
        newArray n xs =  newListArray (1,n) xs
