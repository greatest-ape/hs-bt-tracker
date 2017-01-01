{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Handlers.Common where

import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket

import Control.Monad (forM)

import Types


countLeechers :: Sequence.Seq Peer -> NumberOfLeechers
countLeechers peers = fromIntegral <$>
    Sequence.length $ Sequence.filter (\peer -> _status peer == PeerLeeching) peers

countSeeders :: Sequence.Seq Peer -> NumberOfSeeders
countSeeders peers = fromIntegral <$>
    Sequence.length $ Sequence.filter (\peer -> _status peer == PeerSeeding) peers

getIPAddress :: Socket.SockAddr -> Maybe IPAddress
getIPAddress (Socket.SockAddrInet _ address) = Just $ IPAddress address
getIPAddress _                               = Nothing
