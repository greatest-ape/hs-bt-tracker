{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Handlers.Common where

import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket

import Control.Monad (forM)

import Types


countLeechers :: Sequence.Seq Peer -> Int
countLeechers peers = Sequence.length $ Sequence.filter (== PeerLeeching) $ fmap _status peers

countSeeders :: Sequence.Seq Peer -> Int
countSeeders peers = Sequence.length $ Sequence.filter (== PeerSeeding) $ fmap _status peers

getIPAddress :: Socket.SockAddr -> Maybe IPAddress
getIPAddress (Socket.SockAddrInet _ address) = Just $ IPAddress address
getIPAddress _                               = Nothing
