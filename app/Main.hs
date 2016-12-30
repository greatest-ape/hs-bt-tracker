{-# LANGUAGE OverloadedStrings #-}

-- hs-bt-tracker: Haskell UDP BitTorrent tracker
-- Copyright (c) 2016-2017 Joakim Frostegård <joakim.frostegard@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Main where

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Socket
import qualified System.Posix.Signals as Signals

import Control.Concurrent (killThread)
import Control.Exception.Lifted (bracket)
import Control.Monad (replicateM, forever, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)

import qualified Converters
import qualified Handlers
import qualified Utils

import Types


main :: IO ()
main = do
    let config = Config {
        _serverAddress             = "0.0.0.0",
        _serverPort                = 8000,
        _numberOfThreads           = 2, -- ATM more threads don't necessarily improve performance

        _announceInterval          = 1800,
        _maximumPeersToSend        = 100,

        _connectionMaxAge          = 300,
        _connectionPruneInterval   = 30,

        _peerMaxAge                = 2000,
        _peerPruneInterval         = 60
    }

    initialState <- createInitialState config

    flip runReaderT initialState $ do
        address <- Utils.getConfigField _serverAddress
        port    <- Utils.getConfigField _serverPort
        liftIO $ putStrLn $
            "Starting BitTorrent server on " ++ address ++ ":" ++ show port ++ ".."

        Utils.forkAppM pruneConnections
        Utils.forkAppM prunePeers

        runUDPServer

        Utils.waitForExit

    return ()


createInitialState :: Config -> IO State
createInitialState config = State
    <$> return config
    <*> (STM.atomically $ STM.newTVar $ TorrentMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ ConnectionMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ [])
    <*> MVar.newEmptyMVar


runUDPServer :: AppM ()
runUDPServer = do
    threadId <- Utils.forkAppM $ do
        bracket createSocket exitCleanly $ \socket -> do
            quitOnSignal Signals.sigINT socket
            quitOnSignal Signals.sigTERM socket

            numberOfThreads <- Utils.getConfigField _numberOfThreads

            threadIds <- replicateM (numberOfThreads - 1) $
                Utils.forkAppM $ acceptConnections socket

            Utils.withThreadIds (++ threadIds)

            acceptConnections socket

    Utils.withThreadIds ((:) threadId)

    where
        quitOnSignal signal socket = do
            state <- ask
            liftIO $ Signals.installHandler
                signal
                (Signals.Catch $ runReaderT (exitCleanly socket) state)
                Nothing


createSocket :: AppM Socket.Socket
createSocket = do
    serverAddress <- Utils.getConfigField _serverAddress
    serverPort    <- show <$> Utils.getConfigField _serverPort

    addrInfos <- liftIO $ Socket.getAddrInfo
        (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
        (Just serverAddress)
        (Just serverPort)

    -- Select the first option with IPv4
    let serverAddr = head $ filter(\addr -> Socket.addrFamily addr == Socket.AF_INET) addrInfos

    liftIO $ do
        socket <- Socket.socket
            (Socket.addrFamily serverAddr)
            Socket.Datagram
            Socket.defaultProtocol

        Socket.bind socket $ Socket.addrAddress serverAddr

        return socket


exitCleanly :: Socket.Socket -> AppM ()
exitCleanly socket = do
    threadIds <- Utils.getThreadIds

    liftIO $ do
        forM_ threadIds killThread
        Socket.close socket

    Utils.signalExit

    return ()


acceptConnections :: Socket.Socket -> AppM ()
acceptConnections socket = forever $ do
    (requestBytes, remoteAddress) <- liftIO $ Socket.recvFrom socket 2048

    response <- case Converters.bytesToRequest requestBytes of
        Left (unconsumedByteString, consumedBytes, errorMessage) ->
            return $ ErrorResponse $ ErrorResponseInner
                (TransactionID 0)
                "Invalid request"
        Right (_, _, request) ->
             Handlers.handleRequest request remoteAddress

    liftIO $ Socket.sendTo socket (Converters.responseToBytes response) remoteAddress


pruneConnections :: AppM ()
pruneConnections = forever $ do
    connectionMaxAge        <- Utils.getConfigField _connectionMaxAge
    connectionPruneInterval <- Utils.getConfigField _connectionPruneInterval

    timestampLimit <- (\(Timestamp t) -> Timestamp $ t - connectionMaxAge) <$>
        liftIO Utils.getTimestamp

    Utils.withConnectionMap $ Map.filter (\t -> t > timestampLimit)

    liftIO $ Utils.threadDelaySeconds connectionPruneInterval


prunePeers :: AppM ()
prunePeers = forever $ do
    peerMaxAge        <- Utils.getConfigField _peerMaxAge
    peerPruneInterval <- Utils.getConfigField _peerPruneInterval

    timestampLimit <- (\(Timestamp t) -> Timestamp $ t - peerMaxAge) <$>
        liftIO Utils.getTimestamp

    Utils.withTorrentMap $
        Map.map (Sequence.filter (\peer -> _lastAnnounce peer > timestampLimit))

    liftIO $ Utils.threadDelaySeconds peerPruneInterval
