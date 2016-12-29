{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map
import qualified Network.Socket as Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as Socket

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception.Lifted (bracket)
import Control.Monad (replicateM, forever, forM_)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.IO.Class (liftIO)

import qualified Converters
import qualified Handlers
import qualified Utils

import Types


main :: IO ()
main = do
    let config = Config {
        _serverAddress      = "0.0.0.0",
        _serverPort         = 8080,
        _numberOfThreads    = 4,
        _announceInterval   = 3600,
        _maximumPeersToSend = 100
    }

    initialState <- createInitialState config

    flip runReaderT initialState $ do
        address <- Utils.getConfigField _serverAddress
        port    <- Utils.getConfigField _serverPort
        liftIO $ putStrLn $
            "Starting BitTorrent server on " ++ address ++ ":" ++ show port ++ ".."

        runUDPServer

    forever $ threadDelay $ 1000000 * 60

    return ()


createInitialState :: Config -> IO State
createInitialState config = State
    <$> return config
    <*> (STM.atomically $ STM.newTVar $ TorrentMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ ConnectionMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ [])


runUDPServer :: AppM ()
runUDPServer = bracket createSocket killThreadsUsingSocket $ \socket -> do
    numberOfThreads <- Utils.getConfigField _numberOfThreads

    createdThreadIds <- replicateM numberOfThreads $
        ask >>= liftIO . forkIO . runReaderT (acceptConnections socket)

    Utils.withThreadIds (++ createdThreadIds)


createSocket :: AppM Socket.Socket
createSocket = do
    serverAddress <- Utils.getConfigField _serverAddress
    serverPort    <- show <$> Utils.getConfigField _serverPort

    addrInfos <- liftIO $ Socket.getAddrInfo
        (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
        (Just serverAddress)
        (Just serverPort)

    -- Select the first option with IPv4
    let serverAddr = head $ filter (\addr -> Socket.addrFamily addr == Socket.AF_INET) addrInfos

    liftIO $ do
        socket <- Socket.socket
            (Socket.addrFamily serverAddr)
            Socket.Datagram
            Socket.defaultProtocol

        Socket.bind socket $ Socket.addrAddress serverAddr

        return socket


killThreadsUsingSocket :: Socket.Socket -> AppM ()
killThreadsUsingSocket socket = do
    threadIds <- Utils.getThreadIds

    liftIO $ do
        forM_ threadIds killThread
        Socket.close socket

    return ()


acceptConnections :: Socket.Socket -> AppM ()
acceptConnections socket =
    forever $ do
        (requestBytes, remoteAddress) <- liftIO $ Socket.recvFrom socket 2048

        response <- case Converters.bytesToRequest requestBytes of
            Left (unconsumedByteString, consumedBytes, errorMessage) ->
                return $ ErrorResponse $ ErrorResponseInner (TransactionID 0) "Invalid request"
            Right (_, _, request) ->
                 Handlers.handleRequest request remoteAddress

        liftIO $ Socket.sendTo socket (Converters.responseToBytes response) remoteAddress
