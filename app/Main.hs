module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.HashMap.Strict as Map

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask, asks)
import Control.Monad.IO.Class (liftIO)

import Types.Server
import Utils (withTorrentMap, withConnectionMap)


main :: IO ()
main = do
    initialState <- createInitialState
    runReaderT f initialState


createInitialState :: IO State
createInitialState = State
    <$> (STM.atomically $ STM.newTVar $ TorrentMap Map.empty)
    <*> (STM.atomically $ STM.newTVar $ ConnectionMap Map.empty)


f :: AppM ()
f = do
    withTorrentMap $ Map.insert 1 [1..10]
    withConnectionMap $ Map.insert 1 1

    liftIO $ putStrLn "jawohl"
