module Web.BitTorrent.Tracker.Converters.Common where

import qualified Data.Binary.Get as Binary
import qualified Data.Sequence as Sequence


binaryGetMany getter = binaryGetMany' getter Sequence.empty
    where
        binaryGetMany' getter items = do
            empty <- Binary.isEmpty
            if empty
                then return items
                else do
                    item <- getter
                    binaryGetMany' getter (item Sequence.<| items)
