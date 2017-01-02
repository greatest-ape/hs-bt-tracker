import qualified Data.Sequence as Sequence
import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QC

import qualified Web.BitTorrent.Tracker.Converters as Converters

import Web.BitTorrent.Tracker.Types

import Web.BitTorrent.Tracker.Handlers.Announce (shuffleSequence)


main :: IO ()
main = putStrLn "No tests implemented yet"



-- * Handlers.Announce.shuffleSequence

testShuffleSequenceLength :: Eq a => Sequence.Seq a -> QC.Property
testShuffleSequenceLength xs = compareShuffledSequence xs Sequence.length


shuffleSequenceSortTest :: Ord a => Sequence.Seq a -> QC.Property
shuffleSequenceSortTest xs = compareShuffledSequence xs Sequence.unstableSort


compareShuffledSequence
    :: Eq a
    => Eq b
    => Sequence.Seq a
    -> (Sequence.Seq a -> b)
    -> QC.Property
compareShuffledSequence xs f = QC.monadicIO $ do
    shuffled <- QC.run $ shuffleSequence xs

    QC.assert $ f xs == f shuffled


-- * Request / Response converters

testRequestConverters :: Request -> Bool
testRequestConverters request1 =
    case Converters.bytesToRequest $ Converters.requestToBytes request1 of
        Right (_, _, request2) -> request1 == request2
        Left  (_, _, _       ) -> False


testResponseConverters :: Response -> Bool
testResponseConverters response1 =
    case Converters.bytesToResponse $ Converters.responseToBytes response1 of
        Right (_, _, response2) -> response1 == response2
        Left  (_, _, _        ) -> False
