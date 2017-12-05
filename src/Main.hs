import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS
import NNet
import Control.Monad
import Data.Functor
import Data.Ord
import Data.List
import System.Random

main = do
  [trainI, trainL, testI, testL] <- mapM ((decompress  <$>) . BS.readFile)
    [ "../data/train-images-idx3-ubyte.gz"
    , "../data/train-labels-idx1-ubyte.gz"
    ,  "../data/t10k-images-idx3-ubyte.gz"
    ,  "../data/t10k-labels-idx1-ubyte.gz"
    ]
  b <- newBrain [784, 30, 10]
  n <- (`mod` 10000) <$> randomIO
  putStr . unlines $
    take 28 $ take 28 <$> iterate (drop 28) (render <$> getImage testI n)

  let
    example = getX testI n
    bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
    cute d score = show d ++ ": " ++ replicate (round $ 70 * min 1 score) '+'
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM_ bs $ putStrLn . unlines . zipWith cute [0..9] . feed example

  putStrLn $ "best guess: " ++ show (bestOf $ feed example smart)

  let guesses = bestOf . (\n -> feed (getX testI n) smart) <$> [0..9999]
  let answers = getLabel testL <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
    " / 10000"
