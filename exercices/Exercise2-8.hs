{-# LANGUAGE RecordWildCards #-}

import           Control.Monad                    ( forM_ )
import           Data.Attoparsec.ByteString.Char8 ( Parser )
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Attoparsec.Text             ( isEndOfLine )
import qualified Data.ByteString                  as BS
import qualified Data.List                        as List
import qualified Data.Vector                      as V
import           Numeric.LinearAlgebra            as LinAlg hiding ((<>))

import qualified Statistics.Sample                as Stats


data Sample = MkSample { digit  :: Int
                       , values :: Vector R
                       }
    deriving Show


loadData :: FilePath -> IO [Sample]
loadData fp = do
    contents <- BS.readFile fp
    either error pure $ P.parseOnly p contents

    where
        p :: Parser [Sample]
        p = (P.many1' r)

        r :: Parser Sample
        r = do
            vals <- P.count (1 + 16*16) (P.double <* P.many' P.space)
            _ <- P.takeTill (isEndOfLine) <* P.endOfLine
            pure $ MkSample (round $ head vals) (vector $ tail vals)
        

linearRegression :: [Sample] 
                 -> (Vector R -> Int)
linearRegression samples
    = let x = LinAlg.fromColumns $ [LinAlg.vector (replicate (length samples) 1)] ++ (LinAlg.toColumns $ LinAlg.fromRows $ values <$> samples)
          xT = LinAlg.tr' x
          y = LinAlg.vector $ (fromIntegral . digit) <$> samples
          coeffs = (LinAlg.inv (xT <> x) <> xT) #> y
      in \inp -> round (coeffs `dot` (vector [1] <> inp))


kNearestNeighbors :: Int -> [Sample] -> (Vector R -> Int)
kNearestNeighbors k samples = \x -> round
                                  $ Stats.mean
                                  $ V.fromList
                                  $ map (fromIntegral . digit)
                                  $ take k 
                                  $ reverse 
                                  $ List.sortOn (\MkSample{..} -> values `dot` x) samples


data Prediction a = MkPred { yhat :: a -- Prediction
                           , y :: a    -- Actual value
                           }

main :: IO ()
main = do
    train <- loadData "data/zipcode/zip.train"
    test  <- loadData "data/zipcode/zip.test"
    let trainSet = filter (\s -> digit s `elem` [2,3]) train
        testSet  = filter (\s -> digit s `elem` [2,3]) test
    
    let linRegModel = linearRegression trainSet
        linRegTrainPreds = [MkPred (linRegModel values) digit | MkSample{..} <- trainSet]
        linRegTestPreds =  [MkPred (linRegModel values) digit | MkSample{..} <- testSet]
    
    print $ "Linear Regression: error rate (train): " <> show (errorRate linRegTrainPreds) <> "%"
    print $ "Linear Regression: error rate (test): "  <> show (errorRate linRegTestPreds)  <> "%"
    putStrLn mempty
    
    forM_ [1,3,5,7,15] $ \k -> do
        let kNNModelTrain = kNearestNeighbors k trainSet
            kNNTrainPreds = [MkPred (kNNModelTrain values) digit | MkSample{..} <- trainSet]
            kNNTestPreds =  [MkPred (kNNModelTrain values) digit | MkSample{..} <- testSet]

        print $ "kNN (" <> show k <> "): error rate (train): " <> show (errorRate kNNTrainPreds) <> "%"
        print $ "kNN (" <> show k <> "): error rate (test): "  <> show (errorRate kNNTestPreds)  <> "%"
        putStrLn mempty
    where 
        errorRate :: Eq a => [Prediction a] -> Double
        errorRate preds = 100*(fromIntegral 
                        $ length 
                        $ filter (\MkPred{..} -> yhat /= y) preds) 
                        / fromIntegral (length preds)