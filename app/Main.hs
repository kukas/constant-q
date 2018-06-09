module Main where

import Data.Complex
import Data.WAVE
import Graphics.Image hiding (magnitude, fft, map, sum, cis, zipWith, product, maximum)
import System.Environment
import System.Exit
import Debug.Trace
import Data.Array ((!), array)

-- image output
drawMatrix :: [[Double]] -> Image RPU Y Double
drawMatrix matrix = makeImageR RPU (width, height) (\(x, y) -> PixelY $ (matrix !! y !! (width-1-x))/max*2)
    where width = length (head matrix)
          height = length matrix
          max = maximum $ map maximum matrix

hannWindow :: Int -> Int -> Double
hannWindow n m = w*w
    where w = sin (pi * m' / (n'-1))
          m' = fromIntegral m
          n' = fromIntegral n

exp' :: Double -> Int -> Int -> Complex Double
exp' q k n = cis $ -2 * pi * q * (fromIntegral k) / (fromIntegral n)

-- precompexp :: Int -> Int -> (Int -> Int -> Complex Double)
-- precompexp q nmax = trace ("nmax"++show nmax) ([([exp' q k n | k <- [0..n]] !!) | n <- [0..nmax]] !!)
--     -- putStrLn $ "lol"++(show q)++(show nmax)

-- indexed map
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f list = zipWith f [0..] list

-- http://academics.wellesley.edu/Physics/brown/pubs/cq1stPaper.pdf
-- constantq :: Int -> Arguments -> [Complex Double] -> [[Double]]
-- constantq _ _ [] = []
-- constantq sr settings xs = [row] ++ (constantq sr settings (drop _hopSize xs))
--     where padxs = xs ++ (repeat (0:+0))
--           row = [magnitude $ 1/((fromIntegral n):+0) * sum (processSamples k n) | k <- [0..maxK], let n = nn k]
--           processSamples k n = zipWith (processSample k n) [0..(n-1)] padxs
--           processSample k n = (\i x -> (hannWindow n i :+ 0)*x*((exp' _qFactor i n)))

--           nn k = ceiling $ (fromIntegral sr)*(fromIntegral _qFactor)/(ff k)
--           ff k = 2 ** ((fromIntegral k)/_octaveBins) * _minFreq
--           maxK = ceiling $ log(_maxFreq/_minFreq)/log(2)*_octaveBins
--           precompexp' = precompexp _qFactor (nn 0)
--           _minFreq = minFreq settings
--           _maxFreq = maxFreq settings
--           _qFactor = qFactor settings
--           _octaveBins = fromIntegral $ octaveBins settings
--           _hopSize = hopSize settings

slidingWindow :: Int -> [Complex Double] -> [[Complex Double]]
slidingWindow n [] = []
slidingWindow n samples = [samples++(repeat $ 0:+0)] ++ (slidingWindow n $ drop n $ samples)

constantq :: Arguments -> [Complex Double] -> [[Double]]
constantq settings allsamples = map constq (slidingWindow _hopSize allsamples)
    where
        -- process one step (one column in the resulting image)
        constq :: [Complex Double] -> [Double]
        constq samples = [processBin bin samples | bin <- [0..maxBin]]
        -- process one row in the resulting image
        processBin :: Int -> [Complex Double] -> Double
        processBin bin samples = 1/bwl_f * (magnitude $ sum (mapi (processSample bin) (take bwl samples)))
            where bwl = binWindowLength bin
                  bwl_f = fromIntegral bwl
        processSample :: Int -> Int -> Complex Double -> Complex Double
        processSample bin i sample = (transformFactorMemoized bin i)*sample
        -- bin variables
        binWindowLength bin = ceiling $ _sampleRate*_qFactor/(binFrequency bin)
        binFrequency bin = 2 ** ((fromIntegral bin)/_octaveBins) * _minFreq
        maxBin = ceiling $ log(_maxFreq/_minFreq)/log(2)*_octaveBins
        transformFactor :: Int -> Int -> Complex Double
        transformFactor bin i = x
            where x = ((hannWindow bwl i) :+ 0)*((exp' _qFactor i bwl))
                  bwl = binWindowLength bin
        -- memoized transformFactor using an array
        transformFactorMemoized :: Int -> Int -> Complex Double
        transformFactorMemoized bin i = get i bin
          where
            table = array (0, (maxI+1) * (maxBin+1))
                [ (x + y * (maxI+1), transformFactor x y)
                | y <- [0 .. maxBin]
                , x <- [0 .. maxI]
                ]
            get x y = table ! (x + y * (maxI+1))
            maxI = binWindowLength 0
        -- settings
        _minFreq = minFreq settings
        _maxFreq = maxFreq settings
        _sampleRate = fromIntegral $ sampleRate settings
        _qFactor = fromIntegral $ qFactor settings
        _octaveBins = fromIntegral $ octaveBins settings
        _hopSize = hopSize settings

-- command line argument parsing
data Arguments = Arguments { inputFilename :: String
                           , outputFilename :: String
                           , sampleRate :: Int
                           , minFreq :: Double
                           , maxFreq :: Double
                           , qFactor :: Int
                           , octaveBins :: Int
                           , hopSize :: Int
                           , help :: Bool }


parseArgs :: [String] -> Arguments -> Arguments
parseArgs [] args = args
parseArgs ("-i":x:xs) args = parseArgs xs (args {inputFilename = x})
parseArgs ("-o":x:xs) args = parseArgs xs (args {outputFilename = x})
parseArgs ("-min":x:xs) args = parseArgs xs (args {minFreq = (read x :: Double)})
parseArgs ("-max":x:xs) args = parseArgs xs (args {maxFreq = (read x :: Double)})
parseArgs ("-b":x:xs) args = parseArgs xs (args {octaveBins = (read x :: Int)})
parseArgs ("-p":x:xs) args = parseArgs xs (args {hopSize = (read x :: Int)})
parseArgs ("-q":x:xs) args = parseArgs xs (args {qFactor = (read x :: Int)})
parseArgs ("-h":xs) args = parseArgs xs (args {help = True})

printHelpIfNeeded Arguments {help = True} = printHelp
printHelpIfNeeded Arguments {inputFilename = ""} = printHelp
printHelpIfNeeded Arguments {outputFilename = ""} = printHelp
printHelpIfNeeded _ = return ()

printHelp = do
    putStrLn $ "Hello, World!"
    exitSuccess



main :: IO ()
main = do
    args <- getArgs
    let defaultArgs = Arguments { inputFilename = ""
                                , outputFilename = ""
                                , sampleRate = 44100 -- dummy value, is set according to input file
                                , minFreq = 55
                                , maxFreq = 11000
                                , qFactor = 72
                                , octaveBins = 48
                                , hopSize = 1024
                                , help = False}
    let parsedArgs = parseArgs args defaultArgs
    printHelpIfNeeded parsedArgs
    -- mapM_ print $ slowdft [1,1,1,1,0,0,0,0]
    wave <- getWAVEFile (inputFilename parsedArgs)
    let channels = waveNumChannels $ waveHeader wave
    let samplerate = waveFrameRate $ waveHeader wave

    let parsedArgs' = parsedArgs { sampleRate = samplerate }

    let samples = map (\x -> sum (map sampleToDouble x) / (fromIntegral channels)) $ waveSamples wave
    let complexSamples = map (:+ 0) samples
    -- let windowSize = 1024
    -- let hopSize = 256
    -- let complexSampleWindows = slidingWindow complexSamples windowSize hopSize
    -- let hann = map (hannWindow windowSize) [0..windowSize]
    -- let cq = constantq samplerate 512 complexSamples
    let cq = constantq parsedArgs' complexSamples
    -- print cq
    writeImage (outputFilename parsedArgs') (drawMatrix cq)