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


-- indexed map
mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f list = zipWith f [0..] list

slidingWindow :: Int -> [Complex Double] -> [[Complex Double]]
slidingWindow n [] = []
slidingWindow n samples = [samples++(repeat $ 0:+0)] ++ (slidingWindow n $ drop n $ samples)

-- helper functions for constantq computations
hannWindow :: Int -> Int -> Double
hannWindow n m = w*w
    where w = sin (pi * m' / (n'-1))
          m' = fromIntegral m
          n' = fromIntegral n

cq_exp :: Double -> Int -> Int -> Complex Double
cq_exp q k n = cis $ -2 * pi * q * (fromIntegral k) / (fromIntegral n)

-- http://academics.wellesley.edu/Physics/brown/pubs/cq1stPaper.pdf
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
            where x = ((hannWindow bwl i) :+ 0)*((cq_exp _qFactor i bwl))
                  bwl = binWindowLength bin
        -- memoized transformFactor using an array
        transformFactorMemoized :: Int -> Int -> Complex Double
        transformFactorMemoized bin i = get i bin
          where
            table = array (0, (maxI+1) * (maxBin+1))
                [ (x + y * (maxI+1), transformFactor y x)
                | y <- [0 .. maxBin]
                , x <- [0 .. maxI]
                , x < binWindowLength y
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

printHelpIfNeeded (Arguments {help = True}) defaults = printHelp defaults
printHelpIfNeeded Arguments {inputFilename = ""} defaults = printHelp defaults
printHelpIfNeeded Arguments {outputFilename = ""} defaults = printHelp defaults
printHelpIfNeeded _ _ = return ()

printHelp defaults = do
    name <- getProgName
    putStrLn $ "Usage: " ++ name ++ " [OPTIONS]... -i INPUTFILE -o OUTPUTFILE"
    putStrLn $ "Process INPUTFILE (in WAV format) and save the Constant Q transform spectrogram to OUTPUTFILE (png image format)."
    putStrLn $ "Options:"
    putStrLn $ "  -min FREQ\tSet minimum frequency to FREQ (default "++(show $ minFreq defaults)++")"
    putStrLn $ "  -max FREQ\tSet maximum frequency to FREQ (default "++(show $ maxFreq defaults)++")"
    putStrLn $ "  -b NUM\tSet the number of frequency bins in one octave (default "++(show $ octaveBins defaults)++")"
    putStrLn $ "  -p NUM\tSet the hop size (default "++(show $ hopSize defaults)++")"
    putStrLn $ "  -q NUM\tSet the Q factor (quality of frequency resolution) (default "++(show $ qFactor defaults)++")"
    putStrLn $ "  -h\tPrint a help message and exit"
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
    printHelpIfNeeded parsedArgs defaultArgs
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