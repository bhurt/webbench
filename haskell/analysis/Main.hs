{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Control.DeepSeq
    import Data.Int (Int64)
    import Data.List (foldl', sortBy)
    import Data.Map.Strict (Map)
    import Data.Text.Lazy (Text)
    import Text.Read (readMaybe)
    import Data.List (sort)

    import qualified Data.Map.Strict as Map
    import qualified Data.Text.Lazy as Text

    data Call = Call {
        start :: {-# UNPACK #-} !Double,
        end :: {-# UNPACK #-} !Double,
        succeeded :: !Bool,
        linenum :: {-# UNPACK #-} !Int
    } deriving (Eq)

    instance NFData Call where
        rnf call = rnf (start call)
                        `seq` rnf (end call)
                        `seq` rnf (succeeded call)
                        `seq` rnf (linenum call)
                        `seq` ()

    parseTimeSpec :: Text -> Maybe Double
    parseTimeSpec t =
            if Text.null tl then
                Nothing
            else
                go
                    <$> readMaybe (Text.unpack hd)
                    <*> readMaybe (Text.unpack (Text.drop 1 tl))
        where
            (hd, tl) = Text.breakOn "." (Text.strip t)
            go :: Int64 -> Int64 -> Double
            go secs nsecs = fromIntegral secs
                                + (fromIntegral nsecs / 1000000000.0)

    parseCall :: (Int, Text) -> Maybe Call
    parseCall (idx, t) =
        case Text.splitOn "," t of
            [ a, b ] -> Call <$> parseTimeSpec a <*> parseTimeSpec b
                            <*> pure True <*> pure idx
            (a : b : _) -> Call <$> parseTimeSpec a <*> parseTimeSpec b
                            <*> pure False <*> pure idx
            _ -> Nothing

    parseFile :: Text -> Maybe [Call]
    parseFile t = mapM (force . parseCall) (zip [1..] lns)
        where
            lns = filter (not . Text.null) $ Text.lines t


    normalizeTimes :: [Call] -> [Call]
    normalizeTimes [] = []
    normalizeTimes (x:xs) = sortBy cmp $ map fixtime (x:xs)
        where
            mintime = foldl' go (min (start x) (end x)) xs
            go t call = min (min t (start call)) (end call)
            fixtime call = call {   start = start call - mintime,
                                            end = end call - mintime }
            cmp call1 call2 = compare (start call1) (start call2)

    data Stats = Stats {
        count :: !Int,
        mean :: !Double,
        stddev :: !Double,
        least :: !Double,
        greatest :: !Double,
        median :: !Double,
        fifthPercentile :: !Double,
        ninetyfifthPercentile :: !Double
    }

    calcStats :: [Double] -> Stats
    calcStats [] = Stats {
                        count = 0,
                        mean = 0.0,
                        stddev = 0.0,
                        least = 0.0,
                        greatest = 0.0,
                        median = 0.0,
                        fifthPercentile = 0.0,
                        ninetyfifthPercentile = 0.0 }
    calcStats vals = Stats {
                        count = len,
                        mean = avg,
                        stddev = (sqrt (sigma / cnt)),
                        least = head sorted,
                        greatest = last sorted,
                        median = sorted !! (len `div` 2),
                        fifthPercentile = sorted !! (len `div` 20),
                        ninetyfifthPercentile = sorted !! ((len * 19) `div` 20)
                    }
        where
            len = length vals
            cnt = fromIntegral len
            tot = foldl' (+) 0.0 vals
            avg = tot / cnt
            sigma = foldl' (\s v -> s + (v - avg) * (v - avg)) 0.0  vals
            sorted = sort vals

    callTime :: Call -> Double
    callTime call = end call - start call

    statsHeader :: String
    statsHeader = "count,mean,\"stdev\",min,max,median,\"5th\
                    \ %ile\",\"95th %ile\""

    statsCsv :: Stats -> String
    statsCsv stats = mconcat [
                        show (count stats), ",",
                        show (mean stats), ",",
                        show (stddev stats), ",",
                        show (least stats), ",",
                        show (greatest stats), ",",
                        show (median stats), ",",
                        show (fifthPercentile stats), ",",
                        show (ninetyfifthPercentile stats) ]

    binCalls :: Foldable t => (Call -> t Int) -> [Call] -> Map Int [Call]
    binCalls f = foldl' binCall Map.empty
        where
            binCall m call = foldl' (go call) m (f call)
            go call m i = Map.alter go2 i m
                where
                    go2 Nothing = Just [ call ]
                    go2 (Just lst) = Just $ call : lst

    printGlobalCallStats :: [ Call ] -> IO ()
    printGlobalCallStats calls = do
            putStrLn "\"Global call stats:\"\n"
            putStrLn $ ",\"Total Number of Calls:\","
                        ++ show cnt 
            putStrLn $ ",\"Failure percentage:\","
                        ++ show ((100.0::Double) *
                                    (fromIntegral (length errs)) /
                                    (fromIntegral cnt))
            putStrLn ",\"Call time statistics:\""
            putStrLn gstats
        where
            cnt = length calls
            sucs = filter succeeded calls
            errs = filter (not . succeeded) calls
            gstats = mconcat [
                ",,type,", statsHeader,
                "\n,,all,", statsCsv (calcStats (map callTime calls)),
                "\n,,succeeded,",
                statsCsv (calcStats (map callTime sucs)),
                "\n,,failed,",
                statsCsv (calcStats (map callTime errs))
                ]

    printBinnedStats :: [Call] -> IO ()
    printBinnedStats calls = do
            putStrLn "\n,\"Call Time Statistic By Second:\""
            putStrLn $ ",,sec," ++ statsHeader ++ ",\"error rate\""
            mapM_ putStrLn (map makeCallStats (Map.toAscList bins))
        where
            bins = binCalls ((:[]) . truncate . start) calls
            makeCallStats (i, cs) = mconcat [
                ",,", show i, ",", statsCsv (calcStats (map callTime cs)),
                ",", show (errRate cs) ]
            errRate :: [Call] -> Double
            errRate cs = (fromIntegral
                                (length (filter (not . succeeded) cs)))
                            / (fromIntegral (length cs))

    printCallStats :: [Call] -> IO ()
    printCallStats calls = do
        printGlobalCallStats calls
        printBinnedStats calls

    main :: IO ()
    main = do
            putStrLn "Reading data from stdin..."
            cnts <- getContents
            case parseFile (Text.pack cnts) of
                Nothing ->
                    putStrLn "Failed to parse the input."
                Just calls ->
                    printCallStats (normalizeTimes calls)
