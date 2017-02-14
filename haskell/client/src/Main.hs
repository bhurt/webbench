{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

    import Control.Concurrent.Async
    import Control.Concurrent.Barrier (latchBarrier)
    import Control.Concurrent.STM
    import Control.Concurrent (threadDelay)
    import Control.DeepSeq
    import Control.Exception (evaluate)
    import Data.Aeson (encode, decode')
    import Data.ByteString.Lazy (ByteString)
    import Data.Default
    import Data.Maybe (isJust)
    import Network.HTTP.Client
    import Network.HTTP.Types.Method
    import Network.HTTP.Types.Status (statusCode)
    import qualified Data.ByteString.Char8 as Char8
    import qualified Network.HTTP.QueryString as QString
    import System.Clock
    import System.Console.GetOpt
    import System.Environment
    import System.Random

    import API

    data Call = Call {
        start :: TimeSpec,
        end :: TimeSpec,
        succeeded :: Bool
    } 

    randomSelect :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
    randomSelect (lb, ub) g = (toEnum i, g2)
        where
            (i, g2) = randomR ((fromEnum lb), (fromEnum ub)) g

    randomBounded :: (Enum a, Bounded a, RandomGen g) => g -> (a, g)
    randomBounded = randomSelect (minBound, maxBound)

    clientThread :: IO () -> Int -> Bool -> Request -> TVar Bool -> StdGen
                        -> IO [ Call ]
    clientThread barrier userCnt doDeepCheck basereq flag rgen = do
            manager <- newManager defaultManagerSettings
            barrier
            go manager rgen []
        where
            go manager r res = do
                let (r2, req) = makeReq r
                s <- getTime Monotonic
                resp <- httpLbs req manager
                _ <- evaluate $ force $ responseBody resp
                e <- getTime Monotonic
                let c = Call s e (checkResp resp)
                cont <- readTVarIO flag
                if cont then
                    go manager r2 (c:res)
                else
                    return (c:res)
            makeReq rand = (rand4, basereq {
                                        queryString = qstring,
                                        requestBody = reqbody })
                where
                    reqbody = RequestBodyLBS $ encode sorts
                    qstring = QString.toString $
                                QString.queryString [
                                    ("limit", Char8.pack (show limit)),
                                    ("offset", Char8.pack (show offset)) ]
                    (limit, rand1) = randomR (10, 100::Int) rand
                    (offset, rand2) = randomR (0, (userCnt - limit)) rand1
                    (numsorts, rand3) = randomR (1, 3::Int) rand2
                    (sorts, rand4) = makeSorts numsorts rand3
                    makeSorts n r
                        | n < 0 = ([], r)
                        | otherwise =
                            let (sb, r2) = randomBounded r in
                            let (dir, r3) = randomBounded r2 in
                            let (tl, r4) = makeSorts (n-1) r3 in
                            ((Sorting sb dir : tl), r4)
            checkResp resp =
                ((statusCode (responseStatus resp)) == 200)
                && (not doDeepCheck || (isJust (decodeBody resp)))
            decodeBody :: Response ByteString -> Maybe [ User ]
            decodeBody = decode' . responseBody

    data Arg = Arg {
        url :: String,
        deepcheck :: Bool,
        numSecs :: Int,
        numThreads :: Int,
        help :: Bool }
        deriving (Show, Read, Ord, Eq)

    instance Default Arg where
        def = Arg "http://localhost:3000/" False 1000 1 False

    optDesc :: [ OptDescr (Arg -> Arg) ]
    optDesc = [
        Option ['u'] ["url"] (ReqArg (\s a -> a { url = s }) "URL")
                                    "URL of the server to test",
        Option ['d'] ["deepcheck"] (NoArg (\a -> a { deepcheck = True }))
            "Perform deeper testing of results",
        Option ['s'] ["numsecs"]
            (ReqArg (\s a -> a { numSecs = read s }) "INT")
            "Number of seconds to test the program",
        Option ['n'] ["numclients"]
            (ReqArg (\s a -> a { numThreads = read s }) "INT")
            "Number of concurrent clients to run",
        Option ['h', '?'] ["help"] (NoArg (\a -> a { help = True }))
            "Print out help message" ]

    usage :: String
    usage = usageInfo "The WebBench client program." optDesc

    main :: IO ()
    main = do
            args <- getArgs
            let (fs, _, _, _) = getOpt' Permute optDesc args
            let r = (foldr (.) id fs) def
            if (help r) then
                putStrLn usage
            else
                do
                    flag <- newTVarIO False
                    request' <- parseRequest (url r)
                    let request = request' {
                                    path = "/rest/v1/users",
                                    method = methodPost }
                    cnt <- getCount request'
                    barrier <- latchBarrier (numThreads r)
                    results <- spawn_clients (numThreads r) (numSecs r) flag 0
                                (clientThread barrier cnt (deepcheck r)
                                    request flag)
                    putStrLn $ "Did " ++ show (length results) ++ " calls."
        where
            spawn_clients n nsecs flag g act
                | n > 0 =
                    withAsync (act (mkStdGen g))
                        (\a -> (++)
                                <$> spawn_clients (n-1) nsecs flag (g + 1) act
                                <*> wait a)
                | otherwise = do
                    threadDelay ((nsecs + 4) * 1000000)
                    atomically $ writeTVar flag True
                    return []
            getCount req = do
                manager <- newManager defaultManagerSettings
                resp <- httpLbs (req { path = "/rest/v1/users",
                                        method = methodGet })
                            manager
                case (decode' (responseBody resp)) of
                    Nothing -> error "Failed to get the count."
                    Just cnt -> return cnt


