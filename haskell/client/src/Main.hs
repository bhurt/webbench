{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

    import Control.DeepSeq
    import Control.Exception (evaluate)
    import Data.ByteString.Lazy (ByteString)
    import Data.Maybe (isJust)
    import Network.HTTP.Client
    import Network.HTTP.Types.Status (statusCode)
    import qualified Data.ByteString.Char8 as Char8
    import qualified Network.HTTP.QueryString as QString
    import System.Clock
    import System.Console.GetOpt
    import System.Random
    import Control.Concurrent.STM
    import System.Environment
    import Data.Aeson (encode, decode')

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

    clientThread :: Bool -> Request -> StdGen -> TVar Bool -> IO [ Call ]
    clientThread doDeepCheck basereq rgen flag = do
            manager <- newManager defaultManagerSettings
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
                    (offset, rand2) = randomR (0, 1000000::Int) rand1
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

    data Arg =
        URL String
        | Deepcheck
        | NumSecs Int
        | NumThreads Int
        | Help
        deriving (Show, Read, Ord, Eq)

    optDesc :: [ OptDescr Arg ]
    optDesc = [
        Option ['u'] ["url"] (ReqArg URL "URL") "URL of the server to test",
        Option ['d'] ["deepcheck"] (NoArg Deepcheck)
            "Perform deeper testing of results",
        Option ['s'] ["numsecs"] (ReqArg (NumSecs . read) "INT")
            "Number of seconds to test the program",
        Option ['n'] ["numclients"] (ReqArg (NumThreads . read) "INT")
            "Number of concurrent clients to run",
        Option ['h', '?'] ["help"] (NoArg Help)
            "Print out help message" ]

    main :: IO ()
    main = do
        args <- getArgs
        let (r, s1, s2, s3) = getOpt' Permute optDesc args
        putStrLn $ "Results = " ++ show r
        putStrLn $ "s1 = " ++ show s1
        putStrLn $ "s2 = " ++ show s2
        putStrLn $ "s3 = " ++ show s3

