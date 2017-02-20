{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

    import Control.Concurrent.Async
    import Control.Concurrent.Barrier (latchBarrier)
    import Control.Concurrent.STM
    import Control.Concurrent (threadDelay)
    import Control.DeepSeq
    import Control.Exception (evaluate, SomeException, displayException, catch)
    import Data.Aeson (encode, eitherDecode, decode')
    import Data.ByteString.Lazy (ByteString)
    import Data.Default
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

    data Result =
        Success
        | BadStatus Int
        | Failure String

    instance NFData Result where
        rnf Success = ()
        rnf (BadStatus x) = rnf x `seq` ()
        rnf (Failure x) = rnf x `seq` ()

    rnfTimeSpec :: TimeSpec -> ()
    rnfTimeSpec ts = rnf (sec ts) `seq` rnf (nsec ts) `seq` ()

    data Call = Call {
        start :: TimeSpec,
        end :: TimeSpec,
        succeeded :: Result
    } 

    instance NFData Call where
        rnf call = rnfTimeSpec (start call) `seq` rnfTimeSpec (end call)
                    `seq` rnf (succeeded call) `seq` ()

    showTimeSpec :: TimeSpec -> String
    showTimeSpec ts = show (sec ts) ++ (if nsec ts == 0 then "" else frac)
        where
            frac = '.' : replicate (9 - length ns) '0' ++ ns
            ns = show (nsec ts)

    
    randomSelect :: (Enum a, RandomGen g) => (a, a) -> g -> (a, g)
    randomSelect (lb, ub) g = (toEnum i, g2)
        where
            (i, g2) = randomR ((fromEnum lb), (fromEnum ub)) g

    randomBounded :: (Enum a, Bounded a, RandomGen g) => g -> (a, g)
    randomBounded = randomSelect (minBound, maxBound)

    data ClientState = ClientState {
        barrier :: IO (),
        userCnt :: Int,
        doDeepCheck :: Bool,
        basereq :: Request,
        flag :: TVar Bool,
        writeChannel :: TVar [[Call]] }

    writerThread :: FilePath -> ClientState -> TVar Bool -> IO ()
    writerThread filename cstate allDone = withFile filename WriteMode go
        where
            go handle =
                r <- atomically $ getCalls (writeChannel cstate)
                case r of
                    Nothing -> return ()
                    Just cs -> do
                        mapM_ writeCall cs
                        go handle
            getCalls chan = do
                cs <- readTVar chan
                if (cs == []) then
                    f <- readTVar allDone
                    if f then
                        return $ Nothing
                    else
                        retry
                else
                    do
                        writeTVar chan []
                        return $ concat $ reverse cs
            writeCall = hPutStrLn . showCall
            showCall call = showTimeSpec (start call) ++ ","
                            ++ showTimeSpec (end call)
                            ++ showResult (succeeded call)
            showResult Success = ""
            showResult (BadStatus st) = ",ERR,BAD STATUS: " ++ show st
            showResult (Failure err) = ",ERR," ++ show err
        
    clientThread :: ClientState -> StdGen -> IO ()
    clientThread cstate rgen = do
            manager <- newManager defaultManagerSettings
            (barrier cstate)
            let (n, rgen2) = randomR (0, 100::Int) rgen
            go manager rgen2 (1000 + n) []
        where
            go manager r n res = do
                let (r2, req) = makeReq r
                s <- getTime Monotonic
                succ1 <- catch (doCall manager req) handler
                e <- getTime Monotonic
                let succ2 = case succ1 of
                                Left succ3 -> succ3
                                Right resp -> checkResp resp
                let c' = Call s e succ2
                c <- evaluate $ force c'
                cont <- readTVarIO (flag cstate)
                if cont then
                    if n == 0 then
                        do
                            
                    else
                        go manager r2 (n-1) (c:res)
                else
                    return (c:res)
            doCall manager req = do
                resp <- httpLbs req manager
                _ <- evaluate $ force $ responseBody resp
                return $ Right resp
            handler :: SomeException -> IO (Either Result (Response ByteString))
            handler e = return . Left . Failure . ("Threw exception:" ++)
                            . displayException $ e
            makeReq rand = (rand4, (basereq cstate) {
                                        queryString = qstring,
                                        requestBody = reqbody })
                where
                    reqbody = RequestBodyLBS $ encode sorts
                    qstring = QString.toString $
                                QString.queryString [
                                    ("limit", Char8.pack (show limit)),
                                    ("offset", Char8.pack (show offset)) ]
                    (limit, rand1) = randomR (10, 100::Int) rand
                    (offset, rand2) = randomR (0, ((userCnt cstate) - limit))
                                        rand1
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
                let s = (statusCode . responseStatus) resp in
                if (s /= 200) then
                    BadStatus s
                else if (doDeepCheck cstate) then
                    case decodeBody resp of
                        Left err -> Failure $ "Decode failed: " ++ err
                        Right _ -> Success
                else
                    Success
            decodeBody :: Response ByteString -> Either String [ User ]
            decodeBody = eitherDecode . responseBody

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
                    cflag <- newTVarIO True
                    request' <- parseRequest (url r)
                    let request = request' {
                                    path = "/rest/v1/users",
                                    method = methodPost }
                    cnt <- getCount request'
                    cbar <- latchBarrier (numThreads r)
                    let cstate = ClientState cbar cnt (deepcheck r)
                                                request cflag
                    results <- spawn_clients (numThreads r) (numSecs r) 0
                                cstate
                    putStrLn $ "Did " ++ show (length results) ++ " calls."
        where
            spawn_clients n nsecs g cstate
                | n > 0 =
                    withAsync (clientThread cstate (mkStdGen g))
                        (\a -> (++)
                                <$> spawn_clients (n-1) nsecs (g + 1) cstate
                                <*> wait a)
                | otherwise = do
                    threadDelay (nsecs * 1000000)
                    atomically $ writeTVar (flag cstate) False
                    return []
            getCount req = do
                manager <- newManager defaultManagerSettings
                resp <- httpLbs (req { path = "/rest/v1/users",
                                        method = methodGet })
                            manager
                case (decode' (responseBody resp)) of
                    Nothing -> error "Failed to get the count."
                    Just cnt -> return cnt


