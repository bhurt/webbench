{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

    import API
    import Database.PostgreSQL.Simple
    import Data.Int (Int64)
    import Data.Pool
    import Servant
    import System.Environment
    import Control.Monad.IO.Class
    import Network.Wai.Handler.Warp (run)
    import Database.PostgreSQL.Simple.FromRow
    import Data.Text (Text)
    import Data.Vector (Vector)
    import Data.Foldable (toList)
    import Data.String (fromString)

    instance FromRow Name where
        fromRow = Name <$> field <*> field <*> field <*> field

    instance FromRow Address where
        fromRow = Address <$> field <*> field <*> field <*> field

    instance FromRow User where
        fromRow = User <$> field <*> fromRow <*> fromRow <*> field
                        <*> field <*> interests
            where
                interests = toList <$> (field :: RowParser (Vector Text))

    getUsers :: Pool Connection -> Maybe Int -> Maybe Int -> [Sorting]
                    -> Handler [User]
    getUsers pool limit offset sorting = withResource pool gotConnection
        where
            gotConnection conn =
                let query = fromString $ makeQuery in
                liftIO $ query_ conn query
            makeQuery =
                "SELECT * FROM  userview"
                    ++ orderClause sorting
                    ++ limitClause limit
                    ++ offsetClause offset
                    ++ ";"
            orderClause []        = ""
            orderClause xs        = " ORDER BY" ++ ordering False xs
            ordering _     []     = ""
            ordering comma (x:xs) = (if comma then "," else "")
                                        ++ columnName (sortBy x)
                                        ++ direction (sortDir x)
                                        ++ ordering True xs
            columnName FirstName  = " firstname"
            columnName LastName   = " lastname"
            columnName City       = " city"
            columnName State      = " state"
            columnName Zipcode    = " zipcode"
            columnName Age        = " age"
            direction Ascending   = " ASC"
            direction Descending  = " DESC"
            limitClause (Just n)  = " LIMIT " ++ show n
            limitClause Nothing   = ""
            offsetClause (Just n) = " OFFSET " ++ show n
            offsetClause Nothing  = ""

    getAUser :: Pool Connection -> Int64 -> Handler User
    getAUser pool uid = withResource pool gotConnection
        where
            gotConnection conn = do
                r <- liftIO $ query conn
                                "SELECT * FROM userview WHERE id = ? LIMIT 1;"
                                [uid]
                case r of
                    [] -> throwError err404
                    x : _ -> return x

    serverProxy :: Proxy API
    serverProxy = Proxy

    server :: Pool Connection -> Server API
    server pool = getUsers pool :<|> getAUser pool

    getEnvDef :: String -> String -> IO String
    getEnvDef name deflt = do
        r <- lookupEnv name
        case r of
            Just v -> do
                putStrLn $ "Using given value of " ++ name
                return v
            Nothing -> do
                putStrLn $ "Using the default value of " ++ name
                return deflt

    main :: IO ()
    main = do
        pghost <- getEnvDef "PGHOST" "localhost"
        pgport <- getEnvDef "PGPORT" "5432"
        pguser <- getEnvDef "PGUSER" "webbench"
        pgpass <- getEnvDef "PGPASS" "password"
        pgdb   <- getEnvDef "PGDATABASE" "webbench"
        let conninfo = ConnectInfo pghost (read pgport) pguser pgpass pgdb
        pool <- createPool (connect conninfo) close 1 600.0 1000
        putStrLn "Start server on port 3000"
        run 3000 (serve serverProxy (server pool))
