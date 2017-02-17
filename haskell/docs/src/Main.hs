{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

    import Data.Default
    import Data.Int (Int64)
    import Data.Text (Text)
    import qualified API
    import qualified Data.Text as Text
    import qualified Data.Text.Lazy as ZText
    import qualified Text.Markdown as Markdown
    import Servant
    import Servant.Docs
    import Servant.JS
    import Text.Blaze.Html.Renderer.Text (renderHtml)
    import Servant.Swagger
    import Data.ByteString.Lazy.Char8 as ZChar8
    import qualified Data.Aeson.Encode.Pretty as Aeson
    import Data.Swagger -- .Internal.Schema

    api :: Proxy API.API
    api = Proxy

    instance ToSample API.User

    instance ToSample Int where
        toSamples _ = singleSample 3

    instance ToSample Int64 where
        toSamples _ = singleSample 1478

    instance ToSample Text where
        toSamples _ = singleSample "foo"

    instance ToSample API.Sorting

    instance ToSample API.SortBy

    instance ToSample API.Direction

    instance ToSample API.Name

    instance ToSample API.Address

    instance ToParam (QueryParam "limit" Int) where
        toParam _ = DocQueryParam "limit" []
                        "The maximum number of records to return."
                        Normal

    instance ToParam (QueryParam "offset" Int) where
        toParam _ = DocQueryParam "limit" []
                        "The offset of where to start returning records."
                        Normal

    instance ToCapture (Capture "userId" Int64) where
        toCapture _ = DocCapture "userId" "Id of the user record to fetch."
 
    instance ToSchema API.User

    instance ToSchema API.Name

    instance ToSchema API.Sorting

    instance ToSchema API.Address

    instance ToSchema API.SortBy

    instance ToSchema API.Direction

    intros :: [ DocIntro ]
    intros = [
        DocIntro "The WebBench API Documentation." []
        ]

    main :: IO ()
    main = do
        Prelude.writeFile "./webbench.md" $
            (markdown . docsWithIntros intros . pretty) api
        Prelude.writeFile "./webbench.html" $
            (ZText.unpack . renderHtml . Markdown.markdown def . ZText.pack
                . markdown . docsWithIntros intros . pretty) api
        writeJSForAPI api vanillaJS "./webbench.vanilla.js"
        writeJSForAPI api jquery "./webbench.jquery.js"
        writeJSForAPI api (angular defAngularOptions)
            "./webbench.angular.js"
        writeJSForAPI api (angularService defAngularOptions)
            "./webbench.angular-server.js"
        writeJSForAPI api (axios defAxiosOptions) "./webbench.axios.js"
        ZChar8.writeFile "./webbench.swagger.json" $ Aeson.encodePretty $
            toSwagger api
