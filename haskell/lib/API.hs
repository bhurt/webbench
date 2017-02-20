{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE TypeOperators      #-}

module API where

    import Data.Aeson
    import Data.Int (Int64)
    import Data.Text
    import Data.Typeable
    import GHC.Generics
    import Servant.API

    data Name = Name {
        firstName :: Text,
        middleName :: Maybe Text,
        lastName :: Text,
        title :: Maybe Text
    } deriving (Show, Read, Ord, Eq, Generic, Typeable)

    data Address = Address {
        streetAddress :: Text,
        city :: Text,
        state :: Text,
        zipcode :: Text
    } deriving (Show, Read, Ord, Eq, Generic, Typeable)
        
    data User = User {
        userId :: Int64,
        userName :: Name,
        address :: Address,
        phoneNumber :: Maybe Text,
        age :: Int,
        interests :: [ Text ]
    } deriving (Show, Read, Ord, Eq, Generic, Typeable)

    data SortBy =
        FirstName
        | LastName
        | City
        | State
        | Zipcode
        | Age
        deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Typeable)

    data Direction =
        Ascending
        | Descending
        deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Typeable)

    data Sorting = Sorting {
        sortBy :: SortBy,
        sortDir :: Direction
    } deriving (Show, Read, Ord, Eq, Generic, Typeable)
        
    type API = 
        Get '[JSON] Text
        :<|> "rest" :> "v1" :> UsersAPI

    type UsersAPI = "users" :> QueryParam "limit" Int
                            :> QueryParam "offset" Int
                            :> ReqBody '[JSON] [Sorting]
                            :> Post '[JSON] [User]
                    :<|> "users" :> Get '[JSON] Int64
                    :<|> "users" :> Capture "userId" Int64
                                    :> Get '[JSON] User

    -- A bunch of instance definitions we need

    instance ToJSON Name where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Name

    instance ToJSON Address where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Address

    instance ToJSON User where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON User

    instance ToJSON SortBy where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON SortBy

    instance ToJSON Direction where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Direction

    instance ToJSON Sorting where
        toEncoding = genericToEncoding defaultOptions

    instance FromJSON Sorting
