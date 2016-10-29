module Boilerplate
       ( SimpleTweet (..)
       , toStatus
       , trashMetadata) where

import           Data.Ord          (comparing)
import qualified Data.Text         as T
import           Data.Time.Clock   (UTCTime)
import           Web.Twitter.Types (SearchMetadata (..), Status (..), User (..))


trashMetadata :: SearchMetadata
trashMetadata = SearchMetadata 0 0 "kek" Nothing 0 Nothing "kek" "kek" "kek"

data SimpleTweet = SimpleTweet
    { simpleText :: T.Text
    , simpleDate :: UTCTime
    , simpleId   :: Integer
    } deriving (Show)

instance Eq SimpleTweet where
    (==) a b = simpleId a == simpleId b

instance Ord SimpleTweet where
    compare = comparing simpleId

toStatus :: SimpleTweet -> Status
toStatus SimpleTweet{..} =
    Status
    { statusContributors = Nothing
    , statusCoordinates = Nothing
    , statusCreatedAt = simpleDate
    , statusCurrentUserRetweet = Nothing
    , statusEntities = Nothing
    , statusExtendedEntities = Nothing
    , statusFavoriteCount = 0
    , statusFavorited = Nothing
    , statusFilterLevel = Nothing
    , statusId = simpleId
    , statusInReplyToScreenName = Nothing
    , statusInReplyToStatusId = Nothing
    , statusInReplyToUserId = Nothing
    , statusLang = Nothing
    , statusPlace = Nothing
    , statusPossiblySensitive = Nothing
    , statusScopes = Nothing
    , statusQuotedStatusId = Nothing
    , statusQuotedStatus = Nothing
    , statusRetweetCount = 0
    , statusRetweeted = Nothing
    , statusRetweetedStatus = Nothing
    , statusSource = "kek"
    , statusText = simpleText
    , statusTruncated = False
    , statusUser = genUser simpleDate
    , statusWithheldCopyright = Nothing
    , statusWithheldInCountries = Nothing
    , statusWithheldScope = Nothing
    }

genUser :: UTCTime -> User
genUser date =
    User
    {
     userContributorsEnabled = False
    , userCreatedAt = date
    , userDefaultProfile = False
    , userDefaultProfileImage = False
    , userDescription = Nothing
    , userFavoritesCount = 0
    , userFollowRequestSent = Nothing
    , userFollowing = Nothing
    , userFollowersCount = 0
    , userFriendsCount = 0
    , userGeoEnabled = False
    , userId = 123
    , userIsTranslator = False
    , userLang = "RU"
    , userListedCount = 0
    , userLocation = Nothing
    , userName = "Kektext"
    , userNotifications = Nothing
    , userProfileBackgroundColor = Nothing
    , userProfileBackgroundImageURL = Nothing
    , userProfileBackgroundImageURLHttps = Nothing
    , userProfileBackgroundTile = Nothing
    , userProfileBannerURL = Nothing
    , userProfileImageURL = Nothing
    , userProfileImageURLHttps = Nothing
    , userProfileLinkColor = "Kektext"
    , userProfileSidebarBorderColor = "Kektext"
    , userProfileSidebarFillColor = "Kektext"
    , userProfileTextColor = "Kektext"
    , userProfileUseBackgroundImage = False
    , userProtected = False
    , userScreenName = "Kektext"
    , userShowAllInlineMedia = Nothing
    , userStatusesCount = 0
    , userTimeZone = Nothing
    , userURL = Nothing
    , userUtcOffset = Nothing
    , userVerified = False
    , userWithheldInCountries = Nothing
    , userWithheldScope = Nothing
    }
