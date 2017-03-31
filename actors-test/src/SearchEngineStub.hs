{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Stubs for search engines

module SearchEngineStub
       ( SearchEngineName
       , searchEngines
       , forkEngines
       ) where

import           Control.Concurrent        (forkIO, threadDelay)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Network.HTTP.Types.Status
import           System.Directory          (doesFileExist, listDirectory)
import           System.FilePath           (takeFileName, (</>))
import           System.Random             (randomRIO)
import           Universum
import           Web.Scotty

type SearchEngineName = Text

searchEngines :: [(SearchEngineName, String)]
searchEngines =
    [ ("SmartEngine", "http://localhost:3400/")
    , ("DumbEngine", "http://localhost:3401/")
    , ("SecureEngine", "http://localhost:3402/")
    ]

runEngine :: Bool -> Int -> (Text -> [Text]) -> IO ()
runEngine randomize port processReq = do
    scotty port $ do
        let err404 = status status404 >> text "Not found"
        post "/" $ do
            (req :: Text) <- jsonData
            when randomize $ do
                rand <- liftIO $ randomRIO (0,10::Int)
                liftIO $ threadDelay (rand * 1000000)
            json $ processReq req
            status status200
        notFound err404

findGrep :: FilePath -> IO (Text -> [Text])
findGrep dirPath = do
    putText $ "Opening " <> T.pack dirPath
    files <- filterM (doesFileExist . (dirPath </>)) =<< listDirectory dirPath
    withContent <- mapM (\fpath -> (takeFileName fpath,) <$> TIO.readFile (dirPath </>fpath)) files
    pure $ \req ->
        let matchingFiles =
                filter (\(_,contents) -> req `T.isInfixOf` contents) withContent
            matchingLines =
                map (second $ take 3 . filter (req `T.isInfixOf`) . T.lines) matchingFiles
        in concatMap (\(n,matches) -> map ((T.pack n <> ": ") <>) matches) $
            take 5 matchingLines

forkEngines :: Bool -> IO ()
forkEngines randomize = do
    resp1 <- findGrep "/home/volhovm/code/software-design-hw/actors-test/src/"
    resp2 <- findGrep "/home/volhovm/code/serokell/cardano-sl/src/Pos/"
    resp3 <- findGrep "/home/volhovm/code/software-design-hw/polish-parser/"
    void $ forkIO $ runEngine False 3400 resp1
    void $ forkIO $ runEngine randomize 3401 resp2
    void $ forkIO $ runEngine False 3402 resp3
