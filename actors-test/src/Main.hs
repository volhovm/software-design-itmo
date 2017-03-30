{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Main (main) where

import qualified Data.Map          as M
import           Universum
import           Yesod
import           Yesod.Form.Jquery

import           Actors

----------------------------------------------------------------------------
-- Control
----------------------------------------------------------------------------

data PseudoSearch = PseudoSearch

mkYesod "PseudoSearch" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod PseudoSearch
instance RenderMessage PseudoSearch FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodJquery PseudoSearch

todoForm :: Html -> MForm Handler (FormResult Text, Widget)
todoForm = renderDivs $ areq textField "Request: " Nothing

getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost todoForm
    composed widget enctype Nothing mempty

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost todoForm
    (searchRes,header) <- case result of
        FormSuccess (req :: Text) -> do
            resData <- liftIO $ searchData $ SearchRequest req
            pure (resData, successHeader)
        _ -> pure (mempty, failureHeader "Couldn't process the form")
    composed widget enctype (Just header) searchRes

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

composed widget enctype header (getSearchResponse -> searchResults) =
    defaultLayout $ do
        [whamlet|<p> TOP ACTOR-BASED SEARCH ENGINE|]
        fromMaybe pass header
        submitForm widget enctype
        searchResultsToHTML searchResults

submitForm widget enctype = do
    [whamlet|
    <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{widget}
        <br>
        <button>Submit
    <p> Responses:
    |]

successHeader =
    [whamlet|
    <p>
      <font color="green">Success! |]

failureHeader reason =
    [whamlet|
    <p>
      <font color="red">Failed to process request!
      <br>Reason: #{reason}
    |]

searchResultsToHTML m | M.null m = pass
searchResultsToHTML (M.assocs -> taskLists) =
    mapM_ (uncurry tasksToHTML) taskLists
  where
    tasksToHTML searchEngineName searchResults =
        [whamlet|
        <p> #{searchEngineName}
        <ol>
            $forall searchRes <- searchResults
                #{searchRes}
        |]

----------------------------------------------------------------------------
-- Executable
----------------------------------------------------------------------------

main = warp 11000 PseudoSearch
