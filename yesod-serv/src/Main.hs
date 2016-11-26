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

import           Control.Lens             (use, view, (%=))
import qualified Control.Monad.State.Lazy (get, put)
import           Data.Default             (def)
import           Data.IORef               (IORef, modifyIORef, newIORef, readIORef,
                                           writeIORef)
import qualified Data.Map                 as M
import           Universum
import           Yesod
import           Yesod.Form.Jquery

import           Model                    (Task (..), TodoList (..), TodoRequest (..),
                                           applyRequest, fromTDL, parseRequest)

----------------------------------------------------------------------------
-- Control
----------------------------------------------------------------------------

data TodoListRef = TodoListRef { fromTodoListRef :: IORef TodoList }

mkYesod "TodoListRef" [parseRoutes|
/ HomeR GET POST
|]

instance Yesod TodoListRef
instance RenderMessage TodoListRef FormMessage where
    renderMessage _ _ = defaultFormMessage
instance YesodJquery TodoListRef

todoForm :: Html -> MForm Handler (FormResult TodoRequest, Widget)
todoForm = renderDivs $ TodoRequest <$> areq textField "Request: " Nothing


instance MonadState TodoList (HandlerT TodoListRef IO) where
    put x = do
        ref <- fromTodoListRef <$> getYesod
        liftIO $ writeIORef ref x
    get = liftIO . readIORef =<< fromTodoListRef <$> getYesod

getHomeR :: Handler Html
getHomeR = do
    -- Generate the form to be displayed
    (widget, enctype) <- generateFormPost todoForm
    tdl <- use fromTDL
    composed widget enctype Nothing tdl

postHomeR :: Handler Html
postHomeR = do
    ((result, widget), enctype) <- runFormPost todoForm
    tdl <- use fromTDL
    header <- case result of
        FormSuccess req ->
            case parseRequest req of
                Nothing -> pure $ failureHeader "Couldn't parse command"
                Just k  -> do
                    fromTDL %= applyRequest k
                    pure $ successHeader
        _ -> pure $ failureHeader "Couldn't process the form"
    tdl' <- use fromTDL
    composed widget enctype (Just header) tdl'

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

composed widget enctype header tdl = defaultLayout $ do
    [whamlet|<p> TODOLIST v228.1|]
    fromMaybe (pure ()) header
    usage
    submitForm widget enctype
    tdlToHTML tdl


submitForm widget enctype = do
    [whamlet|
    <form method=post action=@{HomeR} enctype=#{enctype}>
        ^{widget}
        <br>
        <button>Submit
    <p> This place will contain info about todolists hopefully:
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

--todosToHTML :: [Task] -> Handler Html
tdlToHTML (M.assocs -> taskLists) = mapM_ (uncurry tasksToHTML) taskLists
  where
    tasksToHTML taskListName tasks =
        [whamlet|
        <p> #{taskListName}
        <ol>
            $forall Task taskName isDone <- tasks
                $if isDone
                   <li>
                       <font color="green">DONE
                       #{taskName}
                $else
                   <li>
                       <font color="red">TODO
                       #{taskName}

        |]
--    showTask Task {..} =
--        bool "TODO" "DONE" taskIsComplete <> " " <> taskText

usage =
    [whamlet|
     <p> List of commands:
     <ul>
       <li> addlist LISTNAME
       <li> addtask LISTNAME TASKNAME
       <li> rmlist LISTNAME
       <li> toggle LISTNAME TASKINDEX
    |]


----------------------------------------------------------------------------
-- Executable
----------------------------------------------------------------------------

main = warp 3000 =<< (TodoListRef <$> newIORef def)
