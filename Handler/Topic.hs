{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Topic where

import Import
import Yesod.Form.Nic
import Yesod.Auth
import Data.Time
import Control.Monad
import Data.Maybe(fromMaybe)
import Data.String(IsString (..))

getNewTopicR :: Handler RepHtml
getNewTopicR = do
    _ <- requireAuthId
    (formWidget, formEnctype) <- generateFormPost 
            (topicNewModForm Nothing Nothing Nothing Nothing)
    defaultLayout $ do
        setTitle "New Topic"
        $(widgetFile "topic-new")

postNewTopicR :: Handler RepHtml
postNewTopicR = do
    login <- requireAuthId
    ((result, _), _) <- runFormPost
            (topicNewModForm Nothing Nothing Nothing Nothing)
    case result of
        FormSuccess (RespTopic tit des prv drf) -> do
            now   <- liftIO getCurrentTime
            let topic = Topic login now now drf prv tit des
            tid <- runDB $ insert topic
            redirect (ViewTopicR tid) 
        FormFailure (msg:_) -> $(logDebug) msg
        _ -> $(logDebug) "TopicNew Failure"
    setMessageI MsgHdlTopicNewDataMissing
    redirect ErrorR

data RespTopic = RespTopic Text Html Bool Bool

topicNewModForm :: Maybe Text ->
                   Maybe Html ->
                   Maybe Bool -> 
                   Maybe Bool ->
                   Form RespTopic
topicNewModForm mtit mdsc mpvt mdft = renderDivs $ RespTopic
    <$> areq textField     
             (fieldSettings "Topic title"){ fsId = Just "topicTit"} 
             mtit
    <*> areq nicHtmlField
             (fieldSettings "Topic Description(optional)"){ fsId = Just "topicDsc" }
             mdsc
    <*> areq checkBoxField
             (fieldSettings "Make it PRIVATE"){ fsId = Just "topicPvt" }
             mpvt
    <*> areq checkBoxField
             (fieldSettings "Mark it as DRAFT"){fsId = Just "topicDft" }
             mdft

fieldSettings :: String -> FieldSettings master
fieldSettings x = fromString x

getViewTopicR :: TopicId -> Handler RepHtml
getViewTopicR topicId = do
    ((param, paramWidget), paramEnctype) <- runFormGet topicViewForm
    let handle ofs srt = do 
            topic <- runDB $ get404 topicId
            login <- requireAuthId
            let isDraft   = topicDraft topic
                isPrivate = topicPrivate topic
            when ((isDraft || isPrivate) && login /= topicOwner topic) $ do
                setMessageI MsgHdlTopicViewUnauthorized
                redirect ErrorR
            (modWidget, modEncType) <- generateFormPost 
                    (topicNewModForm (Just $ topicTitle topic)
                                     (Just $ topicDescr topic)
                                     (Just isPrivate)
                                     (Just isDraft))
            articleList <- runDB $
                let dbsrt = case srt of
                              ByModified -> Asc ArticleModified
                              ByCreated  -> Asc ArticleCreated
                              ByOwner    -> Asc ArticleOwner
                in selectList [ArticleTopic ==. topicId] [dbsrt, OffsetBy ofs]
            articleOwnerList <- runDB $
                let ownerOf aid = do mnm <- get aid
                                     case mnm of 
                                       Nothing -> return "[unknown]"
                                       Just nm -> return (userIdent nm)
                in mapM (ownerOf . articleOwner . entityVal) articleList
            defaultLayout $ do
                setTitle (toHtml $ topicTitle topic)
                $(widgetFile "topic-view")
        defaultOfs = 0
        defaultSrt = ByModified
    case param of
        FormSuccess (ofs, srt) -> handle (fromMaybe defaultOfs ofs) 
                                         (fromMaybe defaultSrt srt)
        FormMissing            -> handle defaultOfs defaultSrt
        _ -> do
            $(logDebug) "TopicView failure."
            setMessageI MsgHdlTopicViewParameterCorrupt
            redirect ErrorR

data Sort = ByModified | ByCreated | ByOwner deriving Eq

topicViewForm :: Form (Maybe Int, Maybe Sort)
topicViewForm = renderDivs $ (,)
    <$> aopt intField "Page Beginning" (Just (Just 0))
    <*> aopt (selectField fieldSort) "Article Ordering" (Just (Just ByModified))
    where fieldSort = return $ mkOptionList 
            [Option "By Modified" ByModified "ByModified"
            ,Option "By Created"  ByCreated  "ByCreated"
            ,Option "By Owner"    ByOwner    "ByOwner"]

postModTopicR :: TopicId -> Handler RepHtml
postModTopicR topicId = do
    login <- requireAuthId
    topic <- runDB $ get404 topicId
    if (topicOwner topic /= login) then do
        setMessageI MsgHdlTopicModUnauthorized
        redirect ErrorR
    else do
        ((result, _), _) <- runFormPost 
                (topicNewModForm Nothing Nothing Nothing Nothing)
        case result of
            FormSuccess (RespTopic tit des prv drf) -> do
                now <- liftIO getCurrentTime
                runDB $ update topicId [TopicTitle   =. tit,
                                        TopicDescr   =. des,
                                        TopicPrivate =. prv,
                                        TopicDraft   =. drf,
                                        TopicModified=. now]
                redirect (ViewTopicR topicId)
            FormFailure (msg:_) -> $(logDebug) msg
            _ -> $(logDebug) "TopicMod failure."
    setMessageI MsgHdlTopicModDataMissing
    redirect ErrorR
