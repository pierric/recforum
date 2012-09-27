{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Article where

import Import
import Yesod.Form.Nic
import Yesod.Auth
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import Control.Monad(when,forM_)
import Control.Arrow(second)
import Data.List(nubBy)
import Data.Time
import Misc.Utils

getNewArticleR :: TopicId -> Handler RepHtml
getNewArticleR topicId = do
    _ <- requireAuthId
    (widget, encType) <- generateFormPost 
            (articleNewModForm Nothing Nothing Nothing Nothing)
    defaultLayout $ do
        setTitle "New Article"
        $(widgetFile "article-new")

postNewArticleR :: TopicId -> Handler RepHtml
postNewArticleR topicId = do
    login <- requireAuthId
    ((result, _), _) <- runFormPost 
            (articleNewModForm Nothing Nothing Nothing Nothing)
    case result of
        FormSuccess (RespArticle tit des prv drf) -> do
            now   <- liftIO getCurrentTime
            let article = Article topicId login now now drf prv tit des 0
            aid <- runDB $ insert article
            redirect (ViewArticleR aid) 
        FormFailure (msg:_) -> $(logDebug) msg
        _ -> $(logDebug) "ArticleNew failure."
    redirectErrorMsg MsgHdlArticleNewDataMissing

getViewArticleR :: ArticleId -> Handler RepHtml
getViewArticleR articleId = do
    article <- runDB $ get404 articleId
    login <- requireAuthId
    let isDraft   = articleDraft article
        isPrivate = articlePrivate article
    when ((isDraft || isPrivate) && login /= articleOwner article) $ do
        redirectErrorMsg MsgHdlArticleViewUnauthorized

    let authorized = login == articleOwner article
    (movWidget, movEncType) <- generateFormPost paragraphMovForm
    (modWidget, modEncType) <- generateFormPost 
            (articleNewModForm (Just $ articleTitle article)
                               (Just $ articleDescr article)
                               (Just isPrivate)
                               (Just isDraft))

    tfmt <- mkTimeFormator2
    paragraphList <- runDB $ do
        pents  <- selectList [ParagraphArticle ==. articleId, ParagraphActive ==. True]
                             [Asc ParagraphCindex]
        let oids = map (paragraphOwner . entityVal) pents
        owners <- flip mapM oids (\oid -> do
                    mo <- get oid
                    return (oid, userIdent $ processNonexistedUser mo))
        return $ zip pents owners

    contributorList <- runDB $ do
        pents   <- selectList [ParagraphArticle ==. articleId] []
        let aids = map (paragraphOwner . entityVal) pents
        authors <- mapM get aids
        owner   <- get login
        let all = (login, owner) : (zip aids authors)
        return $ map (second $ userIdent . processNonexistedUser) (nubBy (projEq fst) all)

    defaultLayout $ do
        setTitle (toHtml $ articleTitle article)
        addStylesheet $ StaticR css_haddock_css
        $(widgetFile "article-view")

postModArticleR :: ArticleId -> Handler RepHtml
postModArticleR articleId = do
    login <- requireAuthId
    article <- runDB $ get404 articleId
    if (articleOwner article /= login) then do
        redirectErrorMsg MsgHdlArticleModUnauthorized
    else do
        ((result, _), _) <- runFormPost (articleNewModForm Nothing Nothing Nothing Nothing)
        case result of
            FormSuccess (RespArticle tit des prv drf) -> do
                now <- liftIO getCurrentTime
                runDB $ update articleId [ArticleTitle   =. tit,
                                          ArticleDescr   =. des,
                                          ArticlePrivate =. prv,
                                          ArticleDraft   =. drf,
                                          ArticleModified=. now]
                redirect (ViewArticleR articleId)
            FormFailure (msg:_) -> $(logDebug) msg
            _ -> $(logDebug) "ArticleMod failure."
    redirectErrorMsg MsgHdlArticleModDataMissing

{------------------------------------------------------------------------------
  Move a paragraph
  POST
------------------------------------------------------------------------------}
postMovParagraphR :: Handler RepHtml
postMovParagraphR = do
    ((result,_),_) <- runFormPost paragraphMovForm
    case result of
        FormSuccess (pid, offset) -> do
            res <- runDB $ do
                       mp <- get pid
                       case mp of
                           Nothing    -> return (Left MsgHdlParagraphNotExist)
                           Just parag -> do
                               movParag pid parag offset
                               return (Right parag) 
            case res of
                Left msg    -> redirectErrorMsg msg
                Right parag -> redirect (ViewArticleR (paragraphArticle parag))
        FormFailure (msg:_) -> $(logWarn) msg
        _  -> $(logWarn) "Paragraph Move failure."
    redirectErrorMsg MsgHdlParagraphMovDataMissing
    where
    movParag _ _ 0 = return ()
    movParag pid parag ofs = do
        let cnt = abs ofs
            ci  = paragraphCindex parag
        ps <- if ofs < 0 
              then selectList [ParagraphArticle ==. paragraphArticle parag
                              ,ParagraphCindex  <.  ci
                              ,ParagraphActive  ==. True]
                              [Desc ParagraphCindex, LimitTo cnt]
              else selectList [ParagraphArticle ==. paragraphArticle parag
                              ,ParagraphCindex  >.  ci
                              ,ParagraphActive ==. True]
                              [Asc ParagraphCindex, LimitTo cnt]
        let newcis = ci : map (paragraphCindex . entityVal) ps
        liftIO $ putStrLn (show newcis)
        flip mapM_ (zip (ps ++ [Entity pid parag]) newcis) (\ (ent, eci) ->
            update (entityKey ent) [ParagraphCindex =. eci])


data RespArticle = RespArticle Text Html Bool Bool
   
articleNewModForm :: Maybe Text -> 
                     Maybe Html ->
                     Maybe Bool -> 
                     Maybe Bool -> 
                     Form RespArticle
articleNewModForm tit dsc pvt dft = renderDivs $ RespArticle
    <$> areq textField     
             (fieldSettings "Article title"){ fsId = Just "articleTit"} 
             tit
    <*> areq nicHtmlField
             (fieldSettings "Article description (optional)"){ fsId = Just "articleDsc"} 
             dsc
    <*> areq checkBoxField
             (fieldSettings "Mark it PRIVATE"){ fsId = Just "articlePvt"} 
             pvt
    <*> areq checkBoxField
             (fieldSettings "Mark it as DRAFT"){ fsId = Just "articleDft"}
             dft

paragraphMovForm :: Form (ParagraphId, Int)
paragraphMovForm = renderDivs $ (,)
    <$> areq hiddenField (fieldSettings "ParagraphId"){ fsId = Just "paragraphId" } Nothing
    <*> areq intField    (fieldSettings "Move Offset"){ fsId = Just "moveOffset"  } (Just 0)
    
