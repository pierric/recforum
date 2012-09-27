module Handler.Paragraph where

import Import
import Yesod.Auth
import Yesod.Markdown
import Data.Time
import Data.Maybe(fromMaybe)
import Data.String(IsString (..))
import Control.Monad(liftM2)
import Misc.Utils
import Misc.Difference

{------------------------------------------------------------------------------
  New Paragraph
  GET POST
------------------------------------------------------------------------------}
getNewParagraphR :: ArticleId -> Int -> Handler RepHtml
getNewParagraphR aid cindex = do
    _ <- requireAuthId
    ma <- runDB $ get aid
    case ma of
        Nothing      -> redirectErrorMsg MsgHdlArticleNotExist
        Just article -> do
            (widget, encType) <- generateFormPost (paragraphNewModForm Nothing)
            defaultLayout $ do
                setTitle "Create"
                $(widgetFile "paragraph-new")

postNewParagraphR :: ArticleId -> Int -> Handler RepHtml
postNewParagraphR aid ci = do
    uid   <- requireAuthId
    ((result,_), _) <- runFormPost (paragraphNewModForm Nothing)
    case result of
        FormSuccess (content, summary) -> do
            now  <- liftIO getCurrentTime
            -- get maximum oindex of all the existent paragraphs
            -- update existent paragraphs, which has cindex >= ci
            res  <- runDB $ do
                        ma <- get aid
                        case ma of
                            Nothing -> return (Left MsgHdlArticleNotExist)
                            Just article -> do
                                let oi = articleMaxo article
                                update aid [ArticleMaxo +=. 1]
                                upd <- selectList [ParagraphArticle ==. aid, ParagraphCindex >=. ci] []
                                mapM_ (\item -> update (entityKey item) [ParagraphCindex +=. 1]) upd
                                pid <- insert (Paragraph uid now (unMarkdown content) summary aid oi ci 0 True)
                                return (Right pid)
            case res of
                Left msg -> redirectErrorMsg msg
                Right _  -> redirect (ViewArticleR aid)
        FormFailure (msg:_) -> $(logWarn) msg
        _ -> $(logDebug) "ParagraphNew failure."
    redirectErrorMsg MsgHdlParagraphNewDataMissing
    
{------------------------------------------------------------------------------
  Mod Paragraph
  GET POST
------------------------------------------------------------------------------}
getModParagraphR :: ParagraphId -> Handler RepHtml
getModParagraphR pid = do
    uid <- requireAuthId
    -- It is a must to give a explicit signature of runDB, 
    -- in order to help the type checker to use the App instance
    -- and typecheck the logDebug
    result <- (runDB :: YesodDB App App a -> Handler a) $ do 
        ma <- get pid
        case ma of
            Nothing -> do
                $(logWarn) (fromString $ "Paragraph [" ++  show pid ++ "] does not exist.")
                return $ Left MsgHdlParagraphNotExist
            Just a  -> do 
                let aid = (paragraphArticle a)
                mb <- get (paragraphArticle a)
                case mb of 
                    Nothing -> do
                        $(logWarn) (fromString $ 
                            "The article contains the specified Paragraph [" ++ 
                            show pid ++ "] does not exist.")
                        return $ Left MsgHdlArticleNotExist
                    Just b  -> do now  <- liftIO getCurrentTime
                                  succ <- tryUnlock pid uid now
                                  if not succ then 
                                      return $ Left MsgHdlParagraphLocked
                                  else do
                                      lock pid uid now
                                      i <- count [ParagraphArticle ==. aid
                                                 ,ParagraphActive  ==. True
                                                 ,ParagraphCindex  <=. (paragraphCindex a)]
                                      return $ Right (a,b,i)
    case result of
        Left msg -> redirectErrorMsg msg
        Right (parag, article, index) -> do
            (widget, encType) <- generateFormPost (paragraphNewModForm $ Just parag)
            defaultLayout $ do
                setTitle "Modify"
                $(widgetFile "paragraph-mod")

postModParagraphR :: ParagraphId -> Handler RepHtml
postModParagraphR pid = do
    uid    <- requireAuthId
    ((result,_), _) <- runFormPost (paragraphNewModForm Nothing)
    case result of
        FormSuccess (content, summary ) -> do
            now <- liftIO getCurrentTime
            {- Run a transaction to 
             - 0. check if the requested paragraph is valid
             - 1. check if the requested paragraph is locked
             - 2. if locked by other user, submition fails
             - 3. if locked by login user, submition succeed and unlock it.
             - 4. if not locked, submition succeed.
             -}
            res <- runDB $ do
                 mparag <- get pid
                 case mparag of
                    Nothing -> return (Left MsgHdlParagraphNotExist)
                    Just parag -> do
                        lck <- lockInfo pid uid
                        let newParag = Paragraph uid now 
                                         (unMarkdown content)
                                         summary
                                         (paragraphArticle parag)
                                         (paragraphOindex  parag)
                                         (paragraphCindex  parag)
                                         (paragraphVersion parag + 1)
                                         True
                        let submit = do update pid [ParagraphActive =. False]
                                        insert newParag
                        case lck of
                            LckOTH -> return $ Left MsgHdlParagraphLocked
                            NoLck  -> submit >> (return $ Right parag)
                            LckUSR key -> do 
                                key <- submit
                                unlock (LckPID key)
                                return (Right parag)
            case res of 
                Left msg    -> redirectErrorMsg msg
                Right parag -> redirect (ViewArticleR $ paragraphArticle parag)
        FormFailure (msg:_) -> $(logDebug) msg
        _ -> $(logDebug) "ParagraphMod failure."
    redirectErrorMsg MsgHdlParagraphModDataMissing

{------------------------------------------------------------------------------
  View the Paragraph of specific index and version.
  GET
------------------------------------------------------------------------------}
getViewParagraphR :: ArticleId -> Int -> Int -> Handler RepHtml
getViewParagraphR aid oindex version = do
    mpe <- runDB $ getBy (UniqueParagraphV aid oindex version)
    case mpe of
        Nothing -> redirectErrorMsg MsgHdlParagraphNotExist
        Just pe -> do
            let pid   = entityKey pe
                parag = entityVal pe
                aid   =  paragraphArticle parag
            ma <- runDB $ get aid
            case ma of
                Nothing -> redirectErrorMsg MsgHdlArticleNotExist
                Just article -> do
                    index <- runDB $ count [ParagraphArticle ==. aid
                                           ,ParagraphActive  ==. True
                                           ,ParagraphCindex  <=. (paragraphCindex parag)]     
                    defaultLayout $ do
                        setTitle "View paragraph"
                        $(widgetFile "paragraph-view")

getUnlockParagraphR :: ParagraphId -> Handler RepJson
getUnlockParagraphR pid = do
    uid <- requireAuthId
    ret <- runDB $  do
        lck <- lockInfo pid uid
        case lck of
            NoLck      ->                        return 0
            LckUSR lid -> unlock (LckLID lid) >> return 1
            LckOTH     ->                        return 2
    jsonToRepJson (ret :: Int)

{------------------------------------------------------------------------------
  List change history
  GET
------------------------------------------------------------------------------}
getHistParagraphR :: ParagraphId -> Handler RepHtml
getHistParagraphR pid = do
    mparag  <- runDB (get pid)
    case mparag of
        Nothing    -> redirectErrorMsg MsgHdlParagraphNotExist
        Just parag -> do
            let aid =  paragraphArticle parag
            ma <- runDB $ get aid
            case ma of
                Nothing      -> redirectErrorMsg MsgHdlArticleNotExist
                Just article -> do
                    (history, owners, index) <- runDB $ do
                        index  <- count [ParagraphArticle ==. aid
                                        ,ParagraphActive  ==. True
                                        ,ParagraphCindex  <=. (paragraphCindex parag)]
                        history<- selectList [ParagraphArticle ==. aid
                                             ,ParagraphOindex  ==. (paragraphOindex  parag)]
                                             [Desc ParagraphVersion]
                        owners <- let process id = do mu <- get id
                                                      return (id, userIdent (processNonexistedUser mu))
                                  in mapM (process . paragraphOwner . entityVal) history
                        return (history, owners, index)
                    tfmt <- mkTimeFormator2
                    idents <- sequence $ replicate (length history) newIdent
                    let histEntries = zip3 idents history owners
                    defaultLayout $ do
                        setTitle "History"
                        collectCB idents (map entityKey history)
                        $(widgetFile "history-view")
    where 
        collectCB idents pids = 
            let json = array $ flip map (zip idents pids)
                           (\(i,p) -> object [("ident", i), ("pid", toPathPiece (unKey p))])
            in toWidget [julius|var allCB = #{json};|]

{------------------------------------------------------------------------------
  Compare two paragraph
  GET
------------------------------------------------------------------------------}
getHistCompareR :: ParagraphId -> ParagraphId -> Handler RepHtml
getHistCompareR pid1 pid2 = do
    eps <- runDB $ do 
        mp1 <- get pid1
        mp2 <- get pid2
        case liftM2 (,) mp1 mp2 of
            Nothing      -> return $ Left MsgHdlParagraphNotExist
            Just (p1,p2) -> 
                if paragraphArticle p1 /= paragraphArticle p2 then
                    return $ Left MsgHdlCompareParagraphsFromDifferentArticle
                else do
                    ma <- get (paragraphArticle p2)
                    case ma of
                        Nothing -> return $ Left MsgHdlArticleNotExist
                        Just a  -> do 
                            i <- count [ParagraphArticle ==. paragraphArticle p2
                                       ,ParagraphActive  ==. True
                                       ,ParagraphCindex  <=. (paragraphCindex p2)]
                            return $ Right (p1,p2,a,i)
    case eps of
        Left msg -> redirectErrorMsg msg
        Right (p1,p2,article,index) -> do
                let widget = difference (paragraphContent p1) (paragraphContent p2)
                defaultLayout $ do
                    setTitle "Compare paragraphs"
                    differenceCSS
                    $(widgetFile "compare")
{------------------------------------------------------------------------------
 Delete a paragraph
 POST
------------------------------------------------------------------------------}
postDelParagraphR :: ParagraphId -> Handler RepHtml
postDelParagraphR pid = do
    res <- runDB $ do
               mp <- get pid
               case mp of
                   Nothing    -> return (Left MsgHdlParagraphNotExist)
                   Just parag -> do
                       update pid [ParagraphActive =. False]
                       return (Right parag)
    case res of
        Left msg    -> redirectErrorMsg msg
        Right parag -> redirect (ViewArticleR (paragraphArticle parag))

{------------------------------------------------------------------------------
  Forms
------------------------------------------------------------------------------}
paragraphNewModForm :: Maybe Paragraph -> Form (Markdown, Maybe Text)
paragraphNewModForm parag = renderDivs $ (,) 
                      <$> areq markdownField 
                               (fieldSettings "Content")  { fsId = Just "content"} 
                               (fmap (markdown . paragraphContent) parag)
                      <*> aopt textField 
                               (fieldSettings "Summary")  { fsId = Just "summary"}
                               Nothing

