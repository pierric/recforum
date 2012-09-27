{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.User where

import Import

getViewUserR :: UserId -> Handler RepHtml
getViewUserR uid = do
    defaultLayout $ do
        setTitle "User Infomation"
        $(widgetFile "user")
