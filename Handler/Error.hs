{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Error where

import Import

getErrorR :: Handler RepHtml
getErrorR = defaultLayout mempty

getUnavailableR :: Handler RepHtml
getUnavailableR = setMessageI MsgNotImplemented >> defaultLayout mempty
