module Misc.Utils where

import Import
import Data.Time
import System.Locale
import Data.String(IsString (..))
import Data.Maybe
import Yesod.Markdown
import Data.Text(pack, unpack)
import Text.Pandoc
import Text.Pandoc.Highlighting
import Misc.Period

projEq proj a b = proj a == proj b

processNonexistedUser = fromMaybe (User "[Nonexisted]")

fieldSettings = fromString

mkTimeFormator2 = do
    return (formatTime defaultTimeLocale "%b %d %Y, %H:%M")

redirectErrorMsg msg = do
    setMessageI msg
    redirect ErrorR

lockExpired = 1800

data LockType = LckPID ParagraphId
              | LckLID LockId
unlock (LckPID pid) = do
    deleteBy (UniqueLock pid)
unlock (LckLID lid) = do
    delete lid

lock pid uid now = do
    insert $ Lock pid uid now

data LockInfo = NoLck | LckUSR LockId | LckOTH
lockInfo pid uid = do
    mlock <- selectFirst [LockParagraph ==. pid] []
    case mlock of
        Nothing   -> return NoLck
        Just lock -> return $ if lockUser (entityVal lock) == uid 
                              then LckUSR (entityKey lock)
                              else LckOTH

tryUnlock pid uid now = do
    mlock <- selectFirst [LockParagraph ==. pid] []
    case mlock of
        Nothing   -> return True
        Just lock -> let elapsed = diffUTCTime now $ lockTime $ entityVal lock
                     in if elapsed >= lockExpired || uid == lockUser (entityVal lock) then do
                            unlock $ LckLID (entityKey lock)
                            return True
                        else
                            return False

markdown s = Markdown (unpack s)
unMarkdown (Markdown s) = pack s

renderMarkdown = writePandoc writerOptions . parseMarkdown yesodDefaultParserState
    where
    writerOptions = yesodDefaultWriterOptions {
                        writerHighlightStyle = haddock,
                        writerHTMLMathMethod = MathML Nothing }

{--
instance ToJavascript Value where
    toJavascript = fromValue
--}
