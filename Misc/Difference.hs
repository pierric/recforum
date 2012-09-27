module Misc.Difference(difference, differenceCSS) where

import Import
import qualified Data.Text as T
import Data.Algorithm.Diff

difference :: Text -> Text -> Widget
difference t1 t2 = do
    let res = getDiff (T.lines t1) (T.lines t2)
    makeWidget res

makeWidget :: [(DI,Text)] -> Widget
makeWidget diff =
    let gd = flip map diff (\(di, text) -> case di of
                F -> ([hamlet|<div .cfirst>#{text}|],[hamlet|<div .blank>|])
                S -> ([hamlet|<div .blank>|],[hamlet|<div .csecond>#{text}|])
                B -> ([hamlet|<div .cequal>#{text}|],[hamlet|<div .cequal>#{text}|]))
    in [whamlet|
    $forall (w1,w2) <- gd
        <tr>
            <td>^{w1}
            <td>^{w2}
|]

differenceCSS :: Widget
differenceCSS = toWidget [lucius|
.cfirst, .csecond, .cblank, .cequal {
    border: 1px dashed black;
    border-radius: 3px 3px 3px 3px;
}
.cfirst {
    border-color: green;
}
.csecond {
    border-color: red;
}
|]
