module Misc.Period(Period, diffUTCTimeP, formatPeriod) where

import Prelude
import Data.Time
import Data.Text(Text)
import Data.String(fromString)

data Period = PeriodP { days   :: Integer
                      , seconds:: DiffTime }
            | PeriodF { days   :: Integer
                      , seconds:: DiffTime }
            | Now

-- always ensure that days and seconds are positive.
diffUTCTimeP :: UTCTime -> UTCTime -> Period
diffUTCTimeP t1 t2 = let dd = utctDay t1 `diffDays` utctDay t2
                         ds = utctDayTime t1 - utctDayTime t2
                     in if dd == 0 then 
                             if ds < 0 then PeriodP 0 (-ds) else PeriodF 0 ds
                        else if dd < 0 then
                                if ds > 0 then let dd' = (-dd) - 1
                                                   ds' = secondsDay - ds
                                               in PeriodP dd' ds'
                                else PeriodP (-dd) (-ds)
                        else {-- dd > 0 --}
                                if ds < 0 then let dd' = dd - 1
                                                   ds' = ds + secondsDay
                                               in PeriodF dd' ds'
                                else PeriodF dd ds
                    {--
                    in if dd == 0               then PeriodS ds
                       else if dd < 0 && ds > 0 then 
                                let dd' = dd + 1
                                    ds' = ds - secondsDay
                                in if dd' == 0 then PeriodS ds'
                                               else PeriodD dd' ds'
                       else if dd > 0 && ds < 0 then
                                let dd' = dd - 1
                                    ds' = ds + secondsDay
                                in if dd' == 0 then PeriodS ds'
                                               else PeriodD dd' ds'
                       else Period dd ds
                    --}

formatPeriod :: Integer -> Period -> Maybe Text
formatPeriod threshold Now = Just "Right Now"
formatPeriod threshold (PeriodP d s) =
    let (h,m) = getHM s
    in if d == 0 && h == 0 && m == 0 then Just "Just now"
       else if d <= threshold then Just (fromString $ day  d ++ hour h ++ minute m ++ "ago")
       else Nothing
formatPeriod threshold (PeriodF d s) =
    let (h,m) = getHM s
    in if d == 0 && h == 0 && m == 0 then Just "Just now"
       else if d <= threshold then Just (fromString $ day  d ++ hour h ++ minute m ++ "in the future")
       else Nothing

getHM :: DiffTime -> (Int, Int)
getHM dt = (h,m)
    where (h,s) = divMod (truncate dt :: Int) 3600
          (m,_) = divMod s 60

day 0    = ""
day 1    = "1 day "
day n    = show n ++ " days "
hour 0   = ""
hour 1   = "1 hour "
hour n   = show n ++ " hours "
minute 0 = ""
minute 1 = "1 minute "
minute n = show n ++ "minutes "

secondsDay = 24 * 3600
