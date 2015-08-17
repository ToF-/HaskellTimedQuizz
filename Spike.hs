
import System.Time

main = do
    t0 <- getClockTime
    l <- getLine
    t1 <- getClockTime 
    let d = diffClockTimes t1  t0
    putStrLn $ show $ (l,tdSec d)
    main

