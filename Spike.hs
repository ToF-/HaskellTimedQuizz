
import System.Time

type Question = (String,String)
type Response = (String,Delay)
type Delay = Int

ask :: Question -> IO Response
ask (q,_) = do
    putStrLn q
    t0 <- getClockTime
    l <- getLine
    t1 <- getClockTime
    return (l,tdSec $ diffClockTimes t1 t0)

quizz :: [Question] -> IO [Response] 
quizz [] = return []
quizz (q:qs) = do
    r <- ask q
    rs <- quizz qs
    return (r:rs)

main = quizz $ map (\n -> ("mnemo pour le nombre: "++ show n,"idk")) [0..3] 

