import Data.Time.Format
import System.Locale
import Data.Time.Clock

type Question = (String,String)
type Response = (String,Delay)
type Delay = Int

ask :: Question -> IO Response
ask (q,_) = do
    putStrLn (q ++ " ?")
    t0 <- getCurrentTime
    l <- getLine
    t1 <- getCurrentTime
    return (l,round $ diffUTCTime t1 t0)

quizz :: [Question] -> IO [Response] 
quizz [] = return []
quizz (q:qs) = do
    r <- ask q
    rs <- quizz qs
    return (r:rs)

readQuestions :: IO [Question]
readQuestions = do 
    f <- readFile "questions.txt"
    return $ read f

writeResponses :: [(Question,Response)] -> String -> IO ()
writeResponses result name = writeFile name (show result) 

main = do
    qs <- readQuestions
    rs <- quizz qs
    t  <- getCurrentTime
    let name = "responses" ++ (formatTime defaultTimeLocale "%s" t) ++ ".txt"
    writeResponses (zip qs rs) name
