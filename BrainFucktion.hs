import Control.Monad.State.Lazy
import Data.Maybe
import Text.ParserCombinators.ReadP hiding(many,get)
import Control.Applicative
import System.Environment
import Data.Char


data Line =
    Var String
  | Fun String [String] Script
  | Call String [String]
  | LRead String
  | LWrite String
  | LDec String
  | LInc String
  | LLoop String Script
  deriving (Show)

type Script = [Line]

-- Progrms and Functions are intermidiate data types to simplifiy compilation

data Function =
    Dec Int
  | Inc Int
  | Read Int
  | Write Int
  | Loop Int Program
  deriving (Show)

type Program = [Function]


{-
 - Program to brainfuck
 -}

codeGen :: Program -> String
codeGen p = evalState (codeGen' p) 0

codeGen' :: Program -> State Int String
codeGen' = fmap concat . mapM codeGenF


codeGenF :: Function -> State Int String
codeGenF (Inc n)   = seekThen n "+"
codeGenF (Dec n)   = seekThen n "-"
codeGenF (Read n)  = seekThen n ","
codeGenF (Write n) = seekThen n "."
codeGenF (Loop n p) = do
  pre <- seek n -- get to loop position
  iner <- codeGen' p 
  loopRet <- seek n -- get back to loop start
  return $ pre++"["++iner++loopRet++"]" 

seekThen :: Int -> String -> State Int String
seekThen n w = fmap (++ w) $ seek n

seek :: Int -> State Int String
seek t = do
  s <- get
  put t >> return (case compare s t of
    LT -> take (t-s) $ cycle ">"
    GT -> take (s-t) $ cycle "<"
    EQ -> ""
      )

{-
 - Script -> Program
 -}

progGen :: Script -> Program
progGen s = let
  fm = makeFunMap s -- list of function names and programs
  in evalState (progGen' fm s) (0,[])

makeFunMap :: Script -> [(String,Program)]
makeFunMap s = execState (mapM funMapLine s) []

funMapLine :: Line -> State [(String,Program)] ()
funMapLine (Fun name args s) =  do
  let s' = (map Var args) ++ s -- initialize args as variables
  fm <- get -- get current function map
  let p = evalState (progGen' fm s') (0,[]) -- generate the program with current function map
  modify $ (:) (name,p)  -- add function to map
funMapLine _ = return () 

-- the state is the first free cell in memory and the variable to memory location map
progGen' :: [(String,Program)] -> Script -> State (Int,[(String,Int)]) Program
progGen' fm = fmap concat . mapM (progGenL fm)


progGenL :: [(String,Program)] -> Line -> State (Int,[(String,Int)]) Program
progGenL _  (Fun _ _ _) =  return [] -- function definitions don't produce code
progGenL _  (LRead  w)  = getMem w >>= return . return . Read 
progGenL _  (LWrite w)  = getMem w >>= return . return . Write
progGenL _  (LDec   w)  = getMem w >>= return . return . Dec
progGenL _  (LInc   w)  = getMem w >>= return . return . Inc
progGenL _  (Var name) = do
  (n,m) <- get
  put (n+1,(name,n):m)
  return [] -- no aditional code from variable declaration
progGenL fm (LLoop w s) = do
  p <- progGen' fm s 
  n <- getMem w
  return . return $ Loop n p
progGenL fm (Call name args) = do
  let p = fromJust $ lookup name fm
  (n,m) <- get
  as <- mapM getMem args -- get memory location for argument variablse
  put (n,m)
  return $ runWith (as ++ [n+1..]) p -- internally declared variables are placed at the earliest free memroy location

-- get's memory location of variable
getMem :: String -> State (Int,[(String,Int)]) Int
getMem name = do
  (_,m) <- get
  case lookup name m of
    Just n -> return n
    Nothing -> error "probably variable not initialized"

-- calls a program on different memory locations
runWith :: [Int] -> Program -> Program
runWith xs = map (runWithF xs)

runWithF :: [Int] -> Function -> Function
runWithF xs (Dec n)    = Dec   (xs !! n)
runWithF xs (Inc n)    = Inc   (xs !! n)
runWithF xs (Read n)   = Read  (xs !! n)
runWithF xs (Write n)  = Write (xs !! n)
runWithF xs (Loop n p) = Loop  (xs !! n) (runWith xs p)

{-
 - String -> Script
 -}

parseProg :: String -> Script
parseProg w = let
  parser = readP_to_S scriptParser
  parses = parser w
  finished = filter ((== "") . snd) parses
  in fst . head $ finished

scriptParser :: ReadP Script
scriptParser = many lineParser

restOfLine :: ReadP String
restOfLine = munch (/= '\n') <* char '\n'

lineParser :: ReadP Line
lineParser = skipSpaces >> (
  (string "Var "   >> restOfLine >>= return . Var    ) <|>
  (string "Read "  >> restOfLine >>= return . LRead  ) <|>
  (string "Write " >> restOfLine >>= return . LWrite ) <|>
  (string "Dec "   >> restOfLine >>= return . LDec   ) <|>
  (string "Inc "   >> restOfLine >>= return . LInc   ) <|>
  (string "Loop " >> do
    v <- restOfLine
    _ <- skipSpaces >> string "{\n"
    ls <- scriptParser
    _ <- skipSpaces >> string "}\n"
    return $ LLoop v ls )  <|>
  (string "Fun " >> do
    (name:args) <- fmap words restOfLine
    _ <- skipSpaces >> string "{\n"
    ls <- scriptParser
    _ <- skipSpaces >> string "}\n"
    return $ Fun name args ls )  <|>
  (do
  (c:_) <- look
  guard (isLower c) -- function names must be lowercase
  (name:args) <- fmap words restOfLine
  return $ Call name args
  )
        )

-- removes comments empty lines and trailing white space 
preProc :: String -> String
preProc = unlines . filter (not . null) . map removeTrailingSpace . map (fst . break (=='#')) . lines

removeTrailingSpace :: String -> String
removeTrailingSpace = reverse . dropWhile isSpace . reverse

{-
 - main
 -}

compile :: String -> String
compile = codeGen . progGen . parseProg . preProc

main :: IO ()
main = do
  [file] <- getArgs
  w <- readFile file
  putStrLn $ compile w
