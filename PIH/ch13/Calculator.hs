import qualified Parser as P
import System.IO

-- ### from chapter 10 ###
type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

writeat :: Pos -> String -> IO ()
writeat p xs = do
                goto p
                putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
        hSetEcho stdin False
        x <- getChar
        hSetEcho stdin True
        return x
-- ############

-- calculator box interface
box :: [String]
box =
  [ "+---------------+"
  , "|               |"
  , "+---+---+---+---+"
  , "| q | c | d | = |"
  , "+---+---+---+---+"
  , "| 1 | 2 | 3 | + |"
  , "+---+---+---+---+"
  , "| 4 | 5 | 6 | - |"
  , "+---+---+---+---+"
  , "| 7 | 8 | 9 | * |"
  , "+---+---+---+---+"
  , "| 0 | ( | ) | / |"
  , "+---+---+---+---+"
  ]

-- list of supported buttons
buttons :: String
buttons = standard ++ extra
            where standard = "qcd=123+456-789*0()/"
                  extra = "QCD \ESC\BS\DEL\n"

-- show the calculator
showbox :: IO ()
showbox = sequence_ [writeat (1,y) b | (y,b) <- zip [1..] box]

-- display a string in the calculator
-- first clear any remaining string, then show the new ones
display xs = do
                writeat (3,2) (replicate 12 ' ')
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
            display xs
            c <- getCh
            if elem c buttons 
            then process c xs
            else do
                 beep
                 calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC"      = quit
             | elem c "dD\BS\DEL"   = delete xs
             | elem c "=\n"         = eval xs
             | elem c "cC"          = clear
             | otherwise            = press c xs

-- possible actions
quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case P.parse P.expr xs of
            [(n, [])] -> calc (show n)
            _         -> do beep

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
        cls 
        showbox
        clear

main :: IO ()
main = run
