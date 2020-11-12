import Data.Maybe
import Chess; import Chess.FEN
import Minimax
import Control.Parallel.Strategies (using, parList, rseq)
import System.Environment (getArgs, getProgName)
import System.IO.Error (catchIOError, isUserError, isDoesNotExistError,
                        ioeGetFileName, isPermissionError)
import System.Exit (die)

-- Main entry point. Solves board, returns score (White wins) + best first move
solve :: Board −> (Int, [Char], Int)
solve brd = itDeep 1 8 brd

-- Formats the result into something readable
parse :: (Int, [Char], Int) −> [Char]
parse (scr, mv, qmv)
  | scr == 100 
      && odd qmv = intro ++ ", White highly favored," ++ conc
  | scr == 100 = intro ++ ", Black highly favored," ++ conc
  | scr > 0 = intro ++ ", White favored (" ++ show scr ++ ")" ++ conc
  | otherwise = intro ++ ", Black favored (" ++ show scr ++ ")" ++ conc
    where
      intro = "Best move: " ++ show mv
      conc = "search depth: " ++ show qmv

main :: IO()
main = do [ fil e n am e , c a s e s ] <− getArgs
c o n t e n t s <− readF ile fil e n am e
l e t b rd s = map (fromJust . fromFEN ) . take ( read c a s e s ) $ l in e s c o n t e n t s
r e s u l t s = map p a r s e (map s o l v e b rd s ‘ u sin g ‘ p a r Li s t r s e q )
sequence $ map putStrLn r e s u l t s
‘ ca tch IOE r ro r ‘ \ e −> do
pn <− getProgName
di e $ case ioeGetFileName e o f
Just f n | isDoesNotExistError e −> f n ++ ” : no such f i l e ”
| isPermissionError e −> f n ++ ” : P e rmi s si o n denied ”
| isUserError e −> ”Usage : ” ++ pn ++
” <fil e n am e> <# o f t e s t c a s e s>”
| otherwise −> show e
