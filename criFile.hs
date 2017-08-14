import System.Environment
import Data.List
import Data.Char
import System.IO
import RsaKit

main = do
  let fact1 = (26687927510135686850969445340586726084895811115463540429028905740166549205290872261654932488398523122066419637496285998162228948937413724961035511196857039-1)
      fact2 = (22656941262280594759962795846007612272858156976383458492069077563709577663207832352094157380996292470210050264220769275496730643585293796550311253801710281-1)
      phi = (lcm fact1 fact2)
      m = (fact1+fact2+2)
      c = 17
      d = (findD c phi)
  print (findEu [] phi c)
  putStrLn ("d = \n"++(show(d)))
{--
  args <-getArgs
  progName <- getProgName
  if (length(args)) < 3
    then return ()
    else putStrLn ((args!!0)++","++(args!!1))
--}
  
 
 

