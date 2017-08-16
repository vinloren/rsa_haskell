import System.Environment
import Data.List
import Data.Char
import System.IO
import RsaKit

read' ::  String -> Integer
read' x = read x

cmpt :: [String] -> String -> String
cmpt [] out = out
cmpt i out = cmpt (tail(i)) (out++head(i))

main = do
  args <-getArgs
  progName <- getProgName
  if (length(args) < 4)
    then
      putStrLn "You must suppply 3 filenames: input, pubkey/privkey, output plus action crypt/decry"
    else if (args!!3) == "crypt" 
           then crypt args
           else decrypt args
           
decrypt args = do
  putStrLn ("Input, key, output:\n"++(args!!0)++","++(args!!1)++","++(args!!2)++"\n") 
  handle <- openFile (args!!1) ReadMode
  contents <-hGetContents handle
  putStrLn ("key data:\n"++contents)
  let righe = lines contents
      ce = read'(righe!!1)
      mo = read'(righe!!2)
  hClose handle
  handle <- openFile (args!!0) ReadMode
  contents <-hGetContents handle
  let incri = lines contents
  let cri = [ (read' x) | x <-incri]
  let decri =  [powm x ce mo 1 | x <- cri]
  let plain = [cnvOut x [] | x <- decri]
  let comp = cmpt (tail(plain)) (head(plain))
  putStrLn comp
  writeFile (args!!2) comp
  hClose handle
 

crypt args = do
  putStrLn ("Input, key, output:\n"++(args!!0)++","++(args!!1)++","++(args!!2)++"\n")
  handle <- openFile (args!!1) ReadMode
  contents <-hGetContents handle
  putStrLn ("key data:\n"++contents)
  let righe = lines contents
      len = (read'(righe!!0) `div` 8)-1
      ce = read'(righe!!1)
      mo = read'(righe!!2)
  hClose handle 
  contents <- readFile (args!!0)
  handle <- openFile (args!!0) ReadMode
  cont <- hGetContents handle
  let aBlocks = getBlocks [[]] "" cont (fromInteger(len))
  print aBlocks
  let plain = [cnvIn x 0 | x <- aBlocks]
  print plain
  let crypt = [powm x ce mo 1 | x <- plain]
  let recs = [(show x) | x <- crypt]
  writeFile (args!!2) (unlines recs) 
  hClose handle

      


