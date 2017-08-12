import System.Random
import Data.Char
import RsaKit


-- converte Int to Integer e somma a 256*b
intgr :: Int -> Integer -> Integer
intgr a b = 256*b+toInteger(a)

-- converte input string in Integer da cifrare RSA
cnvIn :: [Char] -> Integer -> Integer
cnvIn i n 
  | i == [] = n
  | otherwise = (cnvIn (tail(i)) (intgr(ord(head(i))) n))

-- converte big Integer in [char]
cnvOut :: Integer -> [Char] -> [Char]
cnvOut 0 a = a
cnvOut n a = cnvOut (n `div` 256) ((chr((fromInteger(n)) `mod` 256)):a)


 
main = do
  putStrLn "Num bits primo fattore primo?"
  nb <- getLine
  let e1 = read(nb)
  n <- randomRIO(2^e1, 2^(e1+1)-1)
  let p = if n  `mod` 2 == 1 then n else n + 1
  let fact1 = findPrime p
  putStrLn (show fact1)
  putStrLn "Num bits secondo  fattore primo?"
  nb <- getLine
  let e2 = read(nb)
  n <- randomRIO(2^e2, 2^(e2+1)-1)
  let p = if n  `mod` 2 == 1 then n else n + 1
  let fact2 = findPrime p
  putStrLn (show fact2)
  let m = (fact1*fact2):[]
  let phi = lcm (fact1-1) (fact2-1)
  let y = phi:m
  let c = findC phi
  let ce = c:y
  let d = findD c phi
  let z = d:ce
  let labl = ["Decyphexp","Cyphexp","phi","Module"]
  let rsa = zip labl z
  putStrLn "RSA packet:"
  print rsa
  putStrLn "Frase da cifrare?"
  n <- getLine
  let num = (cnvIn n 0)
  let cyph = powm num c (head m) 1
  putStrLn "Cypher ="
  putStrLn (show cyph)
  let decy = powm cyph d (head m) 1
  putStrLn "Decyph ="
  putStrLn (show decy)
  putStrLn (cnvOut decy [])
