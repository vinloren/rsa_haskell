import System.Random
import System.IO
import RsaKit

 
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
  let c = findC phi
  let ce = c:m
  let d = findD c phi
  let z = d:ce
  let zz = (e1+e2):z
  let labl = ["Bits","Decyphexp","Cyphexp","Module"]
  let rsa = zip labl zz
-- salva pubkey (bits,Cyphexp,Module) in 'pubkey.rsa' e privkey (Bits,Decyphexp,module) in 'privkey.rsa'
  writeFile "./pubkey.rsa" ((show(e1+e2))++"\n"++(show(c))++"\n"++(show(fact1*fact2)++"\n"))
  writeFile "./privkey.rsa" ((show(e1+e2))++"\n"++(show(d))++"\n"++(show(fact1*fact2)++"\n"))
  putStrLn "RSA packet:"
  print rsa
  putStrLn "Frase da cifrare?"
  n <- getLine
  let num = (cnvIn n 0)
  putStrLn (show(num))
  let cyph = powm num c (head m) 1
  putStrLn "Cypher ="
  putStrLn (show cyph)
  let decy = powm cyph d (head m) 1
  putStrLn "Decyph ="
  putStrLn (show decy)
  putStrLn (cnvOut decy [])
