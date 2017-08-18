module RsaKit
( findC
, findD
, findPrime
, powm
, cnvIn
, cnvOut
, invM
, findEu
, getBlocks
) where

import Data.Char


-- test if the result from testp was empty list in which case return 0, else head[xs]
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

-- find prime number by using euler n^(p-1)/2 mod p = 1/-1 being n prime coprime with p
-- if p is actually prime then the test has passed on the full list of 'n's returning []
-- the result of n^(p-1)/2 mod p is squared to ensure a positive 1 given back so that
-- the condition x > 1 in the conditional list comprehension is valid also for -1^2 
testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- compute modular power by using the quadratic sequence based on the least significant bit
-- of the exponent. If 0 accumulate b*b in b, if 1 accumulate b*r in r. The exp. 'e' is 
-- right shifted at each iteraction until it reaches 0 where the resul r is given back. 
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r


-- Le 4 funzioni per trovare d = c^-1 mod phi (ovvero esponente di decifratura 'd' dato 'c' e 'phi')
-- col sistema algoritmo euclideo esteso
extEu :: [(Integer,Integer)] -> Integer -> Integer -> [(Integer,Integer)]
extEu a m c = ((getQR m c):(m,c):a) 

-- ottieni la lista dei divisori / dividendi / resti della serie algoritmo euclideo MCD
findEu :: [(Integer,Integer)] -> Integer -> Integer ->  [(Integer,Integer)]
findEu a _ 0 = [(0,0)]
findEu a _ 1 = a
findEu a m c = findEu res (snd (res !! 1)) (snd (res !! 0)) where res = (extEu a m c)

-- ottieni quoziente e resto di a / b
getQR :: Integer -> Integer -> (Integer,Integer)
getQR a b = ((a `div` b),(a `mod` b))
            
-- trova inverso modulo phi di c (c^1 mod phi) analizzando tuple risultanti da algoritmo 
-- euclideo applicato a phi e c che danno come MCD 1 finale. La lista di coppie (q,r),(D,d)
-- viene analizzata a ritroso a partire da ultima equazione che riporta phi,c riducendo
-- via via le stesse fino a d arrivare alla soluzione c * c^-1 = 1 (c^-1 = exp decifratura)
-- La scansione termina quando il puntatore l arriva a 0 e appare il risultato
invM ::  [(Integer,Integer)] -> Int -> Integer -> Integer -> Integer
invM a l r0 r1
    | l == (length(a)) = (invM a (l-4) (-(fst(a!!(l-2)))*(-1)*(fst((a!!(l-4))))+1) (-(fst(a!!(l-2))))) 
    | l == 0 = ((r0 + (fst(a!!(length(a)-1)))) `mod` ((fst(a!!(length(a)-1)))))
    | otherwise = (invM a (l-2) (-(fst(a!!(l-2)))*r0+r1) r0)
-- fine gruppo di funzioni

-- find 'x' being coprime with phi. It will become cipher exponent 'c'. A small list of primes suffices 
-- since one of them will be surely found to satisfy the condition in the list comprehension
findC :: Integer -> Integer
findC phi = head [x | x <- [17,29,31,53,61,251], (gcd phi x) == 1]

-- dati c e phi trova reciproco di 'c' che sarà espèonente di decifratura 'd'
findD :: Integer -> Integer -> Integer
findD c phi = invM res (length(res)) 1 1 where res = (findEu [] phi c)

-- if testp passes then give back prime p else try again with p+2
-- p is a random odd number picked up by genRSA.hs randomRIO function
findPrime p = if (testp p) == [0] then p else findPrime (p+2)

-- M = p1 * p2
-- phi = mcm (p1-1) (p2-1)
-- c = esponente di cifratura n. primo piccolo compreso  fra 17 e 251: deve essere coprimo con phi
-- d = esponente di decifratura = c^-1 mod phi ovvero d*c mod phi = 1
-- lo si trova con l'algoritmo euclideo esteso qui sopra (invM / gcdExt)

-- converte Int to Integer e somma a 256*b
intgr :: Int -> Integer -> Integer
intgr a b = 256*b+toInteger(a)

-- converte input string in Integer da cifrare RSA
cnvIn :: [Char] -> Integer -> Integer
cnvIn i n 
  | i == [] = n
  | otherwise = (cnvIn (tail(i)) (intgr(ord(head(i))) n))

-- converte big Integer in [char] per ottenere input originale
cnvOut :: Integer -> [Char] -> [Char]
cnvOut 0 a = a
cnvOut n a = cnvOut (n `div` 256) ((chr((fromInteger(n)) `mod` 256)):a)

getBlocks :: [String] -> String -> String-> Int -> [String]
getBlocks ar s contents l 
  | (length(contents)) == 0 = tail(ar)++(s:[])
  | (length(s)) == l = (getBlocks (ar++(s:[])) "" contents l)
  | otherwise = (getBlocks ar (s++(head(contents):[])) (tail(contents))  l)