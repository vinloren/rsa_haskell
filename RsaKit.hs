module RsaKit
( extEu
, findEu
, getQR
, invM
, testa
, testp
, findC
, powm
, findD
, findPrime
, intgr
, cnvIn
, cnvOut
) where

import Data.Char

-- trasforma [] in 0 per lista vuota (è il caso di num primo trovato
-- altrimenti lascia il resto > 1 che identifica num primo NON trovato
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

-- trova primi usando teorema di eulero p^(n-1)/2 mode p = 1/-1 se p e n primi
-- se p è primo passa il test su tutta la lista 2..37 ritornando [] altrimenti
-- la scansione termina non appena p^(n-1)/2 mode p != 1/-1. Il risultato è 
-- elevato al quadrato affiché -1 (p-1) diventi 1 e soddisfi x non > 1
testp :: Integer -> [Integer]
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]

-- calcolo potenza modulare col sistema dei quadrati in sequenza e shift a destra 
-- dell'esponente ad ogni iterazione   
powm :: Integer -> Integer -> Integer -> Integer -> Integer
powm b 0 m r = r
powm b e m r
  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r


-- Funzioni per trovare d = c^-1 mod phi (ovvero esponente di decifratura 'd' dato 'c' e 'phi')
-- col sistema algoritmo euclideo esteso
extEu :: [(Integer,Integer)] -> Integer -> Integer -> [(Integer,Integer)]
extEu a m c = ((getQR m c):(m,c):a) 

findEu :: [(Integer,Integer)] -> Integer -> Integer ->  [(Integer,Integer)]
findEu a _ 1 = a
findEu a _ 0 = [(0,0)]
findEu a m c = findEu res (snd (res !! 1)) (snd (res !! 0)) where res = (extEu a m c)

getQR :: Integer -> Integer -> (Integer,Integer)
getQR a b = ((a `div` b),(a `mod` b))
-- fine gruppo di funzioni
            
-- trova inverso modulo phi di c (c^1 mod phi) analizzando tuple risultanti da algoritmo 
-- euclideo applicato a phi e c che danno come MCD 1 finale. I passaggi sono 1 2 o 3 a 
-- seconda di phi e c ovvero 2 4 o 6 tuple (coppie) di valori
invM ::  [(Integer,Integer)] -> Integer
invM a | (length(a)) == 6 = ((-1*fst(a!!4)+(-1)*fst(a!!0)+(-1)*fst(a!!0)*(-1)*fst(a!!2)*(-1)*fst(a!!4)+fst(a!!5))) `mod` (fst(a!!5))
       | (length(a)) == 4 = ((-1*fst(a!!0)*(-1)*fst(a!!2)+1+fst(a!!3))) `mod` (fst(a!!3))
       | otherwise = ((-1*fst(a!!0)+fst(a!!1))) `mod` (fst(a!!1))

findC :: Integer -> Integer
findC phi = head [x | x <- [17,29,31,53,61,251], (gcd phi x) == 1]

findD :: Integer -> Integer -> Integer
findD c phi = (invM (findEu [] phi c))

findPrime p = if (testp p) == [0] then p else findPrime (p+2)

-- M = p1 * p2
-- phi = mcm (p1-1) (p2-1)
-- c = esponente di cifratura n. primo piccolo compreso  fra 17 e 251 deve essere coprimo con phi
-- d = esponente di decifratura = c^-1 mod phi ovvero d*c mod phi = 1
-- lo si trova con l'algoritmo euclideo esteso qui sopra (modinv / gcdExt)

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