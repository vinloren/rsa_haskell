module RsaKit
( findC
, findD
, findPrime
, powm
, cnvIn
, cnvOut
, invM
, findEu
) where

import Data.Char

-- trasforma [] in 0 per lista vuota (è il caso di num primo trovato
-- altrimenti lascia il resto > 1 che identifica num primo NON trovato
testa :: [Integer] -> Integer
testa []     = 0
testa xs = head xs

-- trova primi usando teorema di eulero p^(n-1)/2 mode p = 1/-1 se p e n primi
-- se p è primo passa il test su tutta la lista 2..61 ritornando [] altrimenti
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
-- euclideo applicato a phi e c che danno come MCD 1 finale. I passaggi sono 1 2 o 3 a 
-- seconda di phi e c ovvero 2 4 6 o 8tuple (coppie) di valori
invM ::  [(Integer,Integer)] -> Int -> Integer -> Integer -> Integer
invM a l r0 r1
    | l == (length(a)) = (invM a (l-4) (-(fst(a!!(l-2)))*(-1)*(fst((a!!(l-4))))+1) (-(fst(a!!(l-2))))) 
    | l == 0 = ((r0 + (fst(a!!(length(a)-1)))) `mod` ((fst(a!!(length(a)-1)))))
    | otherwise = (invM a (l-2) (-(fst(a!!(l-2)))*r0+r1) r0)
    
     
-- fine gruppo di funzioni

-- trova 'x' che sia coprimo con phi. Esso diventa esponente di cifratura 'c'; solitamente 17 è ok altrimenti
-- potrà essere altro in elenco qui sotto
findC :: Integer -> Integer
findC phi = head [x | x <- [17,29,31,53,61,251], (gcd phi x) == 1]

-- dati c e phi trova reciproco di 'c' che sarà espèonente di decifratura 'd'
findD :: Integer -> Integer -> Integer
findD c phi = invM res (length(res)) 1 1 where res = (findEu [] phi c)

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