extEu :: [(Integer,Integer)] -> Integer -> Integer -> [(Integer,Integer)]
extEu a m c = ((getQR m c):(m,c):a) 

findEu :: [(Integer,Integer)] -> Integer -> Integer ->  [(Integer,Integer)]
findEu a _ 1 = a
findEu a _ 0 = [(0,0)]
findEu a m c = findEu res (snd (res !! 1)) (snd (res !! 0)) where res = (extEu a m c)

getQR :: Integer -> Integer -> (Integer,Integer)
getQR a b = ((a `div` b),(a `mod` b))
            
invM ::  [(Integer,Integer)] -> Integer
invM a | (length(a)) == 6 = ((-1*fst(a!!4)+(-1)*fst(a!!0)+(-1)*fst(a!!0)*(-1)*fst(a!!2)*(-1)*fst(a!!4)+fst(a!!5))) `mod` (fst(a!!5))
       | (length(a)) == 4 = ((-1*fst(a!!0)*(-1)*fst(a!!2)+1+fst(a!!3))) `mod` (fst(a!!3))
       | otherwise = ((-1*fst(a!!0)+fst(a!!1))) `mod` (fst(a!!1))