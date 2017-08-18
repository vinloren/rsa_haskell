## My implementation of RSA developed in haskell
I have recently coma across with Haskell language (even though it was born in 1998, I never had the chanche to know of it before) and 
was impressed by the completely new approach to programming it implements which is "functional descriptive" rather than "imperative" as 
C C++ Java Python Perl Ruby and many others are.

Another aspect peculiar to haskell is the way it manages 'Integers' type: an Integer in haskell can have a virtually unlimited size so 
there is no need of special libraries to manage big int numnbers such as those 1024 bit long or more.

Wanting to learn haskell, the first project I thougth of was to implement myself a typical RSA kit in plain haskell as I did in the 
past using C and MPIR libraries.


## The project
The project consists in three source files:

1) RsaKit.hs
2) genRSA.hs
3) criFile.hs

## RsaKit.hs
This files contains all the functions required by genRSA.hs and crifiles.hs. Most of the functions have to do with the tasks required to build 
a RSA framework:

1) finding a couple of (big) primes p,q to build the RSA 'Module' = p*q
2) finding 'phi' = lcm (p-1,q-1) to be used eventually to find out 'd' as 'decipher exponent'
3) choosing a 'cipher exponent' 'c' from a list of small primes checking 'c' is 'coprime' with 'phi' (gcd (c,phi = 1))
4) finding decipher exponent 'd'= c^1 mod phi (d = inverse module of 'c')

### 1 Finding (big) primes
This is achieved by findPrime, testp, testa, powm:

-- if testp passes then give back prime p else try again with p+2. p is a random odd number picked up by genRSA.hs randomRIO function<br>
findPrime p = if (testp p) == 0 then p else findPrime (p+2)

-- test if the result from testp was empty list in which case return 0, else head[xs]<br>
testa :: [Integer] -> Integer<br>
testa []     = 0<br>
testa xs = head xs<br>

-- find prime number by using euler n^(p-1)/2 mod p = 1/-1 being n prime coprime with p. If p is actually prime then the test has passed on the full list of 'n's returning 
the result of n^(p-1)/2 mod p is squared to ensure a positive 1 given back so that the condition x > 1 in the conditional list comprehension is valid also for -1^2<br>
testp :: Integer -> [Integer]<br>
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]<br>

-- compute modular power by using the quadratic sequence based on the least significant bit 
of the exponent. If 0 accumulate b^2 in b, if 1 accumulate b*r in r. The exp. 'e' is 
right shifted at each iteraction until it reaches 0 where the resul r is given back. 
This function will be used also to find c and d exponents and to cipher / decipher.<br>
<b>powm :: Integer -> Integer -> Integer -> Integer -> Integer<br>
powm b 0 m r = r<br>
powm b e m r<br>
nbsp;nbsp;  | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)<br>
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r<br></b>


### 2 Finding phi
Actually this is done in genRSA.hs since it is there the only place where phi is required. 
In is not eventually used anymore once 'c' and 'd' are found. The 'let phi = lcm (fact1-1) (fact2-1)'  
is simple as such since Haskell already supplies the lcm function natively.

### 3 Choosing a 'cipher exponent' 
Any small prime can be chosen as cipher exponent provided it is coprime with 'phi'.<br>
-- find 'x' being coprime with phi. It will become cipher exponent 'c'. A small list of primes suffices 
since one of them will be surely found to satisfy the condition in the list comprehension<br>
findC :: Integer -> Integer<br>
findC phi = head [x | x <- [17,29,31,53,61,251], (gcd phi x) == 1]<br>



