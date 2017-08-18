## My implementation of RSA developed in haskell
I have recently come across with Haskell language (even though it was born in 1998, I never had the chanche to know of it before) and 
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
<b>findPrime p = if (testp p) == 0 then p else findPrime (p+2)</b>

-- test if the result from testp was empty list in which case return 0, else head[xs]<br>
<b>testa :: [Integer] -> Integer<br>
testa []     = 0<br>
testa xs = head xs</b><br>

-- find prime number by using euler n^(p-1)/2 mod p = 1/-1 being n prime coprime with p. If p is actually prime then the test has passed on the full list of 'n's returning 
the result of n^(p-1)/2 mod p is squared to ensure a positive 1 given back so that the condition x > 1 in the conditional list comprehension is valid also for -1^2<br>
<b>testp :: Integer -> [Integer]<br>
testp p = [testa [x | b <- [2,3,5,7,11,13,17,19,23,31,37,41,53,61], let x = (powm b (p `div` 2) p 1)^2 `mod` p, x > 1]]</b><br>

-- compute modular power by using the quadratic sequence based on the least significant bit 
of the exponent. If 0 accumulate b^2 in b, if 1 accumulate b*r in r. The exp. 'e' is 
right shifted at each iteraction until it reaches 0 where the resul r is given back. 
This function will be used also to find c and d exponents and to cipher / decipher.<br>
<b>powm :: Integer -> Integer -> Integer -> Integer -> Integer<br>
powm b 0 m r = r<br>
powm b e m r<br>
<code>  </code> | e `mod` 2 == 1 = powm (b * b `mod` m) (e `div` 2) m (r * b `mod` m)<br>
powm b e m r = powm (b * b `mod` m) (e `div` 2) m r<br></b>


### 2 Finding phi
Actually this is done in genRSA.hs since it is there the only place where phi is required. 
In is not eventually used anymore once 'c' and 'd' are found. The <b>let phi = lcm (fact1-1) (fact2-1)</b> is simple as such since Haskell already supplies the lcm function natively.

### 3 Choosing a 'cipher exponent' 
Any small prime can be chosen as cipher exponent provided it is coprime with 'phi'. To find 'x' being coprime with phi to become cipher exponent 'c', a small list of primes suffices 
since one of them will be surely found to satisfy the condition in the list comprehension.<br>
<b>findC :: Integer -> Integer<br>
findC phi = head [x | x <- [17,29,31,53,61,251], (gcd phi x) == 1]</b><br>

### 4 Finding decipher exponent
The most challenging task here is to find c^-1 mod phi. There are different possibilities to achieve this 
spanning from brute force scan of multipliers [2,3..] until (d*c) mod phi = 1 (impractical being too slow) to 
c^(euler's totient -1) mod phi (too much complicated) or, better, to the use of the euclidean extended 
algorithm applied here.

Let's state that c^-1 * c mod phi = 1 mod phi, let't put c = 17 and phi = 860 to find d = 17^-1 mod 860.<br>
The extended euclidean algorithm works as it does to find the gcd between c and phi which we already know is = 1 
since c and phi are coprimes:<br>
0) 860 / 17 = 50 + 10<br>
1) 17 / 10 = 1 + 7<br>
2) 10 / 7 = 1 + 3<br>
3) 7 / 3 = 2 + 1 <code>   here we can stop since gcd has been found to be = 1</code>

We can now rewrite these 4 equations from the remainder perspective in reverse order:<br>
<table>
<tr><th> n </th><th> r </th><th> q </th><th> d </th><th> D </th></tr>
<tr><td> 0 </td><td> 1 </td><td> (-2) </td><td> 3 </td><td> 7 </td></tr>
<tr><td> 1 </td><td> 3 </td><td> (-1) </td><td> 7 </td><td>10 </td></tr>
<tr><td> 2 </td><td> 7 </td><td> (-1) </td><td> 7 </td><td>17 </td></tr>
<tr><td> 3 </td><td>10 </td><td>(-50) </td><td>17 </td><td>860</td></tr>
</table>

0) 1 = (-2) * 3 + 7
1) 3 = (-1) * 7 + 10<br>
2) 7 = (-1) * 7 + 17<br>
3) 10 = (-50) * 17 + 860<br>

Now we have to scan backwards these equations aiming to get a single final equation from which the value of 17^-1 
mod 860 can be extracted.

The first two equation having 





