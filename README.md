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
<table>
<tr><td>0</td><td>860 / 17</td><td>= 50 + 10</td></tr>
<tr><td>1</td><td> 17 / 10</td><td>= 1 + 7</td></tr>
<tr><td>2</td><td> 10 / 7</td><td>= 1 + 3</td></tr>
<tr><td>3</td><td> 7 / 3</td><td>= 2 + 1</td></tr> 
</table>
here we can stop since gcd has been found to be = 1

We can now rewrite these 4 equations from the remainder perspective in reverse order:<br>
<table>
<tr><th> n </th><th> r </th><th>  q</th><th> d </th><th>D </th></tr>
<tr><td> 0 </td><td> 1 </td><td> (-2) </td><td> 3 </td><td>  7</td></tr>
<tr><td> 1 </td><td> 3 </td><td> (-1) </td><td> 7 </td><td> 10</td></tr>
<tr><td> 2 </td><td> 7 </td><td> (-1) </td><td> 7 </td><td> 17</td></tr>
<tr><td> 3 </td><td>10 </td><td>(-50) </td><td>17 </td><td>860</td></tr>
</table>

If we watch the sequence of these equation we can detect a pattern useful to solve the reduction of the entire 
list to one single row. Let's then write a new table substituting the values with labels (c = 17, phi=860):

<table>
<tr><td>n0=</td><td>(-q0)*n1 + n2</td></tr>
<tr><td>n1=</td><td>(-q1)*n2 + n3</td></tr>
<tr><td>n2=</td><td>(-q2)*n3 + c</td></tr>
<tr><td>n3=</td><td>(-q3)*c + phi</td></tr>
</table>

Now we have to scan backwards these equations, starting with the last one, aiming to get a single final equation from which the value of 17^-1 
mod 860 can be extracted.

In equation n3 and n2 we have to consider just the multipliers of 'c' (-q3) in n3 and 1 in n2 since phi can 
be omitted because all the calculation are 'mod phi' and phi mod phi is = 0. That said we can rewrite:

<table>
<tr><td>n0=</td><td>(-q0)*n1 + n2</td></tr>
<tr><td>n1=</td><td>(-q1)*n2 + n3</td></tr>
<tr><td>n2=</td><td>(-q2)*(-q3) + 1</td></tr>
<tr><td>n3=</td><td>(-q3)</td></tr>
</table>

Then:
<table>
<tr><td>n0=</td><td>(-q0) * n1 + n2</td></tr>
<tr><td>n1=</td><td>(-q1) * (-q2) * (-q3) - q1 + q3</td></tr>
<tr><td>n2=</td><td>(-q2) * (-q3) + 1</td></tr>
</table>

And finally:
<table>
<tr><td>n0=</td><td>(-q0) * (-q1) * (-q2) * (-q3) + (-q0) * (q1) + (-q0) * (-q3) + 51</td></tr>
</table>

<b>Substituting the values to the labels we have:</b><br>
(-2) * (-1) * (-1) * (-50) + (-2) * (-1) + (-2) * (-50) + 49 =<br> 
100 + 2 + 100 + 51 = <br>
(253 + 860) mod 860 = 253 (summed result to module since it may be negative)<br>
<b>so 17^-1 mod 860 = 253 since (17*253) mod 860 = 1</b>

### Haskell implementation
The relevant functions in RsaKit.hs to manage all that was explained above are:<br>
-- Extended euclidean algorithm. The result is a list of couples (q,r),(d,D) from (q,1),(D,d) up to 
(q,r),(phi,c)<br>
<b>extEu :: [(Integer,Integer)] -> Integer -> Integer -> [(Integer,Integer)]<br>
extEu a m c = ((getQR m c):(m,c):a)</b><br>

-- Get the list of divisors / dividends / remainder of the eucliden gcd algorithm<br>
<b>findEu :: [(Integer,Integer)] -> Integer -> Integer ->  [(Integer,Integer)]<br>
findEu a _ 0 = [(0,0)]<br>
findEu a _ 1 = a<br>
findEu a m c = findEu res (snd (res !! 1)) (snd (res !! 0)) where res = (extEu a m c)</b><br>

-- Get quotient and remainder of a / b<br>
<b>getQR :: Integer -> Integer -> (Integer,Integer)<br>
getQR a b = ((a `div` b),(a `mod` b))</b><br>
            
-- Find out module inverse c of phi (c^-1 mod phi) analyzing the resulting couples in list a gotten from findEu applied to phi and c. 
The list of couples (q,r),(D,d) is scanned backwards from the last two double couples starting from the last equation that includes phi,c. The reduction 
works step by step until the list is void in which case we got the solution c * c^-1 = 1 (c^-1 = decipher exp)<br>
<b>invM ::  [(Integer,Integer)] -> Int -> Integer -> Integer -> Integer<br>
invM a l r0 r1<br>
<code>    </code> | l == (length(a)) = (invM a (l-4) (-(fst(a!!(l-2))) * (-1) * (fst((a!!(l-4))))+1) (-(fst(a!!(l-2)))))<br>
<code>    </code> | l == 0 = ((r0 + (fst(a!!(length(a)-1)))) `mod` ((fst(a!!(length(a)-1)))))<br>
<code>    </code> | otherwise = (invM a (l-2) (-(fst(a!!(l-2)))*r0+r1) r0)</b><br>
<br>
<br>
<br>
## genRSA.hs
This files contains the program that willpermit the creation of a set of a cipher pairs (public and private) 
saving them in two files (pubkey.rsa and privkey.rsa) for eventual use. All the functions needed to 
carry out the task are imported from module RsaKit.hs already discussed.

The program will start asking the user to set the bit length of the two primes from which the key pair will 
be created (usually 512 or 1024 bit length for each prime for a 1kb or 2kb module respectively) then the generation 
process takes place following the steps:<br>

1) picking up a randomRIO(2^e, 2^(e+1)-1) odd integer p<br>
2) picking up a randomRIO(2^e, 2^(e+1)-1) odd integer q<br>
3) let fact1 = findPrime p<br>
   let fact2 = findPrime q<br>
   let m = (fact1*fact2) -- m = module <br>
   let phi = lcm (fact1-1) (fact2-1) <br>
   let c = findC  -- c = cipher exp.<br>
   let d = findD c phi<br>
4) save module numbits,cipher exp.,module in file pubkey.rsa<br>
5) save module numbits,decipher exp.,module in file privkey.rsa<br>
6) prompt the request for a sample sentence to be ciphred
7) show the ciphered text then decipher to let the user verify the sentence to be identical to the one responded to the prompt.

### 6 cipher a sentence
<b> putStrLn "Frase da cifrare?"<br>
 n <- getLine<br>
 let num = (cnvIn n 0)<bt>
 putStrLn (show(num))<br>
 let cyph = powm num c (head module) 1<br>
 putStrLn "Cypher ="<br>
 putStrLn (show cyph)</b><br>
The sentence (it's length should be 1 byte less than module length) responded to the test prompt will be transformed in a 
integer to be eventually ciphred:<br>
-- convert Int to Integer and sum it to 256*b<br>
<b>intgr :: Int -> Integer -> Integer<br>
intgr a b = 256*b+toInteger(a)</b><br>

-- convert input string to Integer to be ciphred RSA<br>
<b>cnvIn :: [Char] -> Integer -> Integer<br>
cnvIn i n <br>
<code>  </code>| i == [] = n<br>
<code>  </code>| otherwise = (cnvIn (tail(i)) (intgr(ord(head(i))) n))</b><br>
'i' is the input string, 'n' is the rsulting Integer. Each byte belonging to the string is summed up to 
'n' left shifted 8 bits (initially = 0) so that the resulting integer is ready as 'i' has become void.<br>

### 7 decipher the output back
Now the just ciphered text will be deciphered back to let the user verify that the whole RSA process is working fine.<br>
<b>let decy = powm cyph d (head module) 1<br>
  putStrLn "Decyph ="<br>
  putStrLn (show decy)<br>
  putStrLn (cnvOut decy [])</b><br>
The most relevant function to get the original input back from the ciphered text here is <b>cnvOut decy []</b><br>
-- converte big Integer in [char] to get the original plain text<br>
<b>cnvOut :: Integer -> [Char] -> [Char]<br>
cnvOut 0 a = a<br>
cnvOut n a = cnvOut (n `div` 256) ((chr((fromInteger(n)) `mod` 256)):a)</b><br>
It is actually the reverse process of cnvIn already shown. It is fairly self explanatory so I think it is not neccessary to spend more words.<br>

<br>
<br>
## criFile.hs
This file permits to cipher / decipher data files. It works from command line where 4 args are to be specified:<br>
1) input filename<br>
2) pubkey.rsa or privkey.rsa<br>
3) output filename<br>
4) cipher or deciph option<br>

The first step, either in cipher or decipher mode, is to fetch module bits length, exponent, module from the 
relevant key.rsa file. Those data are text lines terminated by \n each of them representing a integer.

The first line is used to find the block length (bl=nbits/8 -1) for the input file to be read. This length will ensure 
the corresponding integer, built reading a chunk of data 'bl' long , to be lower than the RSA module.
The second line is saved in exp to be used for cipher / decipher, the third one is saved in 'module'.

The second step is to fetch the input file saving it in a list of strings 'bl' long. This process is slightly 
different in crypt versus decrypt. Lets take crypt:<br>
<b> contents <- readFile (args!!0)<br>
 handle <- openFile (args!!0) ReadMode<br>
 cont <- hGetContents handle<br>
 let aBlocks = getBlocks [[]] "" cont (fromInteger(len))</b> -- len is = nbits/8 -1 i.e 127 for mod=1024 bits<br>
 
The function getBlocks is imported from module RsaKit.hs and works as follows:<br>
<b>getBlocks :: [String] -> String -> String-> Int -> [String]<br>
getBlocks ar s contents l <br>
<code>  </code>| (length(contents)) == 0 = tail(ar)++(s:[])<br>
<code>  </code>| (length(s)) == l = (getBlocks (ar++(s:[])) "" contents l)<br>
<code>  </code>| otherwise = (getBlocks ar (s++(head(contents):[])) (tail(contents))  l)</b><br>
  
'cont' contains the entire input file content as a string. This string has to be convertet in multiple strings 
each one long 'len' bytes except the last one that will be likely shorter. The function getBlocks does 
will provide a result as a list of strings corresponding to input 'cont'.

Now this list of strings needs to be transformed in a list of integers corresponding to each string. The 
function to achieve this is:<br>
<b>let plain = [cnvIn x 0 | x <- aBlocks]</b> where cnvIn (imported from RsaKit.hs) is:<br>
-- convert input string to Integer to cipher RSA<br>
<b>cnvIn :: [Char] -> Integer -> Integer<br>
cnvIn i n <br>
<code>  </code>| i == [] = n<br>
<code>  </code>| otherwise = (cnvIn (tail(i)) (intgr(ord(head(i))) n))</b><br>
This function translated into Integer each string supplied (as already shown in genRSA.hs above), since the function is applied inside a list comprehension 
the resutin list will consist in a list of integers corresponding to the original plain text. Now 
all the list can be ciphered to obtain the ciphes text in a new list. The eventual function:<br>
<b>let crypt = [powm x ce mo 1 | x <- plain]</br> will do the trick inside a list comprehension.<br>
Then:<br>
<b>let recs = [(show x) | x <- crypt]</b> will translate the list of Integers in list of strings<br>
and finally:<br>
<b>writeFile (args!!2) (unlines recs)</b> will actually write the data onto the output file. Note the 'unlines' function here 
(being part of imported Data.List module) used to build e single string (written onto the output file) 
from an array of strings (the 'recs' list).<br>





















