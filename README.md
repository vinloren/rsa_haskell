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

### 1 findig (big) primes

This is achieved by
 