A purely functional Adventure
=============================
This project is a Haskell implementation of the classic game "Adventure"
also known as "Colossal Cave Adventure".

The original Fortran code for the game was written by Don Woods in 1977,
an expanded version of a previous one co-authored with William
Crowthers. In 1998 Don Knuth rewrote the game in C and that's the code I
used as a reference.

Install
-------
To install the game GHC must be present on your system. If so, open a
terminal and issue the following commands:

  cd Adventure
  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  runhaskell Setup.hs install


Play
----
Type:

  ./Adventure

License
-------
None of the original Fortran or C files have a license, so I wrote a mail
to Don Woods asking for a clarification. I paste his reply as a reference
for future tinkerers:



I also met Donald Knuth at a symposium at the ETH in Zuerich and he told me
that as far as he is concerned I can use his code as I please.

Therefore I am releasing this version under a very permissive BSD3 license
(see LICENSE in this same directory).
