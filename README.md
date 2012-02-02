Adventure
=========
This project is a Haskell implementation of the classic game "Colossal
Cave Adventure".

The original Fortran code for the game was written by Don Woods in 1977,
an expanded version of a previous one co-authored with William
Crowthers. In 1998 Don Knuth rewrote the game in C (a gem of literate
programming) and that's the code I used as a reference.

Copyright and license
---------------------
None of the original Fortran or C files have a license. I met Donald Knuth
at symposium at the ETH Zuerich and he told me that as far as he was
concerned I could used his code as I pleased. I am releasing this Haskell
version under a permissive BSD3 license (see LICENSE in this same
directory).

Install
-------
To install the game you must have GHC installed and issue the following
commands in a terminal:

  cd Adventure
  runhaskell Setup.hs configure
  runhaskell Setup.hs build
  runhaskell Setup.hs install
