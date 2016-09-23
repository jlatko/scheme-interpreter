Simplified scheme interpreter created in order to learn haskell.
Project based on the book 'Write Yourself a Scheme in 48 Hours' by Jonathan Tang.
Link: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours


To compile use the following command in src directory. You need ghc installed.
ghc -o ../output/parser -outputdir ../output main.hs

At the moment the program is able to evaluate single line of code given as a command line parameter.
Example (from output directory):

...\output>cmd /c parser.exe "`(1 2 ,(if (< 1 3) (+ 2 4) \"1 is greater than three\"))"
(1 2 6)