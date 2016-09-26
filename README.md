Project in progress

Simplified scheme interpreter created in order to learn haskell.  
Project based on the book 'Write Yourself a Scheme in 48 Hours' by Jonathan Tang.  
Link: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours  

Basic functionalities were based on the mentioned book, while the following I implemented myself:
- parsing :
    - backquotes with commas
- primitives : 
    - string?
    - number?
    - symbol?
    - boolean?
    - symbol->string
    - string->symbol
    - partially equal? (comparing lists)
- evaluating :
    - quasiquote with unquote
    - cond
    - case

To compile use the following command in src directory. You need ghc installed.  
ghc -o ../output/parser -outputdir ../output main.hs  

Run program (from output directory):
As interactive interptreter (no command line arguments): 
...\output>cmd /c parser.exe
or just double click the executive file

Reading single expression (single command line argument):
...\output>cmd /c parser.exe "`(1 2 ,(if (< 1 3) (+ 2 4) \"1 is greater than three\"))"  
(1 2 6) 

License: GNU GPL