# Checks if a grammar is LL(1)

It requires input in text format which specifies the grammer in form of Productions. The tool parses such text and then builds it's parsing table. If there are no conflicts it returns the table while if they are it concludes that the Grammar is not LL(1). 
### Steps :
1. It first checks if the language is context free.
2. It then removes left recursion
3. It then left factors the grammar
4. It then tries to create parseTable and if there are no conflicts then it concludes that the grammar is LL(1)

### Pseudo-grammar for taking input a grammar defined in "grammar.txt" file.
1. The terminal are defined between `!||` tags
2. The non-terminals are defined between `|!|` tags
3. The rest of the file defines productions. 
4. Each production is given as `head` followed by `->` followed by a string of nonterminals/ terminals.
5. Alternates for the same string of terminals and nonterminals are seperated by the `!|` tag while the `||` tag marks the end of the production

* The running code is in check.hs
* All functions and data types are defined in Layout.hs
* the parser is defined in ImportRun.hs while it is generally tested using the input.hs file.
* The code will be annoted as well CI will be added soon.