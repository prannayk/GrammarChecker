# Checks if a grammar is LL(1)

It requires input in text format which specifies the grammer in form of Productions. The tool parses such text and then builds it's parsing table. If there are no conflicts it returns the table while if they are it concludes that the Grammar is not LL(1). 
### Steps :
1. It first checks if the language is context free.
2. It then removes left recursion
3. It then left factors the grammar
4. It then tries to create parseTable and if there are no conflicts then it concludes that the grammar is LL(1)
