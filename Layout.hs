data Terminal = Empty | + | - | * | Id
data NonTerminal = [Terminal]
data Production = NonTerminal Terminal NonTerminal

