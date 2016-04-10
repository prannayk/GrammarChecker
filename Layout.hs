import qualified Data.Either as E
import Control.Monad
import Control.Applicative

type Terminal = String
type NonTerminal = String
data Atom = Term Terminal | NonTerm NonTerminal | Empty deriving (Show, Eq)
type Aring = [Atom]
data Production = Produce Aring [Aring] 
type Grammar = ([Terminal],[NonTerminal],[Production])
type Element = (NonTerminal,Terminal,[Production])
--type ParseTable = [Element]

first :: Grammar  -> Aring -> [Aring]
first _ ((Term x):_)= [[Term x]]
first grmmr ((NonTerm x):_) = (match [(NonTerm x)] grmmr) >>= calcfirst grmmr
-- above recursion is infinite in presence of cycles, empty or left recursion
calcfirst :: Grammar -> Aring -> [Aring]
calcfirst grmmr (x:rest) = if ([Empty] `elem` (first grmmr [x])) then (first grmmr [x]) ++ (first grmmr rest) else (first grmmr [x])
-- above recursion handles Empty conditions in calculating first set.
searchTinAtom :: Aring -> Terminal -> [Bool]
searchTinAtom ring x = [(Term x) `elem` ring]
searchTinAring1 ::  [Terminal] -> Production -> [Bool]
searchTinAring1 terms (Produce head _) = [and $ terms >>= searchTinAtom head]
contextFree :: Grammar -> Bool
contextFree (terms,nterms,prods) = and $ prods >>= searchTinAring1 terms
--check for Left Recursion of Production
leftRecur :: Grammar -> Production -> [Bool]
leftRecur grmmr (Produce head tails) = [and $ tails >>= first grmmr >>= checkFirst head]
checkFirst :: Aring -> Aring -> [Bool]
checkFirst head target = [(head == (take (length head) target))]
leftRecurGr :: Grammar -> Bool
leftRecurGr (terms,nterms,prods) = and $ prods >>= leftRecur (terms,nterms,prods)
-- the above checks for left recursive grammar as a whole and if the grammar is recursive, we will have to remove that recursion
checkLeft :: Aring -> Production -> [Aring]
checkLeft ring (Produce ringo rings) 
	| ring == ringo = rings
match ::  Aring -> Grammar -> [Aring]
match [(NonTerm "A")] _ = [[(Term "a")],[( Term "b")]]
match ring (_,_,prods) = prods >>= checkLeft ring
-- check for if grammar is context free
countF:: Grammar -> Terminal -> NonTerminal -> Integer
countF grmmer term nonterm = foldl (\acc x -> if(x==True) then acc+1 else acc) 0 $ map (== [Term term]) (first grmmer [NonTerm nonterm])
--check for ll1
ll1 :: Grammar -> Bool
ll1 (terms,nterms,prods) = and $ map (<1) $ countF (terms,nterms,prods) <$> terms <*> nterms