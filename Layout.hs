module Layout where

import qualified Data.Either as E
import Control.Monad
import Control.Applicative

type Terminal = String
type NonTerminal = String
data Atom = Term Terminal | NonTerm NonTerminal | Empty | Start | End deriving (Show, Eq)
type Aring = [Atom]
data Production = Produce Aring [Aring] 
type Grammar = ([Terminal],[NonTerminal],[Production])
type Element = (NonTerminal,Terminal,[Production])

first :: Grammar  -> Aring -> [Aring]
first _ ((Term x):_)= [[Term x]]
first grmmr ((NonTerm x):_) = (match [(NonTerm x)] grmmr) >>= calcfirst grmmr

calcfirst :: Grammar -> Aring -> [Aring]
calcfirst grmmr (x:rest) = if ([Empty] `elem` (first grmmr [x])) then (first grmmr [x]) ++ (first grmmr rest) else (first grmmr [x])

searchTinAtom :: Aring -> Terminal -> [Bool]
searchTinAtom ring x = [(Term x) `elem` ring]
searchTinAring1 ::  [Terminal] -> Production -> [Bool]
searchTinAring1 terms (Produce head _) = [and $ terms >>= searchTinAtom head]
contextFree :: Grammar -> Bool
contextFree (terms,nterms,prods) = and $ prods >>= searchTinAring1 terms

leftRecur :: Grammar -> Production -> [Bool]
leftRecur grmmr (Produce head tails) = [and $ tails >>= first grmmr >>= checkFirst head]
checkFirst :: Aring -> Aring -> [Bool]
checkFirst head target = [(head == (take (length head) target))]
leftRecurGr :: Grammar -> Bool
leftRecurGr (terms,nterms,prods) = and $ prods >>= leftRecur (terms,nterms,prods)

rmLeftRecursion :: [Production] -> [Production]
rmLeftRecursion [] = []
rmLeftRecursion (prod:prods) = rmLeft prod (rmLeftRecursion prods)
rmLeft :: Production -> [Production] -> [Production]
rmLeft (Produce ring rings) dones = ((filter (isNterm dones) rings) >>= getAlternate dones ring) ++ ((filter (hasTerm) rings) >>= (\x -> [(Produce ring [x])]))
getAlternate :: [Production] -> Aring -> Aring -> [Production]
getAlternate dones (ringo:rango) head = [Produce head (map (++rango) (match [ringo] ([],[],dones)))]
isNterm :: [Production] -> Aring -> Bool
isNterm prods (atom:other) = if (nonTerm atom) then and $ prods >>= (\x -> if (prodHas x atom) then [True] else [False]) else False
hasTerm :: Aring -> Bool
hasTerm ((Term _):_) = True
hasTerm _ = False
nonTerm :: Atom -> Bool
nonTerm (NonTerm _) = True
nonTerm _ = False
prodHas :: Production -> Atom -> Bool
prodHas (Produce (atom:ring) tails) atom2 = atom == atom2
checkLeft :: Aring -> Production -> [Aring]
checkLeft ring (Produce ringo rings) 
	| ring == ringo = rings
match ::  Aring -> Grammar -> [Aring]
match [(NonTerm "A")] _ = [[(Term "a")],[( Term "b")]]
match ring (_,_,prods) = prods >>= checkLeft ring
-- check for if grammar is context free
countF:: Grammar -> Terminal -> NonTerminal -> Integer
countF grmmer term nonterm = foldl (\acc x -> if(x==True) then acc+1 else acc) 0 $ map (== [Term term]) (concat $ map (\x -> if (x == [Empty]) then (follow grmmer [NonTerm nonterm]) else [x]) (first grmmer [NonTerm nonterm]))
--check for ll1
ll1 :: Grammar -> Bool
ll1 (terms,nterms,prods) = and $ map (<1) $ countF (terms,nterms,prods) <$> terms <*> nterms

-- left factor the grammar
factor :: (Eq a) => [[a]] -> [a]
factor (x:[]) = []
factor ((x:xs):xss) = longer (x:factor (filter (\(y:ys) -> x == y) xss)) (factor xss)

longer :: [a] -> [a] -> [a]
longer list1 list2
	| (length list1) > (length list2) = list1
	| otherwise = list2

findRest :: Aring -> Aring -> Aring
findRest check (cas:cases) = if (check == (take (length check) (cas:cases))) then drop (length check) (cas:cases) else findRest check cases
findRest check [] = []
followSet :: Aring -> Production -> [Aring]
followSet check (Produce ring rings) = rings >>= (\x -> [(findRest check x)])

follow :: Grammar -> Aring -> [Aring]
follow _ [(Start)] = [[End]]
follow (terms,nterms,prods) head = filter (/= [Empty]) $ prods >>= (followSet head) >>= (\x -> if (x /= []) then first (terms,nterms,prods) x else [])