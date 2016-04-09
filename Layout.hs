import qualified Data.Either as E
import Control.Monad

type Terminal = String
type NonTerminal = String
data Atom = Term Terminal | NonTerm NonTerminal | Empty deriving (Show, Eq)
type Aring = [Atom]
data Production = Produce Aring [Aring] 
type Grammar = ([Terminal],[NonTerminal],[Production])

first :: Grammar  -> Aring -> [Aring]
first _ ((Term x):_)= [[Term x]]
first grmmr ((NonTerm x):_) = (match [(NonTerm x)] grmmr) >>= first grmmr
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

checkLeft :: Aring -> Production -> [Aring]
checkLeft ring (Produce ringo rings) 
	| ring == ringo = rings

match ::  Aring -> Grammar -> [Aring]
match [(NonTerm "A")] _ = [[(Term "a")],[( Term "b")]]
match ring (_,_,prods) = prods >>= checkLeft ring