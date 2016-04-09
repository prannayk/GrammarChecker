import qualified Data.Either as E
import Control.Monad

type Terminal = String
type NonTerminal = String
data Atom = Term Terminal | NonTerm NonTerminal | Empty deriving (Show, Eq)
type Aring = [Atom]
data Production = Produce Aring [Aring] 
type Grammar = ([Terminal],[NonTerminal],[Production])

first :: Aring -> [Aring]
first ((Term x):_) = [[Term x]]
first ((NonTerm x):_) = (match [(NonTerm x)]) >>= first
match ::  Aring -> [Aring]
match (NonTerm "A") = [[(Term "a")],[( Term "b")]]
searchTinAtom :: Aring -> Terminal -> [Bool]
searchTinAtom ring x = [(Term x) `elem` ring]
searchTinAring1 ::  [Terminal] -> Production -> [Bool]
searchTinAring1 terms (Produce head _) = [and $ terms >>= searchTinAtom head]
contextFree :: Grammar -> Bool
contextFree (terms,nterms,prods) = and $ prods >>= searchTinAring1 terms
leftRecur :: Production -> [Bool]
leftRecur (Produce head tails) = [and $ tails >>= first >>= checkFirst head]
checkFirst :: Aring -> Aring -> [Bool]
checkFirst head target = [(head == (take (length head) target))]
leftRecurGr :: Grammar -> Bool
leftRecurGr (terms,nterms,prods) = and $ prods >>= leftRecur

