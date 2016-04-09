import qualified Data.Either as E
import Control.Monad

type Terminal = String
type NonTerminal = String

-- smallest element of any string in the production
-- redefining a different kind of string which refers to the string of terminals or non terminals and is closer to the grammar than the string of characters we generally use. 
data Atom = Term Terminal | NonTerm NonTerminal | Empty deriving (Show, Eq)
type Aring = [Atom]
data Production = Produce Aring [Aring] 
type Grammar = ([Terminal],[NonTerminal],[Production])

first :: Aring -> [Aring]
first ((Term x):_) = [[Term x]]
first ((NonTerm x):_) = (match (NonTerm x)) >>= first

match ::  Atom -> [Aring]
match (NonTerm "A") = [[(Term "a")],[( Term "b")]]

searchTinAtom :: Aring -> Terminal -> [Bool]
searchTinAtom ring x = [(Term x) `elem` ring]

searchTinAring1 ::  [Terminal] -> Production -> [Bool]
searchTinAring1 terms (Produce head _) = [and $ terms >>= searchTinAtom head]

contextFree :: Grammar -> Bool
contextFree (terms,nterms,prods) = and $ prods >>= searchTinAring1 terms

leftRecur :: Production -> Bool
leftRecur (Produce head tails) = and $ tails >>= first >>= checkFirst head

checkFirst :: Aring -> Aring -> [Bool]
checkFirst head target = [(head == (take (length head) target))]
--search :: (Eq a) => [a] -> [a] -> Bool
--search list1 listmain = (list1 == (take (length list1) listmain))

--leftRecurGr ::  Grammar -> 
