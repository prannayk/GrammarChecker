import ImportRun
import Layout
import Control.Applicative
import Control.Monad
import Text.Parsec
import Data.Either
--parseo stri = 
	--case (parse document "" stri) of
		--Right stri -> 
		--Left err -> ([],[],[])
parseSome handle = return (parse document "" handle)

getSome (Right grammar) = do return (grammar)

main = do
	handle <- readFile "grammar.txt"
	some <- parseSome handle
	(lolz,lulz,prods) <- getSome some
	gr <- getSome some
	putStrLn $ show $ rmLeftRecursion prods
	putStrLn $ show $ ll1  (lolz,lulz,(rmLeftRecursion prods))
	--putStrLn $ show $ ll1 gr
	--putStrLn $ show $ first gr [(NonTerm "A")]
	return ()