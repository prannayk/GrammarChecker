import ImportRun
import Layout
import Control.Applicative
import Control.Monad
import Text.Parsec

parseSome handle = return (parse document "" handle)

getSome (Right grammar) = do return (grammar)

main = do
	handle <- readFile "grammar.txt"
	some <- parseSome handle
	(lolz,lulz,prods) <- getSome some
	gr <- getSome some
	putStrLn $ show $ ll1  (lolz,lulz,(rmLeftRecursion prods))
	return ()