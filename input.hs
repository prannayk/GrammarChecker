import Layout
import Control.Monad
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Text.Parsec.String

document = do
	spaces
	string "!|!"
	spaces
	x <- many term
	spaces
	string "!|!"
	spaces
	string "|!|"
	spaces
	y <- many nterm
	spaces
	string "|!|" 
	spaces
	z <- many production
	spaces
	return (x,y,z)

production = do
	spaces
	z <- aring
	spaces
	string "->"
	spaces
	l <- many aring
	spaces
	return (Produce z l)

term = do
	x <- oneOf ['A'..'Z']
	return (NonTerm x)

nterm = do
	x <- oneOf ['a'..'z']

aring = do
	spaces
	l <- many (try nterm <|> term)
	spaces