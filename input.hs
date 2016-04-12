module ImportRun where

import Layout
import Control.Monad
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Text.Parsec.String

document :: Parser Grammar
document = do
	spaces
	string "!|!"
	spaces
	x <- many terms
	spaces
	string "!|!"
	spaces
	string "|!|"
	spaces
	y <- many nterms
	spaces
	string "|!|" 
	spaces
	z <- many production
	spaces
	return (x,y,z)

terms = do
	x <- oneOf ['a'..'z']
	return [x]

nterms = do
	x <- oneOf ['A'..'Z']
	return [x]

production = do
	spaces
	z <- aring
	spaces
	string "->"
	spaces
	l <- many aring
	spaces
	return $ Produce z l

term = do
	x <- oneOf ['A'..'Z']
	return (NonTerm [x])

nterm = do
	x <- oneOf ['a'..'z']
	return (Term [x])

aring = do
	l <- many (try nterm <|> term)
	return l