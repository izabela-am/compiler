module Cradle.Parser
where

import Data.Char

type Parser a = String -> Maybe (a, String)

char :: Parser Char
char [] = Nothing
char (x:xs) = Just(x, xs)

digit :: Parser Char
digit = char <=> isDigit

digits :: Parser String
digits = iter digit 

number :: Parser Integer
number = literal '-' <-+> digits >>> (\n -> -1 * (read n :: Integer) )
     <|> digits >>> (\n -> read n :: Integer)

space :: Parser Char
space = char <=> isSpace  

notSpace :: Parser Char
notSpace = char <=> (not . isSpace)   

letter :: Parser Char
letter = char <=> isAlpha

letters :: Parser String
letters = iter letter

alphanum :: Parser Char
alphanum = digit <|> letter

literal :: Char -> Parser Char
literal c = char <=> (==c)

result :: a -> Parser a
result a cs = Just(a,cs)

iter :: Parser Char -> Parser String
iter m = (iterS m) <=> (/="")

iterS :: Parser a -> Parser [a]
iterS m = m <+> iterS m >>> (\(x, y) -> x:y)
	 <|> result []

token :: Parser a -> Parser a
token = (<+-> iterS space)

acceptWord :: String -> Parser String
acceptWord w = token (letters <=> (==w))

accept :: String -> Parser String
accept w = token ((iter notSpace) <=> (==w))

infix 7 <=> 
(<=>) :: Parser a -> (a -> Bool) -> Parser a 
(parser <=> predicate) input =
    case parser input of 
        Nothing       -> Nothing 
        Just(a,rest)  -> if (predicate a) then Just(a,rest) else Nothing


infixl 6 <+>
(<+>) :: Parser a -> Parser b -> Parser (a, b)
(parserA <+> parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
        	Just (resultB, cs) -> Just((resultA, resultB), cs)

infixl 6 <+-> 
(<+-> ) :: Parser a -> Parser b -> Parser a
(parserA <+->  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (_, cs) -> Just(resultA, cs)

infixl 6 <-+> 
(<-+> ) :: Parser a -> Parser b -> Parser b
(parserA <-+>  parserB) input = 
	case parserA input of
	    Nothing -> Nothing
	    Just (resultA, remainder) -> case parserB remainder of
	        Nothing -> Nothing
	        Just (resultB, cs) -> Just(resultB, cs) 

infixl 5 >>>
(>>>) :: Parser a -> (a -> b) -> Parser b
(parser >>> transformer) input = 
	case parser input of
		Nothing -> Nothing
		Just (resultA, remainder) -> Just ((transformer resultA), remainder)

infix 4 +>
(+>) :: Parser a -> (a -> Parser b) -> Parser b
(parser +> function) input = 
	case parser input of
		Nothing -> Nothing
		Just (a, cs) -> function a cs

infixl 3 <|>
(<|>) :: Parser a -> Parser a -> Parser a 
(parserA <|> parserB) input = 
	case parserA input of 
    	Nothing -> parserB input 
    	result -> result