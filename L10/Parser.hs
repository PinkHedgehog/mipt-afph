module Parser where

import Data.List
import Data.Functor
import Control.Applicative
import Control.Monad
--import Control.Alternative
import Data.Bifunctor
import Data.Char



newtype Parser a = Parser { unParser :: String -> [(String, a)] }

parseString :: String -> Parser a -> Maybe a
parseString s (Parser p) = case p s of
    [("", val)] -> Just val
    _           -> Nothing

runparser :: Eq a => String -> Parser a -> Maybe a
runparser s (Parser f) = if null pRes then Nothing else Just $ snd . head $ pRes
    where
        pRes = filter (null . fst) $ f s


predP :: (Char -> Bool) -> Parser Char
predP p = Parser f
    where
        f "" = []
        f (c:cs) | p c = [(cs, c)]
                 | otherwise = []

charP :: Char -> Parser Char
charP char = predP (== char)

stringP :: String -> Parser String
stringP s = Parser f
    where
        f s' | s == s' = [("", s)]
             | otherwise = []

skip :: (Char -> Bool) -> Parser ()
skip p = Parser $ \s -> [(dropWhile p s, ())]

prefixP :: String -> Parser String
prefixP s = Parser f
    where
        f input | s `isPrefixOf` input = [(drop (length s) input, s)]
                | otherwise            = []

skipString :: String -> Parser ()
skipString s = Parser f
    where
        f input | s `isPrefixOf` input = [(drop (length s) input, ())]
                | otherwise            = []

predStringP :: (Char -> Bool) -> Parser String
predStringP p = Parser f
    where
        f input = [(dropWhile p input, takeWhile p input)]

acceptP :: Parser String
acceptP = Parser $ \s -> [("", s)]

instance Functor Parser where
    fmap f (Parser p1) = Parser p2
        where
            p2 str = map (second f) $ p1 str


instance Applicative Parser where
    --pure :: a -> Parser a
    --pure x = Parser $ \s -> [(s, x)]
    pure x = Parser (\s -> [(s, x)])
    pf <*> px = Parser (\s -> [ (sx, f x) | (sf, f) <- unParser pf s,
                                          (sx, x) <- unParser px sf])
    --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
    -- Parser f <*> Parser p1 = Parser p2
    --     where
    --         -- f  :: String -> [(String, (a -> b))]
    --         -- p1 :: String -> [(String, a)]
    --         -- p2 :: String -> [(String, b)]
    --         p2 str = [ (xs2, g x) | (xs1, g) <- f  str
    --                               , (xs2, x) <- p1 xs1
    --                 ]

instance Alternative Parser where
    empty = Parser (const [])
    px <|> py = Parser (\s -> unParser px s ++ unParser py s)


lineEndP :: Parser ()
lineEndP = () <$ (skip isControl <* skipString "")

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) = liftA2 (:)

listP :: Parser b -> Parser a -> Parser [a]
listP pdel parser = 
        (stringP "" $> []) 
    <|> (parser <:> pure []) 
    <|> ((parser <* pdel) <:> listP pdel parser)
    -- many pdel *> liftA2 (:) parser

listPChar :: Char -> Parser a -> Parser [a]
listPChar c parser = ((predP (\x -> isControl x || x == c) *> stringP "") $> [])
        <|> ((:) <$> (parser <* skip (== c)) <*> pure [])
        <|> ((:) <$> (parser <* skip (== c)) <*> listPChar c parser)

linesP :: Parser a -> Parser [a]
linesP = listP lineEndP

listAltP :: [Parser a] -> Parser a
listAltP = foldl' (<|>) empty

seqP :: [Parser a] -> Parser [a]
seqP = foldl' (<*) empty
