module GrammarInit where

import Parser
import Data.Char
import Control.Applicative
import Data.List
{-
 - <syntax>         ::= <rule> | <rule> <syntax>
 - <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
 - <opt-whitespace> ::= " " <opt-whitespace> | ""
 - <expression>     ::= <list> | <list> <opt-whitespace> "|" <opt-whitespace> <expression>
 - <line-end>       ::= <opt-whitespace> <EOL> | <line-end> <line-end>
 - <list>           ::= <term> | <term> <opt-whitespace> <list>
 - <term>           ::= <literal> | "<" <rule-name> ">"
 - <literal>        ::= '"' <text1> '"' | "'" <text2> "'"
 - <text1>          ::= "" | <character1> <text1>
 - <text2>          ::= '' | <character2> <text2>
 - <character>      ::= <letter> | <digit> | <symbol>
 - <letter>         ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
 - <digit>          ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 - <symbol>         ::=  "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
 - <character1>     ::= <character> | "'"
 - <character2>     ::= <character> | '"'
 - <rule-name>      ::= <letter> | <rule-name> <rule-char>
 - <rule-char>      ::= <letter> | <digit> | "-"
 -}
-- "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"

data GSyntax       = SRule Rule | ERule Rule GSyntax deriving (Show, Eq) -- single rule or extended rule, which means `rule  syntax`
--data Rule          = Rule OptWhitespace LeftBeak RuleName RightBeak OptWhitespace Def OptWhitespace Expression LineEnd deriving (Show, Eq)
data Rule          = Rule RuleName Expression  deriving (Show, Eq)
data LeftBeak      = LeftBeak deriving (Show, Eq)
data RightBeak     = RightBeak deriving (Show, Eq)
data Def           = Def deriving (Show, Eq)
data LineEnd       = SLineEnd OptWhitespace EOL | ELineEnd LineEnd LineEnd | TLineEnd deriving (Show, Eq)
data OptWhitespace = OptWhitespace deriving (Show, Eq)
-- data Expression    = SExpr List | EExpr List OptWhitespace OrLine OptWhitespace Expression deriving (Show, Eq)
data Expression    = SExpr List | EExpr List Expression deriving (Show, Eq)
--data List          = SList Term | EList Term OptWhitespace List deriving (Show, Eq)
data List          = SList Term | EList Term List deriving (Show, Eq)
data Term          = STerm Literal | ETerm RuleName  deriving (Show, Eq)
-- data Term          = STerm Literal | ETerm LeftBeak RuleName RightBeak deriving (Show, Eq)
--data Literal       = LLiteral LQuotes Text1 LQuotes | SLiteral SQuotes Text2 SQuotes deriving (Show, Eq)
data Literal       = LLiteral Text1 | SLiteral Text2 deriving (Show, Eq)
data LQuotes       = LQuotes deriving (Show, Eq)
data SQuotes       = SQuotes deriving (Show, Eq)

data Text1         = SText1 | EText1 Char1 Text1 deriving (Show, Eq)
data Text2         = SText2 | EText2 Char2 Text2 deriving (Show, Eq)

data Char1         = SChar1 SQuotes | EChar1 Char deriving (Show, Eq)
data Char2         = SChar2 LQuotes | EChar2 Char deriving (Show, Eq)

--data Character     = LCharacter

data Empty         = Empty deriving (Show, Eq)
type RuleName      = String
data WhiteSpace    = WhiteSpace deriving (Show, Eq)

data EOL           = EOL deriving (Show, Eq)
data OrLine        = OrLine deriving (Show, Eq)

isMySymbol :: Char -> Bool
isMySymbol c = c `elem` " |!#$%&()*+,-./:;>=<?@[\\]^_`{}~"
--    [ "|" , " " , "!" , "#" , "$" , "%" , "&" , "(" , ")" , "*" , "+" , "," , "-" , "." , "/" , ":" , ";" , ">" , "=" , "<" , "?" , "@" , "[" , "\\" , "]" , "^" , "_" , "`" , "{" , "}" , "~" ]

parserEmpty :: Parser Empty
parserEmpty = Parser $ \s -> case s of
    "" -> [("", Empty)]
    _  -> [(s, Empty)]

parserEOL :: Parser EOL
parserEOL = EOL <$ (fmap (:[]) (charP '\n') <|> stringP "\n")

parserOrLine :: Parser OrLine
parserOrLine = OrLine <$ charP '|'

parserWhiteSpace :: Parser WhiteSpace
parserWhiteSpace = WhiteSpace <$ predP isSpace

parserDef :: Parser Def
parserDef = Def <$ prefixP "::="

parserRuleName :: Parser RuleName
parserRuleName = Parser f
   where
        rulePred = \x -> isAlphaNum x || x == '-'
        f rulename = unParser (takeWhile rulePred rulename <$ (predP isAlpha *> predStringP rulePred)) rulename
    -- f rulename = unParser (rulename <$ (predP isAlpha *> many (predP isAlphaNum))) rulename

parserLQuotes :: Parser LQuotes
parserLQuotes = LQuotes <$ charP '"'

parserSQuotes :: Parser SQuotes
parserSQuotes = SQuotes <$ charP '\'' <|> SQuotes <$ stringP "'"

parserChar1 :: Parser Char1
parserChar1 = (SChar1 <$> parserSQuotes) <|> EChar1 <$> predP (\x -> (isAlpha x || isDigit x || isMySymbol x || (x == '-')) && (x /= '\'') && isAscii x) -- || (x == '"')

parserChar2 :: Parser Char2
parserChar2 = (SChar2 <$> parserLQuotes) <|> EChar2 <$> predP (\x -> (isAlpha x || isDigit x || isMySymbol x || (x == '-')) && (x /= '"') && isAscii x) -- || (x == '\'')

parserText1 :: Parser Text1
parserText1 = (SText1 <$ parserEmpty) <|> 
            (EText1 <$> parserChar1 <*> parserText1) <|>
            (SText1 <$ parserSQuotes <* parserSQuotes) 

parserText2 :: Parser Text2
parserText2 = (SText2 <$ parserEmpty) <|> 
            (EText2 <$> parserChar2 <*> parserText2) <|>
            (SText2 <$ parserLQuotes <* parserLQuotes)
--Parser $ \s -> nub $ filter (\(a, b) -> null a) $ unParser p s
--data Literal       = LLiteral LQuotes Text1 LQuotes | SLiteral SQuotes Text2 SQuotes deriving (Show, Eq)
parserLiteral :: Parser Literal
parserLiteral = 
    LLiteral <$> (parserLQuotes *> parserText1 <* parserLQuotes) <|>
    SLiteral <$> (parserSQuotes *> parserText2 <* parserSQuotes)
    -- (LLiteral <$> parserLQuotes <*> parserText1 <*> parserLQuotes) <|>
    -- (SLiteral <$> parserSQuotes <*> parserText2 <*> parserSQuotes)
-- type GSyntax = [Rule]
-- data Rule = Rule RuleName Expression
-- type RuleName = String
-- data Expression = SExpr List | EExpr List Expression

parserLeftBeak :: Parser LeftBeak
parserLeftBeak = LeftBeak <$ charP '<'

parserRightBeak :: Parser RightBeak
parserRightBeak = RightBeak <$ charP '>'

--data Term          = STerm Literal | ETerm LeftBeak RuleName RightBeak deriving (Show, Eq)
parserTerm :: Parser Term
parserTerm = (STerm <$> parserLiteral) <|> (ETerm <$> (parserLeftBeak *> parserRuleName <* parserRightBeak))

parserOptWhitespace :: Parser OptWhitespace
parserOptWhitespace = OptWhitespace <$ many parserWhiteSpace

--data List          = SList Term | EList Term OptWhitespace List deriving (Show, Eq)
parserList :: Parser List
parserList = (SList <$> parserTerm) <|> (EList <$> parserTerm <* parserOptWhitespace <*> parserList)

--data Expression    = SExpr List | EExpr List OptWhitespace OrLine OptWhitespace Expression deriving (Show, Eq)
parserExpression :: Parser Expression
parserExpression = (SExpr <$> parserList) <|>
    (EExpr <$> parserList <* parserOptWhitespace <* parserOrLine <* parserOptWhitespace <*> parserExpression)

parserLineEnd :: Parser LineEnd
parserLineEnd = TLineEnd <$ (parserOptWhitespace <* some parserEOL) -- <|> (ELineEnd <$> parserLineEnd <*> parserLineEnd)
-- (TLineEnd <$ empty) <|> (SLineEnd <$> parserOptWhitespace <*> parserEOL) <|> (ELineEnd <$> parserLineEnd <*> parserLineEnd)
--TLineEnd <$ (parserOptWhitespace *> some parserEOL)
    --(SLineEnd <$> parserOptWhitespace <* some parserEOL) -- <|> (ELineEnd <$> parserLineEnd <*> parserLineEnd)
--data Rule          = Rule OptWhitespace LeftBeak RuleName RightBeak OptWhitespace Def OptWhitespace Expression LineEnd deriving (Show, Eq)
parserRule :: Parser Rule
parserRule = Rule <$> 
    ( parserOptWhitespace 
    *> parserLeftBeak
    *> parserRuleName
    <* parserRightBeak
    <* parserOptWhitespace
    <* parserDef
    <* parserOptWhitespace
    ) 
    <*> parserExpression 
    <* some parserLineEnd

{-
    Rule <$>
    parserOptWhitespace *>
    parserLeftBeak *>
    parserRuleName <*
    parserRightBeak <*
    parserOptWhitespace <*
    parserDef <*>
    parserExpression
-}

parserGSyntax :: Parser GSyntax
parserGSyntax = (SRule <$> parserRule <* parserLineEnd) <|> (ERule <$> (parserRule <* parserLineEnd) <*> parserGSyntax)



s :: [Char]
s = "<syntax>         ::= <rule> | <rule> <syntax>\n<rule>           ::= <opt-whitespace> \"<\" <rule-name> \">\" <opt-whitespace> \"::=\" <opt-whitespace> <expression> <line-end>"
v :: [Char]
v = "<syntax>         ::= <rule> | <rule> <syntax>"

s1 :: [Char]
s1 = "<syntax>         ::= <rule> | <rule> <syntax>\n<rule>           ::= <opt-whitespace> \"<\" <rule-name> \">\" <opt-whitespace> \"::=\" <opt-whitespace> <expression> <line-end>\n"
s2 :: [Char]
s2 = "<syntax>         ::= <rule> | <rule> <syntax>\n<rule>           ::= <opt-whitespace> \"<\" <rule-name> \">\" <opt-whitespace> \"::=\" <opt-whitespace> <expression> <line-end>\n<opt-whitespace> ::= \" \" <opt-whitespace> | \"\"\n"

