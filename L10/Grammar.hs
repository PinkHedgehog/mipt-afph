module Grammar where

import Parser
import qualified Data.Char as C
import Control.Applicative
import Control.Alternative.Free

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

type GSyntax = [Rule]
data Rule = Rule
          { ruleName   :: RuleName
          , expression :: Expression
          }
          deriving (Show, Eq)

type Expression = [ExpressionPart]

type ExpressionPart = [Term]

data Term = LTerm Literal | RTerm RuleName deriving (Show, Eq)

newtype RuleName = RuleName String deriving (Show, Eq)
newtype Literal = Literal String deriving (Show, Eq)


isSymbol :: Char -> Bool
isSymbol c = c `elem` " |!#$%&()*+,-./:;>=<?@[\\]^_`{}~"

isCharacter c = isSymbol c || C.isAlpha c || C.isDigit c

isRuleName c = C.isAlphaNum c || c == '-' || c == '_'

parserLiteral :: Parser Literal
parserLiteral = (charP '"' *> p1 <* charP '"') <|> (charP '\'' *> p2 <* charP '\'')
    where
        p1 = Literal <$> predStringP (\c -> (isCharacter c || c == '\'') && c /= '"')
        p2 = Literal <$> predStringP (\c -> (isCharacter c || c == '"') && c /= '\'')

parserLineEnd :: Parser ()
parserLineEnd = () <$ (predP C.isControl <* skipString "")

parseOptWhitespace :: Parser ()
parseOptWhitespace = () <$ many (predP C.isSpace)

-- <rule>           ::= <opt-whitespace> "<" <rule-name> ">" <opt-whitespace> "::=" <opt-whitespace> <expression> <line-end>
parserRule :: Parser Rule
parserRule = Rule <$> (
        skip C.isSpace 
        *> charP '<' 
        *> parserRuleName
        <* charP '>'
        )
        <*> (skip C.isSpace 
        *> skipString "::="
        *> skip C.isSpace
        *> parserExpression <* prefixP "")

parserRuleName :: Parser RuleName
parserRuleName = RuleName <$> Parser f
    where
        f rulename = unParser (takeWhile isRuleName rulename <$ (predP C.isAlpha *> skip isRuleName)) rulename

parserTerm :: Parser Term
parserTerm = (LTerm <$> parserLiteral) <|> (RTerm <$> (charP '<' *> parserRuleName <* charP '>')) 

parserList :: Parser [Term]
parserList = listP (predP C.isSpace) parserTerm

t :: [Char]
t = "'aaa'\n\"bbb\""

parserExpression :: Parser [[Term]]
parserExpression = listP (parseOptWhitespace *> charP '|' <* parseOptWhitespace) parserList

parserGSyntax :: Parser [Rule]
parserGSyntax = linesP parserRule






t3 :: [Char]
t3 = "<rule>           ::= <opt-whitespace> \"<\" <rule-name> \">\" <opt-whitespace> \"::=\" <opt-whitespace> <expression> <line-end>\n<opt-whitespace> ::= \" \" <opt-whitespace> | \"\""

t4 :: [Char]
t4 = "<rule>           ::= <opt-whitespace> \"<\" <rule-name> \">\" <opt-whitespace> \"::=\" <opt-whitespace> <expression> <line-end>"
