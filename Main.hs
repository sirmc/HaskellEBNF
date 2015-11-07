{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.Char8
import qualified Data.Attoparsec.Char8 as A
import Data.Word
import Data.Char (isAlphaNum, isAlpha)
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Map as Map 

--type Grammar = [Rule]
type Grammar = Map.Map Identifier Production
data Rule = Rule Identifier Production deriving Show
type Identifier = String
data Production = Follow Identifier 
                | Terminal String 
                | Expr [Production]
                | Term [Production]
                | Optional Production
                | Many Production
                | Grouped Production
                deriving Show

skipSpaceNoNewline = skipWhile (\x -> isSpace x && not (x == '\n'))

parseGrammar :: Parser Grammar
parseGrammar = do
    --skipSpace
    g <- many1 parseRule
    skipSpace
    return $ grammarToMap  g

grammarToMap g = Map.fromList $ map ruleToTuple g
ruleToTuple (Rule i p) = (i,p)

parseRule :: Parser Rule
parseRule = do
    skipSpace
    lhs <- identifier
    skipSpace
    char '='
    skipSpace
    rhs <- expression
    skipSpace
    char ';'
    skipSpaceNoNewline
    endOfLine
    return $ Rule lhs rhs

production :: Parser Production
production = 
    try (Terminal <$> terminal)
    <|> try (Follow <$> identifier)
    <|> try (manyProduction)
    <|> try (optionalProduction)
    <|> try (groupedProduction)
    <|> try (expression)

expression,term :: Parser Production
expression = do
    t <- sepBy1 term (skipSpace >> char '|' >> skipSpaceNoNewline)
    return $ Expr t

term = do
    skipSpace
    t <- sepBy1 production (skipSpace >> char ',' >> skipSpaceNoNewline)
    skipSpace
    return $ Term t

manyProduction :: Parser Production
manyProduction = do
    char '{'
    p <- expression
    char '}' 
    return $ Many p

optionalProduction :: Parser Production
optionalProduction = do
    char '['
    p <- expression
    char ']' 
    return $ Optional p

groupedProduction :: Parser Production
groupedProduction = do
    char '('
    p <- expression
    char ')' 
    return $ Grouped p

terminal :: Parser String
terminal = 
    try (do
        char '\''
        val <- manyTill' anyChar (char '\'')
        return $ val
    )
    <|> try (do
        char '\"'
        val <- manyTill' anyChar (char '\"')
        return $ val
    )

identifier :: Parser Identifier
identifier = do
    c <- letter_ascii
    lhs <- many' $ satisfy (\c -> isAlphaNum c || c == '_') -- TODO: _
    return $ c:lhs

ebnfFile :: FilePath
ebnfFile = "input.txt"


-- PRINT FUNCTIONS

showTree :: Production -> String
showTree (Terminal s) = "'" ++ s ++ "'"
showTree (Follow s) = s
showTree (Many s) = "{" ++ showTree s ++ "}"
showTree (Optional s) = "[" ++ showTree s ++ "]"
showTree (Grouped s) = "(" ++ showTree s ++ ")"
showTree (Expr (x:[])) = showTree x
showTree (Term (x:[])) = showTree x
showTree (Expr s) = concat $ map (\s -> showTree s ++ "|") s
showTree (Term s) = concat $ map (\s -> showTree s ++ ",") s

--main = print $ parseOnly parseGrammar "abc = { 'def' | 'a' };\n"
--main = B.readFile ebnfFile >>= print . parseOnly parseGrammar
--findRule :: String -> Grammar -> Maybe Production
--findRule s g = Map.lookup s g
printGrammar t = mapM_ (\v -> putStrLn $ v) 
            (Map.mapWithKey (\k v-> ((show k) ++ ": " ++  (showTree v))) t)

main :: IO ()
--main = B.readFile ebnfFile >>= print . parseOnly (parseGrammar <* endOfInput)
main = do
    file1 <- B.readFile ebnfFile
    let p = parseOnly (parseGrammar <* endOfInput) file1
    case p of
         Left err ->putStrLn $ "A parsing error was found: " ++ err
         --Right t -> print $  findRule "identifier" t
         --Right t -> mapM_ (\v -> putStrLn $ v) (Map.mapWithKey (\k v-> ((show k) ++ " " ++  (show v))) t)
         Right t -> printGrammar t


