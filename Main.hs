{-# LANGUAGE OverloadedStrings #-}
import Data.Attoparsec.ByteString.Char8
import Data.Word
import Data.Char (isAlphaNum, isAlpha, ord)
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Map as Map 
import Control.Monad.Random
import Control.Monad
import System.Directory
import System.Exit

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

parseOverride :: Parser (Map.Map String [String])
parseOverride = do
    --skipSpace
    g <- many1 parseOverrideRule
    skipSpace
    return $ Map.fromList $  g

parseOverrideRule :: Parser (String, [String])
parseOverrideRule = do
    skipSpace
    lhs <- identifier
    skipSpace
    char '='
    skipSpace
    rhs <- stringList
    skipSpace
    return $ (lhs, rhs)

stringList :: Parser [String]
stringList = do
    skipSpace
    t <- sepBy1 listTerm (skipSpace >> char ',' >> skipSpaceNoNewline)
    skipSpace
    return $ t

listTerm :: Parser String
listTerm = do
    char '\"'
    --c <- letter_ascii
    --lhs <- many' $ satisfy (\c -> isAlphaNum c || c == '_') -- TODO: _
    val <- manyTill' anyChar (char '\"')
    return $ val
    

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
    skipSpace
    t <- sepBy1 term (skipSpace >> char '|' >> skipSpaceNoNewline)
    skipSpace
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
ebnfFile = "ap2.txt"

overrideFile :: FilePath
overrideFile = "override.txt"

depthThreshold = 70

{- Allows you to override value of Follow, e.g. to force use of certain identifier -}
overrides = Map.fromList $ [("identifier", ["var2", "var1", "var3"])]

findOverride s m = Map.lookup s m

data GrammarInstance = GrammarInstance { grammar :: Grammar,
                                         overrideMap :: Map.Map String [String]
                                        }

generateTree :: GrammarInstance -> Int -> Production -> IO String
generateTree g c (Many t) = do
                    r <- getStdRandom (randomR (0, 4))
                    if c > depthThreshold then pure "" else fmap (concat) $ sequence $ replicate r (generateTree g (c + 1) t) -- pick randon number
generateTree g c (Terminal t) = pure t 
-- TODO: Force it to take a terminating path
generateTree g c (Expr t) = do 
                    r <- getStdRandom (randomR (1, length t))
                    generateTree g (c + 1) $ t !! (r - 1)
generateTree g c (Term t) = fmap (concat) $ sequence $ [generateTree g (c + 1) a | a <- t]
generateTree g c (Follow s) = do 
            let r = findRule s (grammar g)
            let o = findOverride s $ overrideMap g
            case r of
                 Just t -> 
                    case o of
                        Just ov -> do 
                            n <- getStdRandom (randomR (1, length ov))
                            pure $ ov !! (n - 1)
                        Nothing -> generateTree g (c + 1) t
                 Nothing -> pure ("Rule lookup error: " ++ s)
generateTree g c (Grouped t) = generateTree g c t 
generateTree g c (Optional t) = do
                    r <- getStdRandom random
                    if r then (generateTree g (c + 1) t) else (pure "")

generateData :: GrammarInstance ->  IO String
generateData g = do
            let s = findStart $ grammar g
            case s of 
                 Just t -> generateTree g 0 t
                 Nothing -> pure "error"

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

findRule :: String -> Grammar -> Maybe Production
findRule s g = Map.lookup s g


{- Starts at rule 'start' -}
findStart g = findRule "start" g

printGrammar t = mapM_ (\v -> putStrLn $ v) 
            (Map.mapWithKey (\k v-> ((show k) ++ ": " ++  (showTree v))) t)



main :: IO ()
main = do
    file1 <- B.readFile ebnfFile
    let p = parseOnly (parseGrammar <* endOfInput) file1
    overrideFileExsist <- doesFileExist overrideFile
    if overrideFileExsist
    then 
        case p of
            Left err ->putStrLn $ "A parsing error was found: " ++ err
            Right t -> do 
                file2 <- B.readFile overrideFile
                let ov = parseOnly (parseOverride <* endOfInput) file2
                case ov of
                    Left err -> do 
                        putStrLn $ "A parsing error was found: " ++ err
                        exitFailure
                    Right ovi -> do
                        g <- generateData $ GrammarInstance t $ ovi
                        --printGrammar t
                        putStrLn g
    else 
        case p of
            Left err -> do 
                putStrLn $ "A parsing error was found: " ++ err
                exitFailure
            Right t -> do
                g <- generateData $ GrammarInstance t $ Map.empty
                --printGrammar t
                putStrLn g


