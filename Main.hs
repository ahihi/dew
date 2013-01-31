{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module Main where

import Control.Applicative
    ( Applicative 
        ( (<*>)
        )
    , (<$>)
    )
import Control.Arrow
import Control.Exception
    ( try
    )
import Control.Monad
    ( guard
    , liftM
    , liftM2
    , when
    )
import Data.Char
    ( chr
    , ord
    )
import Data.List
    ( foldl'
    , genericIndex
    , genericLength
    , group
    , intercalate
    )
import Data.Maybe
    ( catMaybes
    )
import System.Directory
    ( canonicalizePath
    )
import System.Environment
    ( getArgs
    )
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , stdin
    , stdout
    )
import Text.Parsec.Prim
    ( Stream
    , ParsecT
    )
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec hiding (satisfy, try)

type Arity = Integer
type Index = Integer
data Instruction
    = App Index Index
    | Abs Arity CodeBlock
    | Out
    | Succ
    | Character Char
    | In
    deriving Show
type CodeBlock = [Instruction]
type Object = (CodeBlock, Env)
newtype Env = Env [Object]
    deriving Show
type Dump = [(CodeBlock, Env)]
type Configuration = (CodeBlock, Env, Dump)

data Token
    = TW
    | Tw
    | Tv
    deriving (Eq, Show)

interactive :: IO ()
interactive = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    go []
    where
        go history = do
            let source = buildSource history
            case parseGrass source of
                Left _     -> return ()
                Right prog -> do
                    runCodeBlock True prog
                    return ()
                    --putStrLn $ "Prog: " ++ show code
                    --putStrLn $ "Env:  " ++ show env
                    --putStrLn $ "Dump: " ++ show dump
            putStr "> "
            line <- getLine
            case line of
                "!back" -> go (drop 1 history)
                "!show" -> do
                    putStrLn $ buildSource history
                    go history
                "!quit" -> return ()
                _       -> do
                    let history' = line : history
                    let source' = buildSource history'
                    case parseGrass source' of
                        Left e     -> do
                            print e
                            go history
                        Right prog -> do
                            go history'
        buildSource = concat . reverse

class Pretty a where
    pretty :: a -> String

instance Pretty a => Pretty [a] where
    pretty xs = "[" ++ intercalate ", " (map pretty xs) ++ "]"

instance Pretty Configuration where
    pretty (c, e, d) = unlines
        [ "CodeBlock: " ++ pretty c
        , "Env:       " ++ pretty e
        , "Dump:      " ++ pretty d
        ]

instance Pretty Object where
    pretty (c, e) = "(" ++ pretty c ++ "," ++ pretty e ++ ")"

instance Pretty Env where
    pretty (Env e) = pretty e

instance Pretty Instruction where
    pretty (App m n)         = show m ++ " " ++ show n
    pretty (Abs 1 [App 3 2]) = "ChurchTrue"
    pretty (Abs 1 [])        = "ChurchFalse"
    pretty (Abs n c)         = "\\ " ++ show n ++ " -> " ++ pretty c
    pretty Out               = "Out"
    pretty Succ              = "Succ"
    pretty (Character ch)    = "Character " ++ show (ord ch)
    pretty In                = "In"
    
main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    case args of
        (fp:_) -> canonicalizePath fp >>= readFile >>= runGrass >> return ()
        _      -> putStrLn "No input file given!"

runGrass :: String -> IO Configuration
runGrass = runGrass' False

runGrass' :: Bool -> String -> IO Configuration
runGrass' debug str = case parseGrass str of
    Left e   -> error $ "runGrass: " ++ show e
    Right cb -> runCodeBlock debug cb

runCodeBlock :: Bool -> CodeBlock -> IO Configuration
runCodeBlock debug prog = do
    run $ defaultConfiguration prog
    where
        run conf = do
            when debug $ putStr $ "\n" ++ pretty conf
            case conf of
                ([], Env [f], []) -> putStrLn "" >> return conf
                _                 -> transform conf >>= run

defaultConfiguration :: CodeBlock -> Configuration
defaultConfiguration prog = (prog, defaultEnv, defaultDump)
    where
        defaultEnv = Env $ map mkObj [Out, Succ, Character 'w', In]
        defaultDump = [mkObj $ App 1 1, ([], Env [])]

transform :: Configuration -> IO Configuration
transform conf = case conf of
    (App m n : c, e@(Env el), d) -> return (c_m, Env $ (c_n, e_n) : el_m, (c, e) : d)
        where
            (c_m, e_m@(Env el_m)) = el !- m
            (c_n, e_n@(Env el_n)) = el !- n
    (Abs n c' : c, e@(Env el), d) -> return (c, Env $ (if n == 1 then (c', e) else ([Abs (n-1) c'], e)) : el, d)
    (Out : c, e@(Env (arg:_)), d) -> case arg of
        ([Character ch], _) -> putChar ch >> return (c, e, d)
        (v, _)              -> error $ "Grass: type mismatch - cannot apply Out to " ++ show v
    (Succ : c, Env (arg:et), d) -> case arg of
        ([Character ch], _) -> return (c, Env $ mkObj (Character $ chr $ (ord ch + 1) `mod` 256) : et, d)
        (v, _)              -> error $ "Grass: type mismatch - cannot apply Succ to " ++ show v
    (Character ch : c, Env (arg:et), d) -> case arg of
        ([Character ch'], _) -> return (c, Env $ mkObj (church $ ch == ch') : et, d)
        (v, _)               -> error $ "Grass: type mismatch - cannot apply Character to " ++ show v
    (In : c, Env (arg:et), d) -> do
        input <- try getChar :: IO (Either IOError Char)
        result <- return $ case input of
            Left _   -> arg
            Right ch -> mkObj $ Character ch
        return (c, Env $ result : et, d)
    ([], Env (f:_), (c', Env et') : d) -> return (c', Env $ f:et', d)
    where
        xs !- n = xs `genericIndex` (n-1)
        church True = Abs 1 [App 3 2]
        church False = Abs 1 []

mkObj :: Instruction -> Object
mkObj x = ([x], Env [])

lcV :: [Char]
lcV = "v\xFF56"

lcW :: [Char]
lcW = "w\xFF57"

ucW :: [Char]
ucW = "W\xFF37"

tokenToChar :: Token -> Char
tokenToChar TW = 'W'
tokenToChar Tw = 'w'
tokenToChar Tv = 'v'

parseGrass = parseProgram . preprocess

parseProgram = runParser program () "grass"

program = (\ abss apps _ -> abss ++ apps) <$> abstractionPart <*> applicationPart <*> eof

abstractionPart = abstraction `sepEndBy1` v

applicationPart = application `sepEndBy` v

abstraction = Abs <$> numberOf1 (literal Tw) <*> many application

application = App <$> numberOf1 (literal TW) <*> numberOf1 (literal Tw)

v = many (literal Tv)

withRest :: (Stream s m t, Show t) => ParsecT s u m a -> ParsecT s u m (a, [t])
withRest p = (,) <$> p <*> many anyToken

numberOf1 :: (Stream s m t, Integral i) => ParsecT s u m a -> ParsecT s u m i
numberOf1 p = genericLength <$> many1 p

literal :: (Stream s m Token) => Token -> ParsecT s u m Token
literal c = satisfy (== c)

satisfy :: (Stream s m Token) => (Token -> Bool) -> ParsecT s u m Token
satisfy p = tokenPrim (\ t -> show t) (\ pos t _ -> updatePosChar pos $ tokenToChar t) (\ t -> if p t then Just t else Nothing)

preprocess :: String -> [Token]
preprocess = dropWhile (/= Tw) . tokenize
    where
        tokenize = catMaybes . map toToken
            where
                toToken c
                    | c `elem` lcV = Just Tv
                    | c `elem` lcW = Just Tw
                    | c `elem` ucW = Just TW
                    | otherwise    = Nothing
