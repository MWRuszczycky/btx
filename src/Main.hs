{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text      as Tx
import qualified Types          as T
import System.Environment               ( getArgs             )
import System.Directory                 ( listDirectory
                                        , getCurrentDirectory )
import Control.Monad.State.Lazy         ( execStateT, get
                                        , liftIO              )
import Control.Monad.Except             ( runExceptT
                                        , throwError
                                        , liftEither          )
import Commands                         ( runHelp             )
import CoreIO                           ( readOrMakeFile      )
import BibTeX.Parser                    ( parseBib            )
import Commands                         ( route, done
                                        , updateIn            )

-- =============================================================== --
-- Entry point and clean up

main :: IO ()
main = do
    -- start <- parseCmds . unwords <$> getArgs
    start <- parseArgs =<< getArgs
    case start of
         T.Usage msg   -> finish . Left $ msg
         T.Help xs     -> finish . Left . runHelp $ xs
         T.Normal fp s -> runExceptT ( initBtx s fp >>= runBtx ) >>= finish

finish :: Either String T.BtxState -> IO ()
finish (Left msg) = putStrLn msg
finish (Right _ ) = putStrLn "\nDone."

-- =============================================================== --
-- Initialization

initBtx :: a -> FilePath -> T.ErrMonad ( a, T.BtxState )
-- ^Generate the initial state.
initBtx x [] = initDefaultBtxState x
initBtx x fp = do
    content <- readOrMakeFile fp
    bib <- liftEither . parseBib fp $ content
    return ( x, initBtxState bib )

initDefaultBtxState :: a -> T.ErrMonad (a, T.BtxState)
initDefaultBtxState x = do
    cwd <- liftIO getCurrentDirectory
    fps <- liftIO . listDirectory $ cwd
    case findSingleBibFile fps of
         Just fp -> initBtx x fp
         Nothing -> throwError $ "Cannot find a unique default .bib file in "
                                 ++ "the current directory.\n"
                                 ++ "  (Try: btx help in)"

initBtxState :: T.Bibliography -> T.BtxState
initBtxState b =  T.BtxState { T.inBib   = b
                             , T.toBib   = Nothing
                             , T.fromBib = Nothing
                             , T.logger  = Tx.empty
                             }

findSingleBibFile :: [FilePath] -> Maybe FilePath
findSingleBibFile xs
    | null ys       = Nothing
    | length ys > 1 = Nothing
    | otherwise     = Just . head $ ys
    where ys = filter ( ( == "bib." ) . take 4 . reverse ) xs

-- =============================================================== --
-- Compiling and running

runBtx :: ([T.CommandArgsMonad [T.Ref]], T.BtxState) -> T.ErrMonad T.BtxState
-- ^Compile and run the commands an the initial state.
runBtx (cmds, st) = execStateT ( compile cmds [] ) st

---------------------------------------------------------------------
-- Compiling

compile :: [ T.CommandArgsMonad [T.Ref] ] -> [T.Ref] -> T.BtxStateMonad ()
compile []        rs = done rs
compile ( c : cs) rs = c rs >>= compile cs

---------------------------------------------------------------------
-- Parsing

parseArgs :: [String] -> IO ( T.Start ( T.CommandArgsMonad [T.Ref] ) )
parseArgs ("run":fp:_) = readFile fp >>= return . parseCmds
parseArgs ("run":[])   = getContents >>= return . parseCmds
parseArgs args         = return . parseCmds . unwords $ args

parseCmds :: String -> T.Start ( T.CommandArgsMonad [T.Ref] )
parseCmds xs =
    case words . splitAnd $ xs of
         []            -> T.Usage "This won't do anything (try: btx help)."
         ("in":[])     -> T.Usage "This won't do anything (try: btx help)."
         ("help":cs)   -> T.Help cs
         ("--help":cs) -> T.Help cs
         ("-h":cs)     -> T.Help cs
         ("in":fp:cs)  -> T.Normal fp . parseAnd $ cs
         cs            -> T.Normal [] . parseAnd $ cs

splitAnd :: String -> String
splitAnd []        = []
splitAnd (',':xs)  = " and " ++ splitAnd xs
splitAnd ('\n':xs) = " and " ++ splitAnd xs
splitAnd (x:xs)    = x : splitAnd xs

parseAnd :: [String] -> [ T.CommandArgsMonad [T.Ref] ]
parseAnd []          = []
parseAnd ("and":xs)  = parseAnd xs
parseAnd (x:xs)      = applyCmd x ys : parseAnd zs
    where (ys,zs)  = break ( == "and" ) xs
          applyCmd = T.cmdCmd . route
