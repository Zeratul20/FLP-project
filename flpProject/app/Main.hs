module Main where

import System.IO

import Lab2
import Exp
import Parsing
import Printing
import REPLCommand
import Sugar
import Eval

main :: IO ()
main = do
        putStr "main>"
        hFlush stdout
        s <- getLine
        let comanda = parseFirst replCommand s
        case comanda of
            Just Quit -> return()
            Just (Load s) -> putStrLn s >> main
            Just (Eval s) -> case res of
                Nothing -> putStrLn "" >> main
                Just exp -> let 
                                eexp = desugarExp exp
                                normalizedExp = normalize eexp
                                ecexp = sugarExp normalizedExp
                            in
                                putStrLn (showExp ecexp) >> main
                where res = parseFirst exprParser s
