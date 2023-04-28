
module REPLCommand where

import Lab2
import Control.Applicative (many, (<|>))

data REPLCommand
  = Quit
  | Load String
  | Eval String

parseQ :: Parser REPLCommand 
parseQ = do
              reserved ":q"
              return (Quit)

parseQuit :: Parser REPLCommand 
parseQuit = do
              reserved ":quit"
              return (Quit)

parseL :: Parser REPLCommand 
parseL = do
              reserved ":l"
              s <- many anychar
              return (Load s)

parseLoad :: Parser REPLCommand 
parseLoad = do
              reserved ":load"
              s <- many anychar
              return (Load s)

parseEval :: Parser REPLCommand 
parseEval = do
              s <- many anychar
              return (Eval s)
              

replCommand :: Parser REPLCommand
replCommand = parseQ <|> parseQuit <|> parseL <|> parseLoad <|> parseEval

