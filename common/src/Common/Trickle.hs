module Common.Trickle
    ( parseProgram
    , Program (..)
    , VariableName (..)
    , VariableSigil (..)
    ) where

import qualified Text.Parsec as Parsec
import Data.Char (isLetter)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Parsec (<?>)
import Control.Applicative
import Control.Monad.Identity (Identity)

data Program = Program VariableName
    deriving Show

program :: Parsec.Parse T.Text () Program
program = Program <$> variableName

data VariableName = VariableName
    { variableReferenceSigils :: [VariableSigil]
    , variableName :: T.Text
    }
    deriving Show

variableName :: Parsec.Parse T.Text () VariableName
variableName = VariableName <$> many1 variableSigil <*> lowerCamelCase

lowerCamelCase :: Parsec.Parse T.Text () T.Text
lowerCamelCase = T.pack $ do
    s <- Parsec.many1 $ choice
        [ letter
        , char '_'
        , digit
        ]
    guard $ isLetter $ head s
    pure s

data VariableSigil
    = VariableSigilPure
    | VariableSigilWidget
    | VariableSigilOptional
    | VariableSigilDynamic
    | VariableSigilEvent
    deriving Show

variableSigil :: Parsec.Parse T.Text () VariableSigil
variableSigil = choice
    [ VariableSigilPure <$ char '$'
    , VariableSigilWidget <$ char '@'
    , VariableSigilOptional <$ char '?'
    , VariableSigilDynamic <$ char '%'
    , VariableSigilEvent <$ char '!'
    ]

parseProgram :: T.Text -> Program
parseProgram text = Parsec.parse program "(source)"
