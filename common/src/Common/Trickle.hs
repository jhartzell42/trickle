module Common.Trickle
    ( parseProgram
    , Program (..)
    , VariableName (..)
    , VariableSigil (..)
    ) where

import qualified Text.Parsec as Parsec
import Data.Char (isLetter)
import qualified Data.Text as T
import Control.Monad

data Program = Program VariableName
    deriving Show

program :: Parsec.Parsec T.Text () Program
program = Program <$> variableName

data VariableName = VariableName
    { variableReferenceSigils :: [VariableSigil]
    , variableIdentifier :: T.Text
    }
    deriving Show

variableName :: Parsec.Parsec T.Text () VariableName
variableName = VariableName <$> Parsec.many1 variableSigil <*> lowerCamelCase

lowerCamelCase :: Parsec.Parsec T.Text () T.Text
lowerCamelCase = fmap T.pack $ do
    s <- Parsec.many1 $ Parsec.choice
        [ Parsec.letter
        , Parsec.char '_'
        , Parsec.digit
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

variableSigil :: Parsec.Parsec T.Text () VariableSigil
variableSigil = Parsec.choice
    [ VariableSigilPure <$ Parsec.char '$'
    , VariableSigilWidget <$ Parsec.char '@'
    , VariableSigilOptional <$ Parsec.char '?'
    , VariableSigilDynamic <$ Parsec.char '%'
    , VariableSigilEvent <$ Parsec.char '!'
    ]

parseProgram :: T.Text -> Either Parsec.ParseError Program
parseProgram text = Parsec.parse program "(source)" text
