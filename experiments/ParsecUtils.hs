{-# LANGUAGE FlexibleContexts #-}

module ParsecUtils (natural) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim
import Data.Functor ((<$>))

natural :: Text.Parsec.Prim.Stream s m Char => Text.Parsec.Prim.ParsecT s u m Int
natural = read <$> many1 digit