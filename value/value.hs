module Value.Value (
        Value(..), 
        parseValue
    ) where

import Text.ParserCombinators.Parsec
import Parser

import Value.Type
import Value.Instance
import Value.Color
import Value.Number
import Value.Percentage
import Value.Integer
import Value.Length
import Value.Position
import Value.Ratio
import Value.Frequency
import Value.Resolution
import Value.Shape
import Value.Angle
import Value.String
import Value.Url
import Value.Time
import Value.TimingFunction

parseValue  =       
                try parseShape
            <|> try parseUrl
            <|> try parseTimingFunction
            <|>     parseString
            <|> try parsePosition
            <|> try parseLength
            <|> try parseFrequency
            <|> try parseResolution
            <|> try parseAngle
            <|> try parseRatio
            <|> try parsePercentage
            <|> try parseTime
            <|> try parsePercentage
            <|>     parseNumber
            <|>     parseInteger
            <|>     parseColor 

test = mapM (run parseValue) [
        "10",
        "10.5",
        "20px",
        "30em",
        "red",
        "#345",
        "rgba(2, 4, 5, 0.5)",
        "inherit"
    ]
