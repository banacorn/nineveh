module Value.Type (Value(..), TimingFunctionDirection(..)) where

data Value  = 

            -- Color
              RGB (Value, Value, Value)
            | RGBA (Value, Value, Value, Value)
            | HSL (Value, Value, Value)
            | HSLA (Value, Value, Value, Value)
            | Hex String
            | Transparent
            | CurrentColor
            | ColorKeyword String

            -- Number
            | Number Double
            
            -- Integer'
            | Integer' Integer

            -- Percentage
            | Percentage Double

            -- Length
            | Em Double
            | Ex Double
            | Ch Double
            | Rem Double
            | Vh Double
            | Vw Double
            | Vmin Double
            | Vmax Double
            | Px Double
            | Mm Double
            | Cm Double
            | In Double
            | Pt Double
            | Pc Double

            -- Angle
            | Deg Double
            | Grad Double
            | Rad Double
            | Turn Double

            -- Position
            | Static
            | Relative
            | Absolute
            | Fixed
            | Inherit

            -- Ratio
            | Ratio Integer Integer

            -- Frequency
            | Hz Double
            | KHz Double

            -- Resolution
            | Dpi Double
            | Dpcm Double
            | Dppx Double

            -- Shape
            | Shape (Value, Value, Value, Value)

            -- String
            | String' String

            -- Url
            | Url String

            -- Time
            | S Double
            | Ms Double

            -- Timing Function
            | CubicBezier (Double, Double, Double, Double)
            | Steps (Integer, TimingFunctionDirection)

            -- Variable
            | Variable String

            -- None
            | None

data TimingFunctionDirection = Start | End deriving (Eq)