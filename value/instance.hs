module Value.Instance where

import Value.Type

show' :: Double -> String
show' n = if floor n == ceiling n then show (floor n) else show n

instance Show Value where

    -- Color
    show (RGB (r, g, b))        = "rgb(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ")"
    show (RGBA (r, g, b, a))    = "rgba(" ++ show r ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++ show a ++ ")"
    show (HSL (h, s, l))        = "hsl(" ++ show h ++ ", " ++ show s ++ ", " ++ show l ++ ")"
    show (HSLA (h, s, l, a))    = "hsl(" ++ show h ++ ", " ++ show s ++ ", " ++ show l ++ ", " ++ show a ++ ")"
    show (Hex s)                = s
    show Transparent            = "transparent"
    show CurrentColor           = "currentColor"
    show (ColorKeyword color)   = color
    
    -- Number
    show (Number n) = show' n

    -- Integer
    show (Integer' i) = show i


    -- Percentage
    show (Percentage n) = show' n ++ "%"


    -- Length
    show (Em n) = show' n ++ "em"
    show (Ex n) = show' n ++ "ex"
    show (Ch n) = show' n ++ "ch"
    show (Rem n) = show' n ++ "rem"
    show (Vh n) = show' n ++ "vh"
    show (Vw n) = show' n ++ "vw"
    show (Vmin n) = show' n ++ "vmin"
    show (Vmax n) = show' n ++ "vmax"
    show (Px n) = show' n ++ "px"
    show (Mm n) = show' n ++ "mm"
    show (Cm n) = show' n ++ "cm"
    show (In n) = show' n ++ "in"
    show (Pt n) = show' n ++ "pt"
    show (Pc n) = show' n ++ "pc"

    -- Angle

    show (Deg n) = show' n ++ "deg"
    show (Grad n) = show' n ++ "grad"
    show (Rad n) = show' n ++ "rad"
    show (Turn n) = show' n ++ "turn"

    -- Position
    show Static = "static"
    show Relative = "relative"
    show Absolute = "absolute"
    show Fixed = "fixed"
    show Inherit = "inherit"

    -- Ratio
    show (Ratio n d) = show n ++ "/" ++ show d

    -- Frequency
    show (Hz n) = show' n ++ "Hz"
    show (KHz n) = show' n ++ "KHz"

    -- Resolution
    show (Dpi n) = show' n ++ "dpi"
    show (Dpcm n) = show' n ++ "dpcm"
    show (Dppx n) = show' n ++ "dppx"

    -- Shape
    show (Shape (top, right, bottom, left)) = 
        "rect(" ++ show top ++ ", " ++ show right ++ ", " ++ show bottom ++ ", " ++ show left ++ ")"

    -- String
    show (String' s) = "\'" ++ s ++ "\'"

    -- Url
    show (Url s) = "url('" ++ s ++ "')"

    -- Time
    show (S n) = show n ++ "s"
    show (Ms n) = show n ++ "ms"

    -- Timing Function
    show (CubicBezier (x1, y1, x2, y2)) = "cubic-bezier(" ++ show x1 ++ ", " ++ show y1 ++ ", " ++ show x2 ++ ", " ++ show y2 ++ ")"
    show (Steps (n, direction)) = "steps(" ++ show n ++ ", " ++ show direction ++ ")"

instance Show TimingFunctionDirection where
    show Start = "start"
    show End = "end"

