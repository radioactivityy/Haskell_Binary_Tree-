data RGBColor = RGBColor Int Int Int deriving (Show)

red = RGBColor 255 00
green = RGBColor 0 255 0
blue = RGBColor 0 0 255 

addColor (RGBColor r1 g1 b1) (RGBColor r2 g2 b2) = 
    RGBColor (r1 + r2) (g1+ g2) (b1+b2)
