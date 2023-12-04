import Text.XHtml (height)
data Shape = Circle Float | Rectangle Float Float deriving (Show, Eq)

circleExample = Circle 5.0
rectangleExample = Rectangle 3.0 4.0

calculateRectangle :: Shape -> Float
calculateRectangle  (Rectangle width height) =  width * height

