import Data.IntMap (insert)
data Point = Point {column::Int,row::Int}

data Position = Position {leftTopCorner :: Point, width :: Int, height :: Int} 

data Component
  = TextBox {name :: String, position :: Position, text :: String}
  | Button {name :: String, position :: Position, text :: String}
  | Container {name :: String, children :: [Component]}


gui :: Component
gui =
  Container "My App"
    [ Container "Menu"
        [ Button "btn_new" (Position (Point 0 0) 100 20) "New",
          Button "btn_open" (Position (Point 100 0) 100 20) "Open",
          Button "btn_close" (Position (Point 200 0) 100 20) "Close"
        ],
      Container "Body" [TextBox "textbox_1" (Position (Point 0 20) 300 500) "Some text goes here"],
      Container "Footer" []
    ]

instance Show Position where 
    show (Position (Point col row) width height) =
         "(" ++ show col ++ "," ++ show row ++")["++ show width++ "," ++ show height++"]"

    