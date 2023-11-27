data Point = Point { column :: Int, row :: Int } 

data Position = Position { leftTopCorner :: Point, width :: Int, height :: Int } 

data Component
  = TextBox { name :: String, position :: Position, text :: String } 
  | Button { name :: String, position :: Position, text :: String } 
  | Container { name :: String, children :: [Component] } 

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

instance Show Component where
  show comp = showIndented "" comp
    where
      showIndented :: String -> Component -> String
      showIndented indent (TextBox name pos text) =
        indent ++ show pos ++ " TextBox[" ++ name ++ "]: " ++ text ++ "\n"
      showIndented indent (Button name pos text) =
        indent ++ show pos ++ " Button[" ++ name ++ "]: " ++ text ++ "\n"
      showIndented indent (Container name children) =
        indent ++ "Container - " ++ name ++ "\n" ++ concatMap (showIndented (indent ++ "    ")) children


instance Show Position where
  show (Position (Point c r) h w) = "(" ++ show c ++ "," ++ show r ++ ") [" ++ show w ++ "," ++ show h ++ "]"

instance Show Point where
  show (Point col row) = "Point {column = " ++ show col ++ ", row = " ++ show row ++ "}"

insertInto :: Component -> String -> Component -> Component
insertInto component targetContainerName newItem =
  insertInto' component
  where
    insertInto' :: Component -> Component
    insertInto' (Container name children)
      | name == targetContainerName = Container name (children ++ [newItem])
      | otherwise = Container name (map insertInto' children)
    insertInto' otherComponent = otherComponent

