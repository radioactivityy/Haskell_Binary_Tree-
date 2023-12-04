data Point = Point { column :: Int, row :: Int } 

data Event = MouseEvent Point
            | KeyEvent {keyPressed::Char} deriving (Show)

data Button = Button
  { buttonName :: String
  , buttonPosition :: Position
  , buttonText :: String
  , onClick :: Maybe (Event -> String)
  } 

data Component
  = TextBox { name :: String, position :: Position, text :: String }
  | ButtonComponent Button
  | Container { name :: String, children :: [Component] } 

data Position = Position { leftTopCorner :: Point, width :: Int, height :: Int } 


gui :: Component
gui =
  Container "My App"
    [ Container "Menu"
        [ ButtonComponent $ Button "btn_new" (Position (Point 0 0) 100 20) "New" (Just (\event -> "Clicked on new button.")),
          ButtonComponent $ Button "btn_open" (Position (Point 100 0) 100 20) "Open" Nothing,
          ButtonComponent $ Button "btn_close" (Position (Point 200 0) 100 20) "Close" (Just (\event -> "Clicked on close button."))
        ],
      Container "Body" [TextBox "textbox_1" (Position (Point 0 20) 300 500) "Some text goes here"],
      Container "Footer" []
    ]

clickOnButton :: Component -> Event -> String
clickOnButton (ButtonComponent (Button _ _ _ (Just onClick))) event = onClick event
clickOnButton _ _ = "No action specified for this event."

instance Show Component where
  show comp = showIndented "" comp
    where
      showIndented :: String -> Component -> String
      showIndented indent (TextBox name pos text) =
        indent ++ show pos ++ " TextBox[" ++ name ++ "]: " ++ text ++ "\n"
      showIndented indent (ButtonComponent (Button name pos text onClick)) =
        indent ++ show pos ++ " Button[" ++ name ++ "]: " ++ text ++ "\n"
      showIndented indent (Container name children) =
        indent ++ "Container - " ++ name ++ "\n" ++ concatMap (showIndented (indent ++ "    ")) children


instance Show Position where
  show (Position (Point c r) w h) = "(" ++ show c ++ "," ++ show r ++ ") [" ++ show w ++ "," ++ show h ++ "]"

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

