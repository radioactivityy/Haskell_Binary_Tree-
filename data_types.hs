 data Direction = North | West 
                 | South | East deriving (Show, Enum) -- enum: a type that has a finite set of values
 --  Here's an example of an enumeration type that represents the four seasons − data Season = Spring | Summer | Fall | Winter

 turnLeft East = West 
 turnLeft x = succ x 

-- successorOfA = succ A  -- Returns B
-- successorOfB = succ B  -- Returns C
-- successorOfC = succ C  -- This would result in a runtime error since there's no successor after C


 turnRight North = East 
 turnRight x = pred x 

 turnAround x = turnRight (turnRight x)

