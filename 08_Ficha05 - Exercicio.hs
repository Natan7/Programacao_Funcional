data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part | Total Int Int
