data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               } deriving (Show)

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age       :: Int
                     } deriving (Show, Eq)

mikeD  = Person {firstName = "Michael", lastName = "Diamond",  age = 43}
adRock = Person {firstName = "Adam",    lastName = "Horovitz", age = 41}
mca    = Person {firstName = "Adam",    lastName = "Yauch",    age = 44}

