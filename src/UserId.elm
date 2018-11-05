module UserId exposing (UserId, fromString, toString)


type UserId
    = UserId String


fromString : String -> UserId
fromString id =
    UserId id


toString : UserId -> String
toString (UserId id) =
    id
