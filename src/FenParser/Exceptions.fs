namespace FenParser

type IllegalMoveException(message) =
    inherit System.Exception(message)

[<AutoOpen>]
module ExceptionFunctions =
    let inline illegalMove message =
        raise <| IllegalMoveException(message)
