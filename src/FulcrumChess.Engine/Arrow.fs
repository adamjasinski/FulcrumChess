namespace FulcrumChess.Engine
    
// F# version of Haskell &&& operator. See https://wiki.haskell.org/Arrow_tutorial, http://onoffswitch.net/arrow-operator/
module Arrow =     
    let private split x = (x, x)
    let private combine f (x, y) = f x y
    let private first f (a, b) = (f a, b)
    let private second f (a, b) = (a, f b)
    let onTuple f g = first f >> second g
    let onSingle f g =  split >> (onTuple f g)
    let (.***.) = onTuple
    let (.&&&.) = onSingle
    let onSingleCombine op f g = (onSingle f g) >> combine op

[<AutoOpen>]
module ValueArrow =     
    let private split x = struct(x, x)
    let private combine f struct(x, y) = f x y
    let private first f struct(a, b) = struct(f a, b)
    let private second f struct(a, b) = struct(a, f b)
    let onTuple f g = first f >> second g
    let onSingle f g =  split >> (onTuple f g)
    let (.***.) = onTuple
    let (.&&&.) = onSingle
    let onSingleCombine op f g = (onSingle f g) >> combine op