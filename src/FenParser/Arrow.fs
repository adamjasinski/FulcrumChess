namespace FenParser
    
// F# version of Haskell &&& operator. See http://onoffswitch.net/arrow-operator/.
module Arrow =     
    let split x = (x, x)
    let combine f (x, y) = f x y
    let first f (a, b) = (f a, b)
    let second f (a, b) = (a, f b)
    let onTuple f g = first f >> second g
    let onSingle f g =  split >> (onTuple f g)
    let (.***.) = onTuple
    let (.&&&.) = onSingle
    let onSingleCombine op f g = (onSingle f g) >> combine op