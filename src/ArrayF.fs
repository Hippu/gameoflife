module ArrayF
/// Simple and naive operations for two-dimensional arrays
/// since Fable doesn't support the default multidimensional arrays

/// Initialize the two dimensional array with the given initializer function
let init (length1: int) (length2: int) (initializer: int -> int -> 'T) =
    [| for x in 0 .. length1 ->
        [| for y in 0 .. length2 -> (initializer x y)|] 
    |]

/// Applies the function to the elements of the source array and puts the result in the target array
let mapiTo (transform: int -> int -> 'T -> 'T) (source: 'T[][]) (target: 'T[][]) =
    for x in 0 .. source.Length - 1 do
        let length = source.[x].Length
        for y in 0 .. length - 1 do
            target.[x].[y] <- transform x y source.[x].[y]

/// Sets all the values in the array
let setAll (value: 'T) (array: 'T[][]) =
    for x in 0 .. array.Length - 1 do
        let length = array.[x].Length
        for y in 0 .. length - 1 do
            array.[x].[y] <- value
            
/// Iterate through all the cells in an array and give them as parameters to the 'action' function            
let iteri (action: int -> int -> 'T -> unit) (array: 'T[][]) =
    for x in 0 .. array.Length - 1 do
        for y in 0 .. array.[x].Length - 1 do
            action x y array.[x].[y]