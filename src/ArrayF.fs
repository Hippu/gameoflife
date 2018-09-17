module ArrayF
/// Simple and naive operations for two-dimensional arrays
/// since Fable doesn't support the default multidimensional arrays
let init (length1: int) (length2: int) (initializer: int -> int -> 'T) =
    [| for x in 0 .. length1 ->
        [| for y in 0 .. length2 -> (initializer x y)|] 
    |]

let get (array: 'T[][]) (index1: int) (index2: int) =
    array.[index1].[index2]

let set (array: 'T[][]) (index1: int) (index2: int) (value: 'T) =
    array.[index1].[index2] <- value

let mapi (transform: int -> int -> 'T -> 'U) (array: 'T[][]) =
    [| for x in 0 .. array.Length - 1 ->
        [| for y in 0 .. array.Length - 1 -> transform x y (array.[x].[y]) |]
    |]

let mapiTo (transform: int -> int -> 'T -> 'T) (source: 'T[][]) (target: 'T[][]) =
    for x in 0 .. source.Length - 1 do
        let length = source.[x].Length
        for y in 0 .. length - 1 do
            target.[x].[y] <- transform x y source.[x].[y]

let setAll (value: 'T) (array: 'T[][]) =
    for x in 0 .. array.Length - 1 do
        let length = array.[x].Length
        for y in 0 .. length - 1 do
            array.[x].[y] <- value
            

let iteri (action: int -> int -> 'T -> unit) (array: 'T[][]) =
    for x in 0 .. array.Length - 1 do
        for y in 0 .. array.[x].Length - 1 do
            action x y array.[x].[y]