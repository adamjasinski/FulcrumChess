module Puzzles1

type EmailAddress = EmailAddress of string

let test1 () =
//    "a" |> EmailAddress
//    ["a"; "b"; "c"] |> List.map EmailAddress

    // inline deconstruction
    let a' = "a" |> EmailAddress
    let (EmailAddress a'') = a'
    a''

let findCommonInSortedInputsImperative (A:int array) (B:int array) =
    //Imperative approach
    let mutable i = 0
    let mutable j = 0
    let mutable result = List.empty
    while i < A.Length && j < B.Length do
        if A.[i] = B.[j] then
            result <- A.[i] :: result
            i <- i + 1
            j <- j + 1
        else if A.[i] > B.[j] then
            j <- j + 1
        else
            i <- i + 1
    result |> List.rev |> Array.ofList

let findCommonInSortedInputsFunctional (A:int array) (B:int array) =
    //Functional approach
    let rec loop (lst1:int list) (lst2:int list) (acc:int list) : int list=
        match (lst1, lst2) with
        | (_, []) -> acc
        | ([], _) -> acc
        | (head1::tail1, head2::tail2) -> 
            if head1 = head2 then
                loop tail1 tail2 (head1::acc)
            else if head1 > head2 then
                loop lst1 tail2 acc
            else
                loop tail1 lst2 acc
                
    loop (List.ofArray A) (List.ofArray B) [] |> List.rev |> Array.ofList

