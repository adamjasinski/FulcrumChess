namespace TestHelpers

open Xunit.Sdk
open System.IO
//type LongAlgebraicMove = {From:string}
type MoveGenTestRecord = {Header:string; FEN:string; ExpectedMoves:string list}

//type CCz = |Title of string|Fen of string|Comment|Blank|Move of string*string


type MoveGenTestDataFileAttribute(filename:string) =
    inherit DataAttribute()
    let (|Fen|_|) (line:string) =
        if line |> String.exists ((=) '/') then Some line else None
        //if line |> String.exists (fun c -> c = '/') then Some line else None
    let (|Comment|_|) (line:string) =
        if line.StartsWith("#") then Some line else None
    let (|Blank|_|) (line:string) =
        if System.String.IsNullOrWhiteSpace(line) then Some line else None
    let (|Header|_|) (line:string) =
        if line.StartsWith(";") then Some line else None
//    let (|Move|_|) (line:string) =
//        if line.StartsWith(";") then Some line else None

    let rec parseLoop (state:MoveGenTestRecord list*MoveGenTestRecord option) lines =
        let (acc,current) = state
        match lines with
        | [] ->
            match current with
            | Some c -> c :: acc
            | None -> acc
        | hd::tail ->
            match hd with
            | Comment _ -> parseLoop state tail
            | Header header -> parseLoop (acc, {MoveGenTestRecord.Header=header;FEN="";ExpectedMoves=List.empty} |> Some) tail
            | Fen fen -> parseLoop (acc, current |> Option.map (fun c -> {c with FEN=fen})) tail
            | Blank _ -> match current with
                            | Some c -> parseLoop (c::acc, None) tail 
                            | None -> parseLoop state tail
            | move -> parseLoop (acc, current |> Option.map (fun c -> {c with ExpectedMoves=move::c.ExpectedMoves})) tail

        //if line |> String.exists (fun c -> c = '/') then Some line else None
//    let parse (lines:string seq) =
//        let rec loop (stream) =
//            let c = stream |> head
//            match c with
//            |Title title -> loopSection next(stream) title
//        and loopSection (stream) fen moves =
//            let c = stream |> head
//            match c with
//            |Fen fen -> loopSection next(stream) title fen []
//            |Comment _ -> loopSection next(stream) title fen moves
//            |Move src,dst -> loopSection next(stream) title fen (src,dest)::moves
//            |Blank _ -> loop next(stream) 
//   

//    let testDataRowsToSingleRecord rows =
//        rows |> Seq.fold (fun state row ->
//            match row with
//            | 
//        )
    let readRecordsFromFile () =
//       let sr = StreamReader(filename)
//       sr.ReadLine
        let lines = File.ReadAllLines(filename)
        lines |> List.ofSeq |> parseLoop (List.empty,None)

    override this.GetData(methodInfo) = 
        readRecordsFromFile() 
        |> List.map( fun x -> [|x :> obj|]) 
        |> List.rev 
        |> Seq.ofList
