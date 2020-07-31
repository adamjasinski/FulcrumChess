namespace FulcrumChess.Engine.Tests
open System
open System.Reflection

type BoardRefAttribute(fen:string, url:string) =
    inherit Attribute()
    member __.FEN = fen
    member __.Url = url
    new(fen:string) = BoardRefAttribute(fen,"")

// Works around F# compiler issue with default InlineData(data:obj array) constructor
// See also https://github.com/xunit/xunit/issues/1225
// See also https://github.com/xunit/xunit/blob/master/src/xunit.core/InlineDataAttribute.cs
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
type InlineDataExAttribute(data:obj array) =
    inherit Xunit.Sdk.DataAttribute()
    new(arg1:obj) = InlineDataExAttribute [|arg1|]
    new(arg1:obj, arg2:obj) = InlineDataExAttribute [|arg1; arg2|]
    new(arg1:obj, arg2:obj, arg3:obj) = InlineDataExAttribute [|arg1; arg2; arg3|]
    override this.GetData(testMethod:MethodInfo) = Seq.singleton data

module TupleHelper = 
        //Taken from https://stackoverflow.com/a/60512370
        let isTuple tuple =
            tuple.GetType() |> Reflection.FSharpType.IsTuple 

        let getFields (tuple: obj) = 
            tuple |> Reflection.FSharpValue.GetTupleFields |> Array.toList

        let rec flatten fields =
            fields
            |> List.collect(
                fun tuple ->
                    if isTuple tuple
                    then flatten (getFields tuple)
                    else [tuple]
            )

        let namer(tuple: obj) = 
            if isTuple tuple
            then tuple |> getFields |> flatten
            else [tuple]

//Extension for MemberDataAttribute that unrolls tuples into array of objects
type MemberDataExAttribute(memberName:string, parameters:obj[]) =
    inherit Xunit.MemberDataAttributeBase(memberName, parameters)

    new(memberName:string) = MemberDataExAttribute(memberName, [||])

    override this.ConvertDataItem(testMethod:MethodInfo, item:obj) = 
        TupleHelper.namer item 
        |> List.map box
        |> Array.ofList

