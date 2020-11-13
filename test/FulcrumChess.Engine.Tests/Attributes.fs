namespace FulcrumChess.Engine.Tests
open System
open System.Reflection
open Xunit
open Xunit.Abstractions
open Xunit.Sdk

[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false)>]
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
    inherit DataAttribute()
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
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = false, Inherited = true)>]
type MemberDataExAttribute(memberName:string, parameters:obj[]) =
    inherit MemberDataAttributeBase(memberName, parameters)

    new(memberName:string) = MemberDataExAttribute(memberName, [||])

    override this.ConvertDataItem(testMethod:MethodInfo, item:obj) = 
        TupleHelper.namer item 
        |> List.map box
        |> Array.ofList


[<TraitDiscoverer("FulcrumChess.Engine.Tests.CategoryDiscoverer", "FulcrumChess.Engine.Tests")>]
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
//Apply this attribute to your test method to specify a category.
//Adapted from https://github.com/xunit/samples.xunit/blob/main/TraitExtensibility/CategoryAttribute.cs
type CategoryAttribute(category:string) =
    inherit Attribute()
    interface ITraitAttribute


type CategoryDiscoverer() =
    interface ITraitDiscoverer with
        //Gets the trait values from the Category attribute.
        member __.GetTraits(traitAttribute:IAttributeInfo) =
            let singleCtorArg = traitAttribute.GetConstructorArguments() |> Seq.exactlyOne
            let singleCategory = System.Collections.Generic.KeyValuePair<string,string>("Category", downcast singleCtorArg)
            Seq.singleton singleCategory