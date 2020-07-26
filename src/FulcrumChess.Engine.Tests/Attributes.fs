namespace FulcrumChess.Engine.Tests
open System
open System.Reflection

type BoardRefAttribute(fen:string, url:string) =
    inherit Attribute()
    member __.FEN = fen
    member __.Url = url
    new(fen:string) = BoardRefAttribute(fen,"")

type SlowAttribute() =
    inherit Attribute()

/// Works around F# compiler issue with default InlineData(data:obj array) constructor
// See also https://github.com/xunit/xunit/issues/1225
// See also https://github.com/xunit/xunit/blob/master/src/xunit.core/InlineDataAttribute.cs
[<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
type InlineDataExAttribute(data:obj array) =
    inherit Xunit.Sdk.DataAttribute()
    new(arg1:obj) = InlineDataExAttribute [|arg1|]
    new(arg1:obj, arg2:obj) = InlineDataExAttribute [|arg1; arg2|]
    new(arg1:obj, arg2:obj, arg3:obj) = InlineDataExAttribute [|arg1; arg2; arg3|]
    override this.GetData(testMethod:MethodInfo) = Seq.singleton data
