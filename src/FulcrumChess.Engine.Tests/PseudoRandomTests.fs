namespace FulcrumChess.Engine.Tests
open FulcrumChess.Engine
open Xunit
open Swensen.Unquote
open RandomExtensions

module PseudoRandomTests =  //just 'RandomTests' would sound silly ;)
    open System

    //let countRandomElementsFulfillingPredicate predicate totalCount =

    [<Fact>]
    let ``NextUInt64 returns pseudo random unsigned 64-bit integers`` () =
        let rnd = Random()
        let seqLength = 1000
        let randomSequence = seq { for i in 0..seqLength -> rnd.NextUInt64() }
        let midPoint = System.UInt64.MaxValue / 2UL
        let lessThanMidPoint = randomSequence |> Seq.where( (>)midPoint ) |> Seq.length
        let allowedRandomnessAsymmetry = 0.1
        let expectedHi = int(((float)seqLength / 2.0) * (1.0+allowedRandomnessAsymmetry))
        let expectedLo = int(((float)seqLength / 2.0) * (1.0-allowedRandomnessAsymmetry))

        test <@ lessThanMidPoint <= expectedHi @>
        test <@ lessThanMidPoint >= expectedLo @> 

    [<Fact>]
    let ``NextUInt64 return values have on average half of bits set`` () =
        let rnd = Random()
        let seqLength = 1000
        let randomSequence = [| for i in 0..seqLength -> rnd.NextUInt64() |]
        //note: exactly 32 bits is excluded from calculations here
        let lessThan32bits = randomSequence |> Seq.where( fun i -> BitUtils.countSetBits i < 32 ) |> Seq.length
        let moreThan32bits = randomSequence |> Seq.where( fun i -> BitUtils.countSetBits i > 32 ) |> Seq.length
        let allowedRandomnessAsymmetry = 0.1
        let expectedHi = int(((float)seqLength / 2.0) * (1.0+allowedRandomnessAsymmetry))
        let expectedLo = int(((float)seqLength / 2.0) * (1.0-allowedRandomnessAsymmetry))
        //LanguagePrimitives.IntrinsicOperators
        test <@ (moreThan32bits - lessThan32bits) <= int(allowedRandomnessAsymmetry*(float)seqLength) @>

       