namespace FulcrumChess.Engine
open System.IO
open System.Text.Json

type MagicNumbersFile = {UseFast32BitMultiplying:bool; MagicAndShifts:(uint64*int)[]}

module MagicCache =
    let resolveMagicNumbersWithPersistentCaching (filePath:string) (magicAndShiftsGenerator:unit->(uint64*int)[]) =
        if not <| File.Exists(filePath) then 
            printfn "Generating file %s with magic values" filePath
            let magicAndShifts = magicAndShiftsGenerator()
            let contentToCache = {
                MagicNumbersFile.MagicAndShifts=magicAndShifts;
                UseFast32BitMultiplying=EngineConstants.UseFast32BitMultiplyingForHashing }
            let writerOptions = new JsonWriterOptions(Indented=true)
            use fs = File.Create(filePath)
            use writer = new Utf8JsonWriter(fs, writerOptions)
            JsonSerializer.Serialize(writer, contentToCache)
            magicAndShifts
        else
            let json = File.ReadAllText(filePath)
            // let jsonReadOnlySpan = File.ReadAllBytes(filePath) |> ReadOnlySequence<byte> 
            // let reader = new Utf8JsonReader(jsonReadOnlySpan) 
            let content = JsonSerializer.Deserialize<MagicNumbersFile>(json)
            if content.UseFast32BitMultiplying <> EngineConstants.UseFast32BitMultiplyingForHashing then failwith "Error: Magic values file has been generated with a different multiplying algorithm used for hashing. Please delete the file and restart the program."
            if content.MagicAndShifts.Length <> 64 then failwith "Error: Magic values file was expected to contain exactly 64 items"
            content.MagicAndShifts