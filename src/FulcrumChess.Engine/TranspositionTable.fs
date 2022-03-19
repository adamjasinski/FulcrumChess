namespace FulcrumChess.Engine

open System
open Microsoft.Extensions.Caching.Memory

type TranspositionTableEntry = { Score:int; Depth:int; BestMove:Move}

type TranspositionTable(sizeLimitMb) =
    let sizeLimit = sizeLimitMb * (1024*1024) / sizeof<TranspositionTableEntry>
    let cache = new MemoryCache(MemoryCacheOptions(SizeLimit = sizeLimit))
    let entryOptions = MemoryCacheEntryOptions().SetSlidingExpiration(TimeSpan.FromSeconds(60))

    member __.Get(hash:uint64) : TranspositionTableEntry option = 
        let (found, result) = cache.TryGetValue(hash)
        if found then 
            downcast cache.Get(hash) |> Some
        else
            None

    member __.Set(hash:uint64, entry:TranspositionTableEntry) = cache.Set(hash, entry, entryOptions)

    interface IDisposable with
        member __.Dispose() = cache.Dispose()