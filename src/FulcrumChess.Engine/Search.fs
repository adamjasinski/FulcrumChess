namespace FulcrumChess.Engine
open System

module Search =
    let private sortNextBestMove (moves:ScoredMove[]) curIndex : unit =
        // Finds the new best move and swaps it with the curIndex element, containing the previous best move 
        if moves.Length = 0 then invalidArg (nameof moves) "cannot be empty"
        // Assumption: the curIndex element is the current best move
        let mutable maxScore = 0
        let mutable maxIndex = 0
        for i=curIndex to moves.Length-1 do
            let curScore = moves.[i] |> MoveScore.getScore
            if curScore > maxScore then
                maxScore <- curScore
                maxIndex <- i
        //SpanT.swap curIndex maxIndex moves
        let curMove = moves.[curIndex]
        moves.[curIndex] <- moves.[maxIndex]
        moves.[maxIndex] <- curMove

    // let private toScoredMovesOnStack (moves:Move[]) =
    //     let mapped = moves |> Array.map MoveScore.applyScore
    //     mapped.AsSpan()
    //     // let movesOnStack = SpanT.createOnStack moves.Length
    //     // for i=0 to moves.Length-1
    //     //     movestOnStack.[i] <- moves.[i] |> MoveScore.applyScore
    //     // movesOnStack

    // let negaMaxWithTT (tt:TranspositionTable)(generateLegalMoves:GetMovesForSide) (tryMakeMoveInternal:Position->Move->Position option) (depth:int) (pos:Position) alpha beta =
    //     let ttEntryOpt = tt.Get(pos.HashKey)
    //     match ttEntryOpt with
    //     | Some tt -> tt.Score //TODO - probably not as simple as one may think
    //     | None ->
    //         let (score, bestMove) = pos |> negaMax generateAllPseudoMovesForSide tryMakeMoveInternal depth alpha beta
    //         { Score = score; Depth = depth; BestMove = bestMove } 

    let rec quiescenceSearch (pos:Position) (generateLegalMoves:GetMovesForSide) (tryMakeMoveInternal:Position->Move->Position option) (alpha:float) (beta:float)  =
        //NB: score is the stand_pat
        let score =  pos |> Eval.naiveEval generateLegalMoves
        if score >= beta then
            beta
        else
            let qMoves = 
                pos 
                |> generateLegalMoves 
                |> Array.filter Move.isCapture 
                |> Array.map MoveScore.applyScore

            let rec checkQMoves i alpha beta =
                if i >= qMoves.Length then
                    alpha
                else
                    sortNextBestMove qMoves i
                    let move = qMoves.[i].Move
                    let pos' = move |> tryMakeMoveInternal pos |> Option.get 
                    let score' = -1. * (quiescenceSearch pos' generateLegalMoves tryMakeMoveInternal -beta -alpha)

                    if score' > alpha && score' >= beta then
                        beta
                    else
                        let alpha' = Math.Max(alpha, score')
                        checkQMoves (i+1) alpha' beta

            let alpha' = Math.Max(alpha, score)
            checkQMoves 0 alpha' beta
            
        
    let rec negaMax (pos:Position) (generateLegalMoves:GetMovesForSide) (tryMakeMoveInternal:Position->Move->Position option) (depth:int) (alpha:float) (beta:float)  =
        if depth <= 0 then
            let score = quiescenceSearch pos generateLegalMoves tryMakeMoveInternal alpha beta
            score
        elif alpha >= beta then
            alpha
        else
            let legalMoves = pos |> generateLegalMoves |> Array.map MoveScore.applyScore

            let rec checkLegalMoves i alpha beta =
                if i >= legalMoves.Length then
                    alpha
                else
                    sortNextBestMove legalMoves i
                    let move = legalMoves.[i].Move
                    let pos' = move |> tryMakeMoveInternal pos |> Option.get 
                    let score' = -1. * (negaMax pos' generateLegalMoves tryMakeMoveInternal (depth - 1) -beta -alpha)

                    if score' > alpha && score' >= beta then
                        //TODO - update TT if score' > alpha
                        //TODO - add killer moves, etc. if score' >= beta
                        beta
                    else
                        let alpha' = Math.Max(alpha, score')
                        checkLegalMoves (i+1) alpha' beta

            let alpha' = checkLegalMoves 0 alpha beta
            alpha'

    let rec negaMaxRoot (pos:Position) (generateLegalMoves:GetMovesForSide) (tryMakeMoveInternal:Position->Move->Position option) (depth:int) (alpha:float) (beta:float)  =
        let legalMoves = pos |> generateLegalMoves |> Array.map MoveScore.applyScore

        let rec checkLegalMoves i alpha beta bestMove : (float*Move) =
            if i >= legalMoves.Length then
                (alpha, bestMove)
            else
                sortNextBestMove legalMoves i
                let move = legalMoves.[i].Move
                let pos' = move |> tryMakeMoveInternal pos |> Option.get 
                let score' = -1. * (negaMax pos' generateLegalMoves tryMakeMoveInternal (depth - 1) -beta -alpha)

                if score' > alpha && score' >= beta then
                    //TODO - update TT if score' > alpha
                    //TODO - add killer moves, etc. if score' >= beta
                    (beta, move)
                else
                    let alpha' = Math.Max(alpha, score')
                    let bestMove' = if score' > alpha then move else bestMove
                    checkLegalMoves (i+1) alpha' beta bestMove'

        let (alpha',bestMove') = checkLegalMoves 0 alpha beta Move.nullMove
        (alpha',bestMove')


    let goSearch (pos:Position) (generateLegalMoves:GetMovesForSide) (tryMakeMoveInternal:Position->Move->Position option) (maxDepth:int) =
        //TODO - set search settings
        let alpha = Int32.MinValue
        let beta = Int32.MaxValue
        let (score, bestMove) = negaMaxRoot pos generateLegalMoves tryMakeMoveInternal maxDepth alpha beta
        (score, bestMove)

            // // TODO - if no legal moves, it's checkmate
            // let mutable value = 0.
            // let mutable candidateMove = MoveScore.nullMove
            // for i = 0 to legalMoves.Length-1 do
            //     sortNextBestMove legalMoves i
            //     candidateMove <- legalMoves.[i]
                
            //     let pos' = tryMakeMoveInternal pos candidateMove.Move |> Option.get //NB - it should be guaranteed that the move is legal
            //     value <- -1. * fst(negaMax pos' generateLegalMoves tryMakeMoveInternal (depth - 1) -beta -alpha)
            // (value, candidateMove)