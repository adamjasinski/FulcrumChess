namespace FulcrumChess.Engine

module Eval =
    module Weights =
        [<Literal>] 
        let King = 200.
        [<Literal>] 
        let Queen = 9.
        [<Literal>] 
        let Rook = 5.
        [<Literal>] 
        let Bishop = 3.
        [<Literal>] 
        let Knight = 3.
        [<Literal>] 
        let Pawn = 1.
        [<Literal>]
        let Mobility = 0.2

    let inline calculateSimpleMaterialDiff weight bbW bbB=
        let white = (BitUtils.countSetBits bbW |> float)
        let black = (BitUtils.countSetBits bbB |> float)
        weight * (white - black)
        
    let naiveEval (generateLegalMoves:GetMovesForSide)(pos:Position) =
        let sideToPlay = pos.SideToPlay

        let getLegalMovesCount (side:Side) =
            let pos' = 
                if side = pos.SideToPlay then pos 
                else { pos with SideToPlay=opposite pos.SideToPlay}

            generateLegalMoves pos' |> Array.length

        let legalMovesCountSideToPlay = sideToPlay |> getLegalMovesCount

        let isCheckMateForSideToPlay = legalMovesCountSideToPlay = 0

        if isCheckMateForSideToPlay then
            match sideToPlay with
            | White -> -Weights.King
            | Black -> Weights.King
        else
            let materialDiff = 
                (calculateSimpleMaterialDiff Weights.Queen pos.WhiteQueen pos.BlackQueen) +
                (calculateSimpleMaterialDiff Weights.Rook pos.WhiteRooks pos.BlackRooks) +
                (calculateSimpleMaterialDiff Weights.Bishop pos.WhiteBishops pos.BlackBishops) +
                (calculateSimpleMaterialDiff Weights.Knight pos.WhiteKnights pos.BlackKnights) +
                (calculateSimpleMaterialDiff Weights.Pawn pos.WhitePawns pos.BlackPawns)

            // let material = 
            //     match sideToPlay with
            //     | White ->  (pos.WhiteQueen |> calculateSimpleMaterialScore Weights.Queen) +
            //                 (pos.WhiteRooks |> calculateSimpleMaterialScore Weights.Rook) +
            //                 (pos.WhiteBishops |> calculateSimpleMaterialScore Weights.Bishop) +
            //                 (pos.WhiteKnights |> calculateSimpleMaterialScore Weights.Knight) +
            //                 (pos.WhitePawns |> calculateSimpleMaterialScore Weights.Pawn)
            //     | Black ->  (pos.BlackQueen |> calculateSimpleMaterialScore Weights.Queen) +
            //                 (pos.BlackRooks |> calculateSimpleMaterialScore Weights.Rook) +
            //                 (pos.BlackBishops |> calculateSimpleMaterialScore Weights.Bishop) +
            //                 (pos.BlackKnights |> calculateSimpleMaterialScore Weights.Knight) +
            //                 (pos.BlackPawns |> calculateSimpleMaterialScore Weights.Pawn)

            let legalMovesCountOppositeSide = opposite sideToPlay |> getLegalMovesCount
            let mobilityDiff = float(legalMovesCountSideToPlay - legalMovesCountOppositeSide) * Weights.Mobility
            let sideToPlayScore = if sideToPlay = White then 1. else -1.
            sideToPlayScore * (materialDiff + mobilityDiff)
        