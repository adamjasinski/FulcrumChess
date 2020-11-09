namespace FulcrumChess.Engine
//Magic number generation.
//Magic numbers are used to hash equivalent occupancy values/attack set
//See also https://chessprogramming.wikispaces.com/Magic+Bitboards; https://chessprogramming.wikispaces.com/Looking+for+Magics


type MagicValues ={
    MagicNumbersAndShiftsRook:(uint64*int)[];
    MagicNumbersAndShiftsBishop:(uint64*int)[];
}

type MoveGenerationLookups = {
    MagicNumbersAndShifts:MagicValues;
    RookMovesDb:uint64[][];
    BishopMovesDb:uint64[][];
    KingMovesDb:uint64[];
    KnightMovesDb:uint64[];
    WhitePawnMovesDb:(uint64*uint64)[];
    BlackPawnMovesDb:(uint64*uint64)[];
}

module PregeneratedMagic =
    let getMagicValuesAndShiftsFor (pc:SlidingPiece) (magicValues:MagicValues) =
        match pc with
        | Rook -> magicValues.MagicNumbersAndShiftsRook
        | Bishop -> magicValues.MagicNumbersAndShiftsBishop

    let MagicFor64BitHashing =
        //Magic numbers taken from http://www.afewmorelines.com/understanding-magic-bitboards-in-chess-programming/ (also at http://www.rivalchess.com/magic-bitboards/)
        let magicNumberRook = [|
                0xa180022080400230UL; 0x40100040022000UL; 0x80088020001002UL; 0x80080280841000UL; 0x4200042010460008UL; 0x4800a0003040080UL; 0x400110082041008UL; 0x8000a041000880UL; 0x10138001a080c010UL; 0x804008200480UL; 0x10011012000c0UL; 0x22004128102200UL; 0x200081201200cUL; 0x202a001048460004UL; 0x81000100420004UL; 0x4000800380004500UL; 0x208002904001UL; 0x90004040026008UL; 0x208808010002001UL; 0x2002020020704940UL; 0x8048010008110005UL; 0x6820808004002200UL; 0xa80040008023011UL; 0xb1460000811044UL; 0x4204400080008ea0UL; 0xb002400180200184UL; 0x2020200080100380UL; 0x10080080100080UL; 0x2204080080800400UL; 0xa40080360080UL; 0x2040604002810b1UL; 0x8c218600004104UL; 0x8180004000402000UL; 0x488c402000401001UL; 0x4018a00080801004UL; 0x1230002105001008UL; 0x8904800800800400UL; 0x42000c42003810UL; 0x8408110400b012UL; 0x18086182000401UL; 0x2240088020c28000UL; 0x1001201040c004UL; 0xa02008010420020UL; 0x10003009010060UL; 0x4008008008014UL; 0x80020004008080UL; 0x282020001008080UL; 0x50000181204a0004UL; 0x102042111804200UL; 0x40002010004001c0UL; 0x19220045508200UL; 0x20030010060a900UL; 0x8018028040080UL; 0x88240002008080UL; 0x10301802830400UL; 0x332a4081140200UL; 0x8080010a601241UL; 0x1008010400021UL; 0x4082001007241UL; 0x211009001200509UL; 0x8015001002441801UL; 0x801000804000603UL; 0xc0900220024a401UL; 0x1000200608243UL
            |]

        let magicNumberBishop = [|
                0x2910054208004104UL; 0x2100630a7020180UL; 0x5822022042000000UL; 0x2ca804a100200020UL; 0x204042200000900UL; 0x2002121024000002UL; 0x80404104202000e8UL; 0x812a020205010840UL; 0x8005181184080048UL; 0x1001c20208010101UL; 0x1001080204002100UL; 0x1810080489021800UL; 0x62040420010a00UL; 0x5028043004300020UL; 0xc0080a4402605002UL; 0x8a00a0104220200UL; 0x940000410821212UL; 0x1808024a280210UL; 0x40c0422080a0598UL; 0x4228020082004050UL; 0x200800400e00100UL; 0x20b001230021040UL; 0x90a0201900c00UL; 0x4940120a0a0108UL; 0x20208050a42180UL; 0x1004804b280200UL; 0x2048020024040010UL; 0x102c04004010200UL; 0x20408204c002010UL; 0x2411100020080c1UL; 0x102a008084042100UL; 0x941030000a09846UL; 0x244100800400200UL; 0x4000901010080696UL; 0x280404180020UL; 0x800042008240100UL; 0x220008400088020UL; 0x4020182000904c9UL; 0x23010400020600UL; 0x41040020110302UL; 0x412101004020818UL; 0x8022080a09404208UL; 0x1401210240484800UL; 0x22244208010080UL; 0x1105040104000210UL; 0x2040088800c40081UL; 0x8184810252000400UL; 0x4004610041002200UL; 0x40201a444400810UL; 0x4611010802020008UL; 0x80000b0401040402UL; 0x20004821880a00UL; 0x8200002022440100UL; 0x9431801010068UL; 0x1040c20806108040UL; 0x804901403022a40UL; 0x2400202602104000UL; 0x208520209440204UL; 0x40c000022013020UL; 0x2000104000420600UL; 0x400000260142410UL; 0x800633408100500UL; 0x2404080a1410UL; 0x138200122002900UL    
            |]

        let magicNumberShiftsRook = [|
                52; 53; 53; 53; 53; 53; 53; 52; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 52; 53; 53; 53; 53; 53; 53; 52
            |]

        let magicNumberShiftsBishop = [|
                58; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 59; 59; 
                59; 59; 57; 57; 57; 57; 59; 59; 59; 59; 57; 55; 55; 57; 59; 59; 
                59; 59; 57; 55; 55; 57; 59; 59; 59; 59; 57; 57; 57; 57; 59; 59; 
                59; 59; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 58
            |]

        let magicNumbersAndShiftsRook = Array.zip magicNumberRook magicNumberShiftsRook
        let magicNumbersAndShiftsBishop = Array.zip magicNumberBishop magicNumberShiftsBishop

        {MagicValues.MagicNumbersAndShiftsRook=magicNumbersAndShiftsRook;MagicNumbersAndShiftsBishop=magicNumbersAndShiftsBishop}

    let PartialMagicFor32BitHashing = 
        // Contains some more computationally expensive magic values for corner/edge squares
        let magicNumberRook:uint64[]  = 
            let a = Array.zeroCreate<uint64> 64
            a.[0] <- 0x2051022021001080UL
            a.[1] <- 0x50204000100804UL
            a.[2] <- 0x1201000008018a0UL
            a.[3] <- 0x10010020100402UL
            a.[4] <- 0x900052202040002UL
            a.[5] <- 0x208210041000204UL
            a.[6] <- 0x1008080810002UL
            a.[7] <- 0x209000802041UL
            a.[8] <- 0x802a400000001008UL
            a.[9] <- 0x120112001004040UL
            a.[10] <- 0x1214088000108020UL
            a.[11] <- 0x8880017008010UL
            a.[12] <- 0x200481040004014UL
            a.[13] <- 0x8022c0440004041UL
            a.[14] <- 0x4080010280008202UL
            a.[15] <- 0x4080419000808041UL
            a.[16] <- 0xa9700e881002080UL
            a.[17] <- 0x4000520000200208UL
            a.[18] <- 0x28042102100020UL
            a.[19] <- 0x1001112000080101UL
            a.[20] <- 0x6889400421200808UL
            a.[21] <- 0x2210022040010404UL
            a.[22] <- 0x4402c00444840101UL
            a.[23] <- 0x805800100804201UL
            a.[24] <- 0x88084800010021UL
            a.[25] <- 0x40810800023022UL
            a.[26] <- 0x4195200040224101UL
            a.[27] <- 0x4021010800401003UL
            a.[28] <- 0x1000801000025d1UL
            a.[29] <- 0x808241000040202UL
            a.[30] <- 0x12044100802c0201UL
            a.[31] <- 0x2020a10601201152UL
            a.[32] <- 0x40004800820020UL
            a.[33] <- 0x3040004200202051UL
            a.[34] <- 0x4020240008050810UL
            a.[35] <- 0x100200a00011010UL
            a.[36] <- 0xa000804804d1222UL
            a.[37] <- 0x104040001010002UL
            a.[38] <- 0x2100440140810002UL
            a.[39] <- 0x4100128204000063UL
            a.[40] <- 0x800100090204880UL
            a.[41] <- 0x102004020008422UL
            a.[42] <- 0x20101000002a0024UL
            a.[43] <- 0x800808224080810UL
            a.[44] <- 0x200202020188124UL
            a.[45] <- 0x4a02020044090a14UL
            a.[46] <- 0x1000400090002UL
            a.[47] <- 0xc100800184800dc3UL
            a.[48] <- 0x40084080010038a0UL
            a.[49] <- 0x40c08100104003a1UL
            a.[50] <- 0x100040480881e0UL
            a.[51] <- 0x8028000008090UL
            a.[52] <- 0xa2002000040108UL
            a.[53] <- 0x4004020805000806UL
            a.[54] <- 0x4002010040260005UL
            a.[55] <- 0x80088000500849UL
            a.[56] <- 0x25a01680010142UL
            a.[57] <- 0x31008100001040UL
            a.[58] <- 0x80a2430118800a30UL
            a.[59] <- 0x28041105000990UL
            a.[60] <- 0x3845a01a0100030aUL
            a.[61] <- 0x8022030910850044UL
            a.[62] <- 0x100008102000502UL
            a.[63] <- 0x60408944200863UL
            a

        let magicNumberBishop = Array.zeroCreate<uint64> 64

        let magicNumberShiftsRook = [|
                52; 53; 53; 53; 53; 53; 53; 52; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 53; 54; 54; 54; 54; 54; 54; 53; 
                53; 54; 54; 54; 54; 54; 54; 53; 52; 53; 53; 53; 53; 53; 53; 52
            |]

        let magicNumberShiftsBishop = [|
                58; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 59; 59; 
                59; 59; 57; 57; 57; 57; 59; 59; 59; 59; 57; 55; 55; 57; 59; 59; 
                59; 59; 57; 55; 55; 57; 59; 59; 59; 59; 57; 57; 57; 57; 59; 59; 
                59; 59; 59; 59; 59; 59; 59; 59; 58; 59; 59; 59; 59; 59; 59; 58
            |]

        let magicNumbersAndShiftsRook = Array.zip magicNumberRook magicNumberShiftsRook
        let magicNumbersAndShiftsBishop = Array.zip magicNumberBishop magicNumberShiftsBishop

        {MagicValues.MagicNumbersAndShiftsRook=magicNumbersAndShiftsRook;MagicNumbersAndShiftsBishop=magicNumbersAndShiftsBishop}