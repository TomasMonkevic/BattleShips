module TestData where
    
    import Moves

    correctData2 :: String
    correctData2 = "[\"coord\",[\"E\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]]"

    correctData3 :: String
    correctData3 = "[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"
        
    noBracketsData1 :: String
    noBracketsData1 = "[\"coord\",\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    noBracketsData2 :: String
    noBracketsData2 = "\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    noBracketsData3 :: String
    noBracketsData3 = "[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    noQuotesData1 :: String
    noQuotesData1 = "[\"coord\",[D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    noQuotesData2 :: String
    noQuotesData2 = "[\"coord\",[\"D\",\"2\"],\"result\",HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    noQuotesData3 :: String
    noQuotesData3 = "[coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"

    incorrectMovesData1 :: String
    incorrectMovesData1 = "[\"coord\",[\"D\",\"2\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"D\",\"2\"],\"result\",null,\"prev\",null]]]"

    incorrectKeyData1 :: String
    incorrectKeyData1 = "[\"rip\",[\"D\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",null,\"prev\",null]]"
