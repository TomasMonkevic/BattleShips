module Score where

    import Utils

    data ShotType = MISS | HIT
        deriving (Show, Eq)

    data Moves = Moves { 
        coords :: (String, String), 
        result :: Maybe ShotType, 
        prev :: Maybe Moves 
    }
        deriving Show

    allMoves :: Maybe Moves -> ([(String, String)], [(String, String)])
    allMoves moves = f moves (moveCount moves) ([],[])
        where 
            f :: Maybe Moves -> Integer -> ([(String, String)], [(String, String)]) -> ([(String, String)], [(String, String)])
            f Nothing _ acc = acc
            f (Just Moves { coords = c, prev = p}) player (playerOneMoves, playerTwoScore) = 
                if player `mod` 2 == 0 then
                        f p (player + 1) (playerOneMoves, c : playerTwoScore)
                    else
                        f p (player + 1) (c : playerOneMoves, playerTwoScore)

    moveCount :: Maybe Moves -> Integer
    moveCount moves = f moves 0
            where 
                f :: Maybe Moves -> Integer -> Integer
                f Nothing acc = acc
                f (Just Moves { prev = p})  acc = f p (acc + 1)
                        
    scoreInternal :: Maybe Moves -> (Integer, Integer)
    scoreInternal moves = f moves (moveCount moves) (0,0)
        where 
            f :: Maybe Moves -> Integer -> (Integer, Integer) -> (Integer, Integer)
            f Nothing _ acc = acc
            f (Just Moves { result = r, prev = p}) player (playerOneScore, playerTwoScore) = 
                if r == Just HIT then
                    if player `mod` 2 == 0 then --when ship was hit, second players seys HIT, the score is increase for the first player and vice versa
                        f p (player + 1) (playerOneScore + 1, playerTwoScore)
                    else
                        f p (player + 1) (playerOneScore, playerTwoScore + 1)
                else
                    f p (player + 1) (playerOneScore, playerTwoScore)

    areMovesValid :: Maybe Moves -> Bool
    areMovesValid moves = 
        let
            m = allMoves moves
        in
            (allUnique (fst m)) && (allUnique (snd m))
            
    score :: String -> Either String (Integer, Integer)
    score msg = 
        let 
            moves = parseMessage msg
        in
            case moves of
                Left error -> Left error
                Right m -> 
                    if areMovesValid m then
                        Right (scoreInternal m)
                    else
                        Left "Error: A cell has one or more moves in it!"


    
    getElement :: String -> Either String String
    getElement (h:t) =
        if h /= '\"' then
            Left "Error: Element not starting with quotes!"
        else
            Right (takeWhile (/='\"') t)

    getElements :: String -> Either String [String]
    getElements (h:t) =
        if h /= '[' then
            Left "Error: Array not starting with square brackets!"
        else
            case (f t []) of
                Left error -> Left error
                Right elems -> Right (reverse elems)
            where
                f :: String -> [String] -> Either String [String]
                f [] acc = Right acc
                f elements acc = 
                    let
                        el = getElement elements
                    in
                        case getElement elements of
                            Left error -> Left error
                            Right el -> f  (drop ((length el) + 3) elements) (el : acc)

    parseMessage :: String -> Either String (Maybe Moves)
    parseMessage message = f message
        where
            f :: String -> Either String (Maybe Moves)
            f [] = Right Nothing
            f msg = 
                let
                    cKey = if (head msg) == '[' then
                        getElement (tail msg)
                    else 
                        Left "Error: Array not starting with square brackets!"
                    msg2 = takeWhile (/=']') (tail (dropWhile (/=',') msg))
                    cElem = getElements msg2
                    rez = case cElem of
                        Left error -> Left error
                        Right elems -> 
                            let
                                c = (head elems, last elems)
                                msg3 = tail (dropWhile (/=',') msg)

                                --result key
                                msg4 = tail (dropWhile (/=',') msg3)
                                msg5 = tail (dropWhile (/=',') msg4)
                                msg6 = tail (dropWhile (/=',') msg5)
                                r = if (take 4 msg8) == "null" then
                                        Right Nothing
                                    else
                                        case getElement msg6 of
                                            Left error -> Left error
                                            Right res -> if res == "HIT" then Right (Just HIT) else Right (Just MISS) 

                                msg7 = tail (dropWhile (/=',') msg6)

                                --p key
                                msg8 = tail (dropWhile (/=',') msg7)
                                p = if (take 4 msg8) == "null" then Right Nothing else f msg8
                            in
                                case p of
                                    Left error -> Left error
                                    Right ppp ->  case r of
                                                Left error -> Left error
                                                Right rrr -> Right (Just Moves {
                                                coords = c,
                                                result = rrr,
                                                prev = ppp
                                            })
                in
                    case cKey of
                        Left error -> Left error
                        Right ck -> if ck == "coord" then 
                                case rez of
                                    Left error -> Left error
                                    Right rrrz -> Right rrrz
                                else
                                    Left "Error: No coord key found!"