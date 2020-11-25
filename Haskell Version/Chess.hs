import Chesshs

forcedCapture :: Color -> Board -> [((Int, Int), (Int, Int))]

forcedCapture clr brd = filter canCapture pairs
  where
    pieces                          = piecesOf clr brd
    otherPieces                     = piecesOf (otherColor clr) brd
    pairs                           = [(p1, p2) | p1 <− pieces, p2 <− otherPieces]
    canCapture ((x1, y1), (x2, y2)) = okMove x1 y1 x2 y2 brd
    
movesFrom :: Int -> Int -> Board -> [(Int, Int, Int, Int)]
movesFrom x y brd
  | isNothing piece          = []
  | clr ownpiece /= turn brd = []
  | otherwise                = movesFrom' ownpiece
    where
      piece = pieceAt x y brd
      ownpiece = fromJust piece
      owncolor = clr ownpiece

      mvFilter mvs
        | null right || not (opponentAt x2 y2) = left
        | otherwise                            = head right : []
          where
            (left, right) = span (\(x2, y2) -> 
              isNothing $ pieceAt x2 y2 brd) mvs
            (x2, y2) = head right
            
      opponentAt x2 y2 = isJust target && 
          clr (fromJust target) /= owncolor
        where
          target = pieceAt x2 y2 brd
          
      rookFrom xx yy =
        (mvFilter $ zip [xx−1, xx−2 .. 0] [yy, yy..]) ++
        (mvFilter $ zip [xx+1, xx+2 .. 7] [yy, yy..]) ++
        (mvFilter $ zip [xx, xx..] [yy−1, yy−2 .. 0]) ++
        (mvFilter $ zip [xx, xx..] [yy+1, yy+2 .. 7])
        
      bishopFrom xx yy =
       (mvFilter $ zip [xx−1, xx−2 .. 0] [yy−1, yy−2 .. 0]) ++
       (mvFilter $ zip [xx+1, xx+2 .. 7] [yy+1, yy+2 .. 7]) ++
       (mvFilter $ zip [xx+1, xx+2 .. 7] [yy−1, yy−2 .. 0]) ++
       (mvFilter $ zip [xx−1, xx−2 .. 0] [yy+1, yy+2 .. 7])

      inBounds x2 y2 = 0 <= x2 && x2 < 8 && 0 <= y2 && y2 < 8
      
      movesFrom' (Piece Rook) = rookFrom x y
      movesFrom' (Piece Bishop) = bishopFrom x y
      movesFrom' (Piece Queen) = rookFrom x y ++ bishopFrom x y
      movesFrom' (Piece Knight)
        | null caps = empty
        | otherwise = caps
          where
            locs = filter (uncurry inBounds) [
              (x+2, y+1), (x+1, y+2), (x−2, y+1), (x−1, y+2), 
              (x+2, y−1), (x+1, y−2), (x−2, y−1), (x−1, y−2)]
            caps = filter (\(x2, y2) -> opponentAt x2 y2) locs
            empty = filter (\(x2, y2) -> isNothing $ 
              pieceAt x2 y2 brd) locs
          
      movesFrom' (Piece King)
        | null caps = empty
        | otherwise = caps
          where
            locs = filter (uncurry inBounds) [
              (x+1, y+1), (x, y+1) , (x−1, y+1), (x+1, y ),
              (x−1, y), (x+1, y−1), (x, y−1), (x−1, y−1)]
            caps = filter (\(x2, y2) -> opponentAt x2 y2) locs
            empty = filter (\(x2, y2) -> isNothing $ 
              pieceAt x2 y2 brd) locs

      movesFrom' (Piece White Pawn)
        | not $ null captures = captures
        | y == 7 || isJust (pieceAt x (y+1) brd) = []
        | y == 1 && isNothing (pieceAT x (y+2) brd) = 
          [(x, y+1), (x, y+2)]
        | otherwise = [(x, y+1)]
          where
            enpassantCaps = filter (\(x2, y2) ->
              enpassant brd == Just (x2, y2)) $ 
              filter (uncurry inBounds) $ 
              [(x+1, y), (x−1, y)]
            captures = filter (\(x2, y2) -> opponentAt x2 y2) $
              (enpassantCaps ++ filter (uncurry inBounds)
              [(x+1, y+1), (x−1, y+1)])
              
      movesFrom' (Piece Black Pawn)
        | not $ null captures = captures
        | y == 0 || isJust (pieceAt x (y-1) brd) = []
        | y == 6 && isNothing (pieceAT x (y-2) brd) = 
          [(x, y-1), (x, y-2)]
        | otherwise = [(x, y-1)]
          where
            enpassantCaps = filter (\(x2, y2) ->
              enpassant brd == Just (x2, y2)) $ 
              filter (uncurry inBounds) $ 
              [(x-1, y), (x+1, y)]
            captures = filter (\(x2, y2) -> opponentAt x2 y2) $
              (enpassantCaps ++ filter (uncurry inBounds)
              [(x-1, y-1), (x+1, y-1)])
