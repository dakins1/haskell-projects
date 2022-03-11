findWinner :: GameState -> Outcome
findWinner state@(board,plr) = 
  case checkWinner state of
      Just outcome -> outcome
      Nothing -> let moves = validMoves state 
                     nexts = dropNothings $ map (\x -> makeMove x state) moves
                     outcomes = map findWinner nexts
                 in bestOutcomeFor plr outcomes

bestOutcomeFor :: Player -> [Outcome] -> Outcome
bestOutcomeFor plr [] = error "bestFor called on empty list of outcomes."
bestOutcomeFor plr outs | (Won plr) `elem` outs = Won plr
                        | Draw `elem` outs = Draw
                        | otherwise = Won (opposite plr)

bestMove :: GameState -> Maybe Move
bestMove state@(board,plr) = 
  case checkWinner state of
      Just outcome -> Nothing
      Nothing -> let moves = validMoves state 
                     nexts = dropNothingsFst $ [(makeMove mv state, mv) | mv <- moves]
                     outcomes = [(findWinner newState, mv) | (newState, mv)  <- nexts]
                 in bestMoveFor plr outcomes


dropNothingsFst :: [(Maybe a, b)] -> [(a,b)]
dropNothingsFst ((Nothing,_):xs) = dropNothingsFst xs
dropNothingsFst ((Just a, b):xs) = (a,b):(dropNothingsFst xs)

bestMoveFor :: Player -> [(Outcome,Move)] -> Move
bestMoveFor plr [] = error "bestMoveFor called on empty list of outcomes."
bestMoveFor plr outs = 
  let winPlr = lookup (Won plr) outs
      draw = lookup Draw outs
  in case (winPlr, draw) of
        (Just winMv, _) -> winMv 
        (Nothing, Just drawMv) -> drawMv
        (Nothing, Nothing) = snd $ head outs

type Rating = Int

findRating :: GameState -> Rating --not the best name, but by analogy to findWinner...
findRating 0 state = rateGame state
findRating n state@(board,plr) = 
  case checkWinner state of
      Just outcome -> rateOutcome outcome
      Nothing -> let moves = validMoves state 
                     nexts = dropNothings $ map (\x -> makeMove x state) moves
                     outcomes = map (findRating (n-1)) nexts
                 in bestRatingFor plr outcomes

rateOutcome :: Outcome -> Rating
rateOutcome (Won Player1) = 100000
rateOutcome (Won Player2) = -100000
rateOutcome (Draw) = 0

bestRatingFor :: Player -> [Rating] -> Rating
bestRatingFor Player1 = max 
bestRatingFor Player2 = min 

--as an open problem, goodMove should still look very similar to bestMove and findRating.
