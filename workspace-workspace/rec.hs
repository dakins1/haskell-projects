generalRec :: (a -> c -> Maybe b) -> (a -> c -> [(a,c)]) -> (([b],c) -> b) -> a -> b
generalRec base decomp comp a =
  case base a of
    Just b -> b
    Nothing -> let (subs, rest) = decomp a
                   recs = map (generalRec base decomp comp) subs
               in comp (recs, rest)
