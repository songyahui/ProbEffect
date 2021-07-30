-- ued to prepend the list, and recompute the probability.
increase :: Float -> Int -> [(Float, [Int])] -> [(Float, [Int])]
increase f n = map (\ (a, b) -> (f * a, n : b))


decreasingList :: Int -> [(Float, [Int])]
decreasingList prevnum
  | prevnum <= 1 = [(1.0, [1])]
  | prevnum == 2
  = let dice_1 = increase (1 / 6) 2 (decreasingList 1) in
    let rest   = increase (5 / 6) 2 (decreasingList 1) in dice_1 ++ rest
  | prevnum == 3
  = let dice_1 = increase (1 / 6) 3 (decreasingList 1) in
    let dice_2 = increase (1 / 6) 3 (decreasingList 2) in
    let rest   = increase (4 / 6) 3 (decreasingList 1)
    in dice_1 ++ dice_2 ++ rest
  | prevnum == 4
  = let dice_1 = increase (1 / 6) 4 (decreasingList 1) in
    let dice_2 = increase (1 / 6) 4 (decreasingList 2) in
    let dice_3 = increase (1 / 6) 4 (decreasingList 3) in
    let   rest = increase (3 / 6) 4 (decreasingList 1)
    in dice_1 ++ dice_2 ++ dice_3 ++ rest
  | prevnum == 5
  = let dice_1 = increase (1 / 6) 5 (decreasingList 1) in
    let dice_2 = increase (1 / 6) 5 (decreasingList 2) in
    let dice_3 = increase (1 / 6) 5 (decreasingList 3) in
    let dice_4 = increase (1 / 6) 5 (decreasingList 4) in
    let   rest = increase (2 / 6) 5 (decreasingList 1)
    in dice_1 ++ dice_2 ++ dice_3 ++ dice_4 ++ rest
  | otherwise  -- prevnum >= 6 
  = let dice_1 = increase (1 / 6) 6 (decreasingList 1) in
    let dice_2 = increase (1 / 6) 6 (decreasingList 2) in
    let dice_3 = increase (1 / 6) 6 (decreasingList 3) in
    let dice_4 = increase (1 / 6) 6 (decreasingList 4) in
    let dice_5 = increase (1 / 6) 6 (decreasingList 5) in
    let dice_6 = increase (2 / 6) 6 (decreasingList 1) 
    in dice_1 ++ dice_2 ++ dice_3 ++ dice_4 ++ dice_5 ++ dice_6 

compareList :: [Int] -> [Int] -> Bool
compareList a b =
  case (a, b) of 
  ([],[]) -> True 
  (x:xa, y:yb) -> x == y && compareList xa yb 
  _ -> False


findAndMerge :: (Float, [Int]) -> [(Float, [Int])] -> ([(Float, [Int])], Bool)
findAndMerge (p, li) prob = 
  case prob of 
  [] -> ([(p, li)], False)
  (p1, li1) : xs ->
    if compareList li li1 then ((p + p1, li) : xs, True)
    else 
      let (a, b) = findAndMerge (p, li) xs in 
      ((p1, li1) : a, b)


normalize :: [(Float, [Int])] -> [(Float, [Int])]
normalize li =
  case li of 
  [] -> []
  [x] -> [x]
  x : xs -> 
    let (a, b) = findAndMerge x xs in 
    if b then normalize a
    else x : normalize xs 


interface :: Int -> [(Float, [Int])]
interface n = normalize $ decreasingList n