let t1a = result (Rock, Paper) = SndWin
let t1b = result (Rock, Scissors) = FstWin
let t1c = result (Paper, Scissors) = SndWin
let t1d = result (Paper, Rock) = FstWin
let t1e = result (Scissors, Paper) = FstWin
let t1f = result (Scissors, Rock) = SndWin
let t1g = result (Rock, Rock) = Tie
let t1h = result (Paper, Paper) = Tie
let t1i = result (Scissors, Scissors) = Tie



let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Rock, Scissors) = false
let t2c = is_tie (Paper, Scissors) = false
let t2d = is_tie (Paper, Rock) = false
let t2e = is_tie (Scissors, Paper) = false
let t2f = is_tie (Scissors, Rock) = false
let t2g = is_tie (Rock, Rock) = true
let t2h = is_tie (Paper, Paper) = true
let t2i = is_tie (Scissors, Scissors) = true



let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([Rock], [Scissors]) =
               [(Rock, Scissors)]
let t3c = game_from_plays ([Rock; Paper; Rock; Scissors], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]               
let t3d = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock; Scissors]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3e = game_from_plays ([Rock], [Scissors; Rock; Rock]) =
               [(Rock, Scissors)]
let t3f = game_from_plays ([Rock; Paper; Rock], [Scissors]) =
               [(Rock, Scissors)]
let t3g = game_from_plays ([Rock; Paper; Rock], []) =
               []
let t3h = game_from_plays ([], [Scissors; Rock; Rock]) =
               []                              



let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [(Scissors, Scissors); (Rock, Rock); (Paper, Paper); (Rock, Scissors)] = true
let t4c = valid_game [(Rock, Rock)] = false
let t4d = valid_game [(Rock, Scissors); (Rock, Rock)] = false
let t4e = valid_game [(Rock, Rock); (Paper, Paper); (Rock, Scissors); (Scissors, Scissors)] = false
let t4f = valid_game [] = true
let t4g = valid_game [(Rock, Scissors); (Rock, Paper)] = false



let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Rock, Rock); (Rock, Rock)] = Tie
let t5c = play_game [(Rock, Rock); (Paper, Rock)] = FstWin
let t5d = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5e = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5f = play_game [(Scissors, Scissors); (Rock, Rock); (Paper, Paper); (Rock, Scissors)] = FstWin
let t5g = play_game [(Rock, Rock)] = Tie
let t5h = play_game [(Rock, Scissors); (Rock, Rock)] = Tie
let t5i = play_game [(Rock, Rock); (Paper, Paper); (Rock, Scissors); (Paper, Scissors)] = SndWin
let t5j = play_game [(Rock, Scissors); (Rock, Paper)] = FstWin



let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (F 32.0) = 32.0
let t6b1 = to_f (F (-20.0)) = -20.0
let t6c = to_f (F 212.0) = 212.0
let t6d = to_f (C 0.0) = 32.0
let t6e = to_f (C 100.0) = 212.0
let t6f = to_f (C 54.0) = 129.2
let t6g = to_f (C (-54.0)) = -65.2
let t6h = to_f (C (-10.0)) = 14.00



let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (F 5.0, F 4.5) = 1
let t7c = temp_compare (F 2.3, F 2.3) = 0
let t7d = temp_compare (C 2.3, C 4.5) = -1
let t7e = temp_compare (C 5.0, C 4.5) = 1
let t7f = temp_compare (C 2.3, C 2.3) = 0
let t7g = temp_compare (C 100.0, F 212.0) = 0
let t7h = temp_compare (F 32.0, C (-100.0)) = 1
let t7i = temp_compare (C (-100.0), F 32.0) = -1



let t8a = string_of_temp (C 2.3) = "2.3C"
let t8b = string_of_temp (C (-2.3)) = "-2.3C"
let t8c = string_of_temp (F 2.3) = "2.3F"
let t8d = string_of_temp (F (-2.3)) = "-2.3F"



let t9a = max_temp [F 2.1; C 2.1] = C 2.1
let t9b = max_temp [F 32.0; C 0.0] = C 0.0
let t9c = max_temp [C 2.1] = C 2.1
let t9d = max_temp [F 2.1] = F 2.1
let t9e = max_temp [F (-20.0); F 0.0; F (-100.0); F 20.0; F 35.0; F 19.0] = F 35.0
let t9f = max_temp [C 0.0; C 2.1; C 32.0; C (-100.0)] = C 32.0
let t9g = max_temp [F 2.1; C 2.1] = C 2.1
let t9h  = try (max_temp []; false)  with
            | Failure "max_temp" -> true
            | _ -> false



let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
let t10b = max_temp2 [F 32.0; C 0.0] = C 0.0
let t10c = max_temp2 [C 2.1] = C 2.1
let t10d = max_temp2 [F 2.1] = F 2.1
let t10e = max_temp2 [F (-20.0); F 0.0; F (-100.0); F 20.0; F 35.0; F 19.0] = F 35.0
let t10f = max_temp2 [C 0.0; C 2.1; C 32.0; C (-100.0)] = C 32.0
let t10g = max_temp2 [F 2.1; C 2.1] = C 2.1
let t10h= try (max_temp2 []; false)  with
            | Failure "max_temp2" -> true
            | _ -> false