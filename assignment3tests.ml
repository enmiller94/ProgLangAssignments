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

let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin

let t6a = to_f (F 2.3) = 2.3

let t7a = temp_compare (F 2.3, F 4.5) = -1

let t8a = string_of_temp (C 2.3) = "2.3C"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1
