let t1a = result (Rock, Paper) = SndWin
let t1b = result (Scissors, Paper) = FstWin
let t1c = result (Paper, Rock) = FstWin
let t1d = result (Rock, Scissors) = FstWin
let t1e = result (Paper, Scissors) = SndWin
let t1f = result (Scissors, Rock) = SndWin
let t1g = result (Paper, Paper) = Tie
let t1h = result (Rock, Rock) = Tie
let t1i = result (Scissors, Scissors) = Tie

let t2a = is_tie (Rock, Paper) = false
let t2a = is_tie (Rock, Paper) = false
let t2b = is_tie (Scissors, Paper) = false
let t2c = is_tie (Rock, Paper) = false
let t2d = is_tie (Rock, Rock) = true
let t2e = is_tie (Paper, Paper) = true
let t2f = is_tie (Scissors, Scissors) = true

let t3a = game_from_plays ([Rock; Paper; Rock], [Scissors; Rock; Rock]) =
               [(Rock, Scissors); (Paper, Rock); (Rock, Rock)]
let t3b = game_from_plays ([], []) =  []
let t3c = game_from_plays ([Rock; Paper; Scissors], []) = []
let t3d = game_from_plays ([], [Rock; Paper; Scissors]) = []
let t3e = game_from_plays ([Rock], [Scissors]) = [(Rock, Scissors)]
let t3f = game_from_plays ([Paper], [Rock; Paper]) = [(Paper, Rock)]
let t3g = game_from_plays ([Paper; Paper], [Paper; Scissors]) = [(Paper, Paper); (Paper, Scissors)]

let t4a = valid_game [(Rock, Scissors)] = true
let t4b = valid_game [] = false
let t4c = valid_game [(Rock, Paper); (Paper, Rock)] = false
let t4d = valid_game [(Paper, Paper)] = false
let t4e = valid_game [(Paper, Paper); (Rock, Rock); (Scissors, Scissors); (Rock, Paper)] = true

let t5a = play_game [(Rock, Rock); (Scissors, Rock)] = SndWin
let t5b = play_game [(Rock, Rock)] = Tie
let t5c = play_game [(Rock, Rock); (Paper, Rock)] = FstWin
let t5d = play_game [(Paper, Paper); (Rock, Rock); (Scissors, Scissors); (Paper, Rock)] = FstWin
let t5e = play_game [(Rock, Paper)] = SndWin

let t6a = to_f (F 2.3) = 2.3
let t6b = to_f (C 0.0) = 32.0
let t6c = to_f (C 100.0) = 212.0
let t6d = to_f (F 32.0) = 32.0

let t7a = temp_compare (F 2.3, F 4.5) = -1
let t7b = temp_compare (C 0.0, F 32.0) = 0
let t7c = temp_compare (C 100.0, F 99.0) = 1

let t8a = string_of_temp (C 2.3) = "2.3C"

let t9a = max_temp [F 2.1; C 2.1] = C 2.1

let t10a = max_temp2 [F 2.1; C 2.1] = C 2.1

