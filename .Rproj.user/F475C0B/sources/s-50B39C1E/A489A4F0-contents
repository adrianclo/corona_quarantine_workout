library(tidyverse)

# click SOURCE to get today's corona workout printed out for you!

game_set <- tibble(
  square = 1:25,
  instruction = c(
    "planc", "upward bridge", "superman",
    "lunges","jumping jacks", "lateral squats",
    "REPEAT", # square 7
    "mountain climbers", "sumo squats",
    "jumping rope", "lateral planc", "dips",
    "push ups", "GO BACK 3 STEPS", # square 14
    "squats",
    "chair stand", "donkey kicks", "jumping squats",
    "RESTART", # square 19
    "burpees", "v planc",
    "squats", "GO BACK 2 STEPS", # square 23
    "stationary running", "chair stand"
  ),
  time = c(
    "1 min", "12 rep", "30 s",
    "10 rep per leg", "30 s", "10 rep per leg",
    NA, "30 s", "12 rep",
    "30 s", "30 s per side", "10 rep",
    "8 rep", NA, "15 rep",
    "30 s", "10 rep per leg", "8 rep",
    NA, "8 rep", "30 s",
    "20 rep", NA, "30 s", "30 s"
  )
)
# game_set

game_now <- tibble()
cumul_step <- 0
while( cumul_step < 25 ) {
  dice_roll <- sample(1:6, 1)
  cumul_step <- sum(cumul_step, dice_roll)
  
  print(cumul_step)
  
  if(cumul_step == 7) {
    game_now <- bind_rows(game_now, tail(game_now, 1))
  } else if(cumul_step == 14) {
    game_now <- bind_rows(game_now, slice(game_set, 11))
    cumul_step <- 11
  } else if(cumul_step == 19) {
    game_now <- bind_rows(game_now, slice(game_set, 1))
    cumul_step <- 1
  } else if(cumul_step == 23) { 
    game_now <- bind_rows(game_now, slice(game_set, 21))
    cumul_step <- 21
  } else if(cumul_step == 25) {
    game_now <- bind_rows(game_now, slice(game_set, cumul_step))
    cat("You are finished for today. Come back tomorrow!")
    break()
  }
  else if(cumul_step > 25) {
    cat("You are finished for today. Come back tomorrow!")
    break()
  } else {
    game_now <- bind_rows(game_now, slice(game_set, cumul_step))
  } 
}

game_now
