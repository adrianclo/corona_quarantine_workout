library(tidyverse)

game_set <- tibble(
  square = 1:25,
  instruction = c(
    "planc", "upward bridge", "superman",
    "lunges","jumping jacks", "lateral squats",
    "REPEAT", "mountain climbers", "sumo squats",
    "jumping rope", "lateral planc", "dips",
    "push ups", "GO BACK 3 STEPS", "squats",
    "chair stand", "donkey kicks", "jumping squats",
    "RESTART", "burpees", "v planc",
    "squats", "GO BACK 2 STEPS", "stationary running", "chair stand"
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

game_set

game_now <- tibble()
cumul_step <- 0
while( cumul_step < 25 ) {
  dice_roll <- sample(1:6, 1)
  cumul_step <- sum(cumul_step, dice_roll)
  
  if(cumul_step > 25) {
    break()
  } else { print(cumul_step) }
  
  #game_now <- bind_rows(game_now, )
}
