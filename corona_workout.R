library(tidyverse)

# extra settings
jumping_rope <- F # keep jumping rope?
extra_hard <- F # >25 are counted backwards, and workout is finished once landing exactly on 25

# click SOURCE to get today's corona workout printed out for you!

game_set <- matrix(c(
  # square instruction set rep unit extra
  "1",  "planc",              "1", "1", "min",  NA,
  "2",  "upward bridge",      "1", "10", "rep", NA,
  "3",  "superman",           "1", "30", "s",   NA,
  "4",  "lunges",             "1", "10", "rep", "per leg",
  "5",  "jumping jacks",      "1", "30", "s", NA,
  "6",  "lateral squats",     "1", "30", "s", "per side",
  "7",  "REPEAT", NA,NA,NA,NA, 
  "8",  "mountain climbers",  "1", "30", "s", NA,
  "9",  "sumo squats",        "1", "12", "rep", NA,
  "10", "jumping ropes",      "1", "30", "s", NA,
  "11", "lateral planc",      "1", "30", "s", "per side",
  "12", "dips",               "1", "10", "rep", NA,
  "13", "push ups",           "1", "8", "rep", NA,
  "14", "GO BACK 3 STEPS", NA,NA,NA,NA,
  "15", "squats",             "1", "15", "rep", NA,
  "16", "chair stand",        "1", "30", "s", NA,
  "17", "donkey kicks",       "1", "10", "rep", "per leg",
  "18", "jumping squats",     "1", "8", "rep", NA,
  "19", "RESTART", NA,NA,NA,NA,
  "20", "burpees",            "1", "8", "rep", NA,
  "21", "v planc",            "1", "30", "s", NA,
  "22", "squats",             "1", "20", "rep", NA,
  "23", "GO BACK 2 STEPS", NA,NA,NA,NA,
  "24", "stationary running", "1", "30", "s", NA,
  "25", "chair stand",        "1", "30", "s", NA
  ), nrow = 25, byrow = T)
colnames(game_set) <- c("square", "instruction", "set", "time", "unit", "extra")
game_set <- as_tibble(game_set)

if(!jumping_rope) game_set[10,2] <- "stationary running"
# game_set

game_now <- tibble()
cumul_step <- 0
while( cumul_step < 25 ) {
  dice_roll <- sample(1:6, 1)
  cumul_step <- sum(cumul_step, dice_roll)
  
  print(cumul_step)
  if(!extra_hard) {
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
  } else {
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
      cumul_step <- 25 - cumul_step + 25
      game_now <- bind_rows(game_now, slice(game_set, cumul_step))
    } else {
      game_now <- bind_rows(game_now, slice(game_set, cumul_step))
    }
  }
}

game_now <- game_now %>% select(-square)

if(nrow(game_now) == 10) {
  game_now
} else {
  game_now %>% as.data.frame()
}