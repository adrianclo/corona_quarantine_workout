library(tidyverse)
library(tibble)

#' (1) full work out
#' (2) planc variants
#' (3) chair and squats
set <- 2

# extra settings
sensitive_neighbour <- T # replace all jumping activities with quiet ones
level <- "hard"          # easy - medium - hard
extra_hard <- F          # >25 are counted backwards, and workout is finished once landing exactly on 25

# click SOURCE to get today's corona workout printed out for you!

game_list <- list(
  full_work_out = tibble::tribble(
    ~square, ~instruction,     ~set, ~rep, ~unit, ~extra,
    "1",  "planc",              "1", "60", "s", NA,
    "2",  "upward bridge",      "1", "10", "rep", NA,
    "3",  "superman",           "1", "30", "s",   NA,
    "4",  "lunges",             "1", "10", "rep", "per leg",
    "5",  "jumping jacks",      "1", "30", "s",   NA,
    "6",  "lateral squats",     "1", "10", "rep", "per side",
    "7",  "REPEAT", NA,NA,NA,NA, 
    "8",  "mountain climbers",  "1", "30", "s",   NA,
    "9",  "sumo squats",        "1", "12", "rep", NA,
    "10", "jumping ropes",      "1", "30", "s",   NA,
    "11", "lateral planc",      "1", "30", "s",   "per side",
    "12", "dips",               "1", "10", "rep", NA,
    "13", "push ups",           "1", "8",  "rep", NA,
    "14", "GO BACK 3 STEPS", NA,NA,NA,NA,
    "15", "squats",             "1", "15", "rep", NA,
    "16", "chair stand",        "1", "30", "s",   NA,
    "17", "donkey kicks",       "1", "10", "rep", "per leg",
    "18", "jumping squats",     "1", "8",  "rep", NA,
    "19", "RESTART", NA,NA,NA,NA,
    "20", "burpees",            "1", "8",  "rep", NA,
    "21", "v planc",            "1", "30", "s",   NA,
    "22", "squats",             "1", "20", "rep", NA,
    "23", "GO BACK 2 STEPS", NA,NA,NA,NA,
    "24", "stationary running", "1", "30", "s",   NA,
    "25", "chair stand",        "1", "30", "s",   NA
  ),
  planc = tibble::tribble(
    ~square, ~instruction,             ~set, ~rep, ~unit, ~extra,
    "1",     "standard",               "1",  "20", "s",   NA,
    "2",     "extended arms",          "1",  "30", "s",   NA,
    "3",     "standard alternate arm",     "1",  "20", "s",   NA,
    "4",     "standard",               "1",  "30", "s",   NA,
    "5",     "left side extended arm", "1",  "30", "s",   NA,
    "6",     "extended arms",          "1",  "60", "s",   NA,
    "7",     "RESTART",NA,NA,NA,NA,
    "8",     "standard",               "1",  "45", "s",   NA,
    "9",     "right side",             "1",  "30", "s",   NA,
    "10",    "standard alternate leg", "1",  "30", "s",   NA,
    "11",    "opposite extended",      "1",  "20", "s",   NA,
    "12",    "REPEAT",NA,NA,NA,NA,
    "13",    "standard alternate arm",      "1",  "30", "s",   NA,
    "14",    "right side extended arm","1",  "30", "s",   NA,
    "15",    "GO FORWARD 2 STEPS",NA,NA,NA,NA,
    "16",    "right side extended arm left leg up","1","20","s",NA,
    "17",    "opposite standard",      "1",  "20", "s", NA,
    "18",    "standard",               "1", "60", "s", NA,
    "19",    "extended alternate arm and leg", "1", "30", "s", NA,
    "20",    "right side left leg up", "1", "30", "s", NA,
    "21",    "GO BACK 3 STEPS", NA,NA,NA,NA,
    "22",    "standard alternate leg", "1", "45", "s", NA,
    "23",    "standard", "1", "60", "s", NA,
    "24",    "left side extended arm right leg up", "1","30", "s", NA,
    "25",    "extended alternate arm and leg", "1", "45", "s", NA
  )
)

game <- game_list[[set]]

if(set == 1 & sensitive_neighbour) {
  game[5,2] <- "superman"
  game[8,2] <- "push ups"
  game[8,4] <- 20
  game[8,5] <- "rep"
  game[10,2] <- "lateral planc"
  game[10,5] <- "per side"
  game[18,2] <- "push ups"
  game[20,2] <- "push ups"
  game[24,2] <- "planc"
}

if((set == 1 & level == "easy") | set == 2) {
  game <- game
} else if(set == 1 & level == "medium") {
  set_index <- sample(1:2, size = 25, replace = T, prob = c(.30, .70))
  ww <- which(is.na(game$set))
  if(length(ww) > 0) { set_index[c(7,14,19,23)] <- NA }
  game$set <- set_index
} else if(set == 1 & level == "hard") {
  set_index <- sample(1:3, size = 25, replace = T, prob = c(.10, .40, .50))
  ww <- which(is.na(game$set))
  if(length(ww) > 0) { set_index[c(7,14,19,23)] <- NA }
  game$set <- set_index
  
  game <- game %>% 
    mutate(rep = as.numeric(rep)) %>% 
    mutate(rep = rep + sample(seq(0,20, by = 5), 
                              size = 1, 
                              prob = c(.05, .15, .20, .25, .35)))
} else { stop("Your level does not exist..")}
# game

if(set == 1) {
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
        game_now <- bind_rows(game_now, slice(game, 11))
        cumul_step <- 11
      } else if(cumul_step == 19) {
        game_now <- bind_rows(game_now, slice(game, 1))
        cumul_step <- 1
      } else if(cumul_step == 23) { 
        game_now <- bind_rows(game_now, slice(game, 21))
        cumul_step <- 21
      } else if(cumul_step == 25) {
        game_now <- bind_rows(game_now, slice(game, cumul_step))
        cat("You are finished for today. Come back tomorrow!")
        break()
      }
      else if(cumul_step > 25) {
        cat("You are finished for today. Come back tomorrow!")
        break()
      } else {
        game_now <- bind_rows(game_now, slice(game, cumul_step))
      }
    } else {
      if(cumul_step == 7) {
        game_now <- bind_rows(game_now, tail(game_now, 1))
      } else if(cumul_step == 14) {
        game_now <- bind_rows(game_now, slice(game, 11))
        cumul_step <- 11
      } else if(cumul_step == 19) {
        game_now <- bind_rows(game_now, slice(game, 1))
        cumul_step <- 1
      } else if(cumul_step == 23) { 
        game_now <- bind_rows(game_now, slice(game, 21))
        cumul_step <- 21
      } else if(cumul_step == 25) {
        game_now <- bind_rows(game_now, slice(game, cumul_step))
        cat("You are finished for today. Come back tomorrow!")
        break()
      }
      else if(cumul_step > 25) {
        cumul_step <- 25 - cumul_step + 25
        game_now <- bind_rows(game_now, slice(game, cumul_step))
      } else {
        game_now <- bind_rows(game_now, slice(game, cumul_step))
      }
    }
  }
} else if(set == 2) {
  game_now <- tibble()
  cumul_step <- 0
  while( cumul_step < 25 ) {
    dice_roll <- sample(1:6, 1)
    cumul_step <- sum(cumul_step, dice_roll)
    
    print(cumul_step)
    if(cumul_step == 7) {
      game_now <- bind_rows(game_now, slice(game, 1))
      cumul_step <- 1
    } else if(cumul_step == 12) {
      game_now <- bind_rows(game_now, tail(game_now, 1))
    } else if(cumul_step == 15) {
      game_now <- bind_rows(game_now, slice(game, 17))
      cumuL_step <- 17
    } else if(cumul_step == 21) {
      game_now <- bind_rows(game_now, slice(game, 18))
      cumuL_step <- 18
    } else {
      game_now <- bind_rows(game_now, slice(game, cumul_step))
    }
  }
} else { stop("Your exercise set does not exist..") }

game_now <- game_now %>% select(-square)

if(nrow(game_now) <= 10) {
  game_now
} else {
  game_now %>% as.data.frame()
}