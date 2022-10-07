library(tidyverse)
library(keypress)
library(glue)

# Growing up in the West Indies we played a bootleg version of cricket using a pencil. 
# You marked the middle of each side: 0, 1, 2, 4, 6, W, and bowled by rolling the pencil and 
# stopping it with a finger. Whatever was written on the upturned side determined the result - no 
# runs, some number of runs, or a wicket.

# This implementation mirrors this game on the command line. Run using `Rscript cli_cricket.R` and 
# press any button to bowl. The defaults are set for five overs, four wickets, and the user bowling 
# both innings (one per team). Setting the `user` argument of play_innings(x1...xn) can change that. 
# The computer randomly picks a bowling outcome.

init_batting <- function(team){
  
  result = mutate(
    team, 
    status = case_when(positions == 1 ~ "on strike", positions == 2 ~ "not out", TRUE ~ status)
    )
  
  return(result)
  
}

bowl_ball <- function(){
  
  result = sample(seq(1, 6, 1), 1)
  
  return(result)
  
}

bowl_ball_actual <- function(){
  
  while(TRUE){
    
    for(i in seq(1, 6, 1)){
      
      cat("\r", i)
      Sys.sleep(.2)
      stop_signal <- keypress(block = FALSE)
      
      if(stop_signal != "none") {
        result = i
        break
      }
      
    }
    
    if(stop_signal != "none") break
    
  }
  
  return(result)
  
}

add_runs <- function(team, outcome){
  
  result = mutate(
    team, 
    score = case_when(status == "on strike" ~ score + outcome, TRUE ~ score)
  )
  
  return(result)
  
  
}

switch_strike <- function(team){
  
  result = mutate(
    team, 
    status = case_when(status == "on strike" ~ "not out", status == "not out" ~ "on strike", TRUE ~ status)
  )
  
  return(result)
  
}

wicket_taken <- function(team, wickets_taken){
  
  result = mutate(team, status = case_when(status == "on strike" ~ "out", TRUE ~ status))
  
  if(wickets_taken != 4){
    
    next_to_bat <- result %>% 
      filter(status == "yet to bat") %>% 
      filter(positions == min(positions)) %>% 
      pull(positions)
    
    result <- mutate(
      result, 
      status = case_when(positions == next_to_bat ~ "on strike", TRUE ~ status)
      )
    
  }
  
  return(result)
  
}

play_innings <- function(team, target = -1, wickets_previous_innings = -1, chase = FALSE, user = FALSE){
  
  wickets_taken = 0
  balls_bowled = 0
  team = init_batting(team)
  
  while ((wickets_taken < 4) & (balls_bowled < overs*6)) {
    
    outcome = ifelse(user, bowl_ball_actual(), bowl_ball())
    balls_bowled = balls_bowled  + 1
    wickets_taken = ifelse(outcome %% 5 == 0, wickets_taken + 1, wickets_taken)
    
    team <- mutate(
      team, 
      balls_faced = case_when(status == "on strike" ~ balls_faced + 1, TRUE ~ balls_faced)
    )
    
    add_runs_partial = partial(add_runs, outcome = outcome)
    wicket_taken_partial = partial(wicket_taken, wickets_taken = wickets_taken)
    add_runs_switch_strike = compose(switch_strike, add_runs_partial)
    
    evaluate_outcome <- switch(
      as.character(outcome),
      "1" = add_runs_switch_strike,
      "2" = add_runs_partial,
      "3" = add_runs_switch_strike,
      "4" = add_runs_partial,
      "5" = wicket_taken_partial,
      "6" = add_runs_partial
    )
    
    team = evaluate_outcome(team)
    ticker = glue(
      "Score: {sum(pull(team, score))}/{wickets_taken}  Balls: {balls_bowled}/{overs*6}"
      )
    
    print(glue("SCORECARD"))
    cat("\n")
    print(glue("Result: {ifelse(outcome == 5, 'Wicket!', paste(outcome, 'run/s'))}"))
    cat("\n")
    print(team)
    cat("\n")
    print(ticker)
    cat("\n")
    
    if(chase & (sum(pull(team, score)) > target)){
      
      print(glue("Chasing Team Wins!\n
                 Team 1: {target}/{wickets_previous_innings}
                 Team 2: {sum(pull(team, score))}/{wickets_taken}"))
      break
      
    }
    
  }
  
  if(chase & (sum(pull(team, score)) < target)){
    
    print(glue("Defending Team Wins!\n
                 Team 1: {target}/{wickets_previous_innings}
                 Team 2: {sum(pull(team, score))}/{wickets_taken}"))
    
  }
  
  if(chase & (sum(pull(team, score)) == target)){
    
    print(glue("Draw!\n
                 Team 1: {target}/{wickets_previous_innings}
                 Team 2: {sum(pull(team, score))}/{wickets_taken}"))
    
  }
  
  result = list(
    "target" = sum(pull(team, score)),
    "wickets_previous_innings" = wickets_taken
  )
  
  return(result)
  
}

generate_teams <- function(){
  
  players <- c("A. Baudrille", "C. Duffy", "E. Francis", "G. Hardy", "I. Jensen",
               "K. Landry",  "M. Noat", "O. Paulus", "Q. Rincon", "S. Tracy")
  
  team1 = seq(1, 5, 1) %>% 
    data.frame(positions = .) %>% 
    mutate(players = players[1:5]) %>% 
    mutate(status = "yet to bat") %>% 
    mutate(score = 0) %>% 
    mutate(balls_faced = 0)
  
  team2 = seq(1, 5, 1) %>% 
    data.frame(positions = .) %>% 
    mutate(players = players[6:10]) %>% 
    mutate(status = "yet to bat") %>% 
    mutate(score = 0) %>% 
    mutate(balls_faced = 0)
  
  result <- list("team1" = team1, "team2" = team2)
  
  return(result)
  
}

start_game <- \(){
  
  overs <- 5
  user_controlled <- TRUE
  teams <- generate_teams()
  team1 <- teams["team1"]
  team2 <- teams["team2"]
  
  cat("\n\nFirst Innings\n\n")
  score <- play_innings(team1, user = user_controlled)
  cat("\n\nSecond Innings\n\n")
  
  throwaway <- play_innings(
    team2,
    score["target"], 
    score["wickets_previous_innings"], 
    chase = TRUE, 
    user = user_controlled
  )
  
}



