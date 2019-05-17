library(tidyverse)
library(rvest)
library(zoo)

#load game play-by-play data
game_data <- read_html("http://www.espn.com/nhl/playbyplay/_/gameId/401126320")

tables <- html_nodes(game_data, css = "table")
tables <- html_table(tables, fill = TRUE)

game_info <- tables[[6]] 
game_info <- game_info[, !duplicated(colnames(game_info), fromLast = TRUE)] 
game_info <- game_info[1:4]

awayTeam <- colnames(game_info)[3]
homeTeam <- colnames(game_info)[4]

# variable to track period number
p <- case_when(grepl(pattern = "1st period", 
                     x = game_info$Play[nrow(game_info)]) ~ 1,
               grepl(pattern = "2nd period", 
                     x = game_info$Play[nrow(game_info)]) ~ 2,
               grepl(pattern = "3rd period", 
                     x = game_info$Play[nrow(game_info)]) ~ 3,
               grepl(pattern = "1st Overtime",
                     x = game_info$Play[nrow(game_info)]) ~ 4 ,
               TRUE ~ 5)

tibble(
  x = seq(-10, 10, 0.1),  # x axis in increments of .5
  y = dnorm(seq(-10, 10, 0.1), mean = -1.34, sd = 2)# probability of winning
) %>%
  mutate(winner = ifelse(x <= 0, homeTeam, awayTeam)) %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = 0, ymax = y, fill = winner)) +
  labs(x = "Away team's margin of victory", y = "Probability")

if(p == 1) 
{
  period_1 <- tables[[6]]   #load period play-by-play data
  
  #remove blank 'Play' column
  period_1 <- period_1[, !duplicated(colnames(period_1), fromLast = TRUE)] 
  period_1 <- period_1[1:4]   # remove unnamed column
  
  # fill score values from end to beginning
  period_1[[3]] <- na.locf(period_1[[3]], na.rm = FALSE, fromLast = TRUE)
  period_1[[4]] <- na.locf(period_1[[4]], na.rm = FALSE, fromLast = TRUE)
  
  period_1 <- period_1 %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),  
      second = gsub(".*:", "", Time) %>% as.numeric(),  
      min_played = (minute + (second / 60)),            
      min_remain = 60 - min_played,                     
      away_score = period_1[[3]],                       
      home_score = period_1[[4]]) %>% #,                      
    #period = "P1") %>%
    select(minute, second, min_played, min_remain, away_score,
           home_score)  %>%
    arrange(min_played)
  
  #if(period_1$min_played[nrow(period_1)] == 20)
  #{
  #  p <- 2
  #}
}

if(p == 2)
{
  period_2 <- tables[[6]] 
  period_2 <- period_2[, !duplicated(colnames(period_2), fromLast = TRUE)] 
  period_2 <- period_2[1:4] 
  
  period_2[[3]] <- na.locf(period_2[[3]], na.rm = FALSE, fromLast = TRUE)
  period_2[[4]] <- na.locf(period_2[[4]], na.rm = FALSE, fromLast = TRUE)
  
  period_2 <- period_2 %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),
      second = gsub(".*:", "", Time) %>% as.numeric(),
      min_played = (minute + (second / 60)) + (p - 1) * 20,
      min_remain = 60 - min_played,
      away_score = period_2[[3]],
      home_score = period_2[[4]],
      period = "P2"
    ) %>%
    select(period, minute, second, min_played, min_remain, away_score,
           home_score) %>%
    arrange(min_played)
  
  #if(period_2$min_played[nrow(period_2)] == 20)
  #{
  #  p <- 3
  #}
}

if(p == 3) 
{
  period_3 <- tables[[6]] 
  period_3 <- period_3[, !duplicated(colnames(period_3), fromLast = TRUE)] 
  period_3 <- period_3[1:4] 
  
  period_3[[3]] <- na.locf(period_3[[3]], na.rm = FALSE, fromLast = TRUE)
  period_3[[4]] <- na.locf(period_3[[4]], na.rm = FALSE, fromLast = TRUE)
  
  period_3 <- period_3 %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),
      second = gsub(".*:", "", Time) %>% as.numeric(),
      min_played = (minute + (second / 60)) + (p - 1) * 20,
      min_remain = 60 - min_played,
      away_score = period_3[[3]],
      home_score = period_3[[4]],
      period = "P3"
    ) %>%
    select(period, minute, second, min_played, min_remain, away_score,
           home_score) %>%
    arrange(min_played)
  
  #if(period_3$min_played[nrow(period_3)] == 20 & 
  #   period_3$away_score == period_3$home_score)
  #{
  #  p <- p + 1
  #}
}

if(p == 4)
{
  period_OT <- tables[[6]] 
  period_OT <- period_OT[, !duplicated(colnames(period_OT), fromLast = TRUE)] 
  period_OT <- period_OT[1:4] 
  
  period_OT[[3]] <- na.locf(period_OT[[3]], na.rm = FALSE, fromLast = TRUE)
  period_OT[[4]] <- na.locf(period_OT[[4]], na.rm = FALSE, fromLast = TRUE)
  
  period_OT <- period_OT %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),
      second = gsub(".*:", "", Time) %>% as.numeric(),
      min_played = (minute + (second / 60)),
      min_remain = 60 - min_played,
      away_score = period_OT[[3]],
      home_score = period_OT[[4]],
      period = "OT1"
    ) %>%
    select(period, minute, second, min_played, min_remain, away_score,
           home_score)
  
  #if(period_OT$min_played[nrow(period_OT)] == 20 & 
  #   period_OT$away_score == period_OT$home_score)
  #{
  #  p <- p + 1
  #}
}

if(p == 5)
{
  period_OT2 <- tables[[6]] 
  period_OT2 <- period_OT2[, !duplicated(colnames(period_OT2), fromLast = TRUE)] 
  period_OT2 <- period_OT2[1:4] 
  
  period_OT2[[3]] <- na.locf(period_OT2[[3]], na.rm = FALSE, fromLast = TRUE)
  period_OT2[[4]] <- na.locf(period_OT2[[4]], na.rm = FALSE, fromLast = TRUE)
  
  period_OT2 <- period_OT2 %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),
      second = gsub(".*:", "", Time) %>% as.numeric(),
      min_played = (minute + (second / 60)),
      min_remain = 60 - min_played,
      away_score = period_OT2[[3]],
      home_score = period_OT2[[4]],
      period = "OT2"
    ) %>%
    select(period, minute, second, min_played, min_remain, away_score,
           home_score)
  
  #if(period_OT2$min_played[nrow(period_OT2)] == 20)
  #{
  #  p <- p + 1
  #}
}

if(p == 1)
{
  full_game <- bind_rows(list(period_1))
}
if(p == 2)
{
  full_game <- bind_rows(list(period_1, period_2))
}
if(p == 3)
{
  full_game <- bind_rows(list(period_1, period_2, period_3))
}
if(p == 4)
{
  full_game <- bind_rows(list(period_1, period_2, period_3, period_OT))
}
if(p == 5)
{
  full_game <- bind_rows(list(period_1, period_2, period_3, period_OT,
                              period_OT2))
}

full_game <- full_game %>%
  #rename(!!away := away_score, !!home := home_score) %>%
  filter(!is.na(away_score), !is.na(home_score))

if (p < 3.1)
{
  minute <- 0:60
  second <- 0:59
} else
{
  if(p == 4)
  {
    minute <- 1:20
    second <- 0:59
  } else
  {
    minute <- 1:40
    second <- 0:59
  }
}

game <- crossing(minute, second) %>%
  arrange(desc(minute), desc(second)) %>%
  mutate(min_remain = minute + (second / 60), 
         min_played = 60 - min_remain,
         home = 0, 
         away = 0) %>%
  filter(min_remain < 60)

for (i in seq_len(nrow(full_game))) {
  cur_time <- round(full_game$min_remain[i], digits = 2)
  cur_row <- which(round(game$min_remain, digits = 2) == cur_time)
  game$home[cur_row:nrow(game)] <- full_game$home_score[i]
  game$away[cur_row:nrow(game)] <- full_game$away_score[i]
}

game <- game %>%
  mutate(
    away_margin = away - home,
    mean = (-1.34 * (min_remain / 60)) + (away_margin * (min_played / 60)),
    sd = 2 / sqrt(60 / min_remain),
    home_winprob = pnorm(0, mean = mean, sd = sd, lower.tail = TRUE),
    away_winprob = 1 - home_winprob
  ) %>%
  filter(min_played < full_game$min_played[nrow(full_game)])

game

game %>%
  gather(team, winprob, home_winprob:away_winprob) %>%
  ggplot(aes(x = min_played, y = winprob, color = team)) +
  geom_line()

game %>%
  gather(team, winprob, home_winprob:away_winprob) %>%
  ggplot(aes(x = min_played, y = winprob, color = team)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "yellow"),
                     labels = c(awayTeam, homeTeam)) +
  geom_hline(aes(yintercept = 0.5), color = "#000000", linetype = "dashed",
             size = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10), "%")) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 4),
                     labels = paste0(seq(60, 0, -4))) +
  labs(y = "Win Probability", x = "Minutes Remaining") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())
