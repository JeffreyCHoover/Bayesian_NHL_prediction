library(tidyverse)
library(rvest)
library(zoo)
library(glue)
library(here)

full_game <- data.frame()
continue <- TRUE
teamColors <- readRDS(here("teamColors.RDS"))
teamColors <- readRDS(here("teamColors.RDS"))

while(continue)
{
#load game play-by-play data
game_data <- read_html("http://www.espn.com/nhl/playbyplay/_/gameId/401131477")

tables <- html_nodes(game_data, css = "table")
tables <- html_table(tables, fill = TRUE)

game_info <- tables[[6]] 
game_info <- game_info[, !duplicated(colnames(game_info), fromLast = TRUE)] 
game_info <- game_info[1:4]

awayTeam <- colnames(game_info)[3]
homeTeam <- colnames(game_info)[4]
homeColor <- teamColors %>%
  filter(team == homeTeam) %>%
  pull(primary_color)
awayColor <- teamColors %>%
  filter(team == awayTeam) %>%
  pull(case_when(primary_color == homeColor ~ secondary_color,
                  TRUE ~ primary_color))

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
  y = dnorm(seq(-10, 10, 0.1), mean = -1.5, sd = 2)# probability of winning
) %>%
  mutate(winner = ifelse(x <= 0, homeTeam, awayTeam)) %>% 
  ggplot() +
  geom_ribbon(aes(x = x, ymin = 0, ymax = y, fill = winner)) +
  labs(x = "Away team's margin of victory", y = "Probability", 
       fill = "Teams", title = "Pre-game Win Probabilities") +
  scale_fill_manual(values = c(awayColor, homeColor))

source(here("NHL.functions.R"))
periods <- period(p, tables)

full_game <- full_game %>%
  rbind(periods) %>%
  distinct() %>%
  filter(!is.na(away_score), !is.na(home_score)) %>%
  arrange(period, minute, second)

second <- 0:59
minute <- time_convert(p * 20)

game <- crossing(minute, second) %>%
  arrange(desc(minute), desc(second)) %>%
  mutate(min_remain = minute + (second / 60), 
         min_played = 60 - min_remain,
         home = 0, 
         away = 0) %>%
  filter(minute < 60 | (minute == 60 & second == 0))

for (i in seq_len(nrow(full_game))) {
  cur_time <- round(full_game$min_remain[i], digits = 2)
  cur_row <- which(round(game$min_remain, digits = 2) == cur_time)
  game$home[cur_row:nrow(game)] <- full_game$home_score[i]
  game$away[cur_row:nrow(game)] <- full_game$away_score[i]
}

game <- game %>%
  mutate(
    away_margin = away - home,
    mean = (-1.5 * (min_remain / 60)) + (away_margin * (min_played / 60)),
    sd = 2 / sqrt(60 / min_remain),
    home_winprob = pnorm(0, mean = mean, sd = sd, lower.tail = TRUE),
    away_winprob = 1 - home_winprob
  ) %>%
  filter(min_played < full_game$min_played[nrow(full_game)])

game %>%
  gather(team, winprob, home_winprob:away_winprob) %>%
  ggplot(aes(x = min_played, y = winprob, color = team)) +
  geom_line() + 
  scale_color_manual(values = c(awayColor, homeColor),
                     labels = c(awayTeam, homeTeam))

dev.off()
dev.new(3, 3)
plot(
game %>%
  gather(team, winprob, home_winprob:away_winprob) %>%
  ggplot(aes(x = min_played, y = winprob, ymin = 0, ymax = 100, fill = team)) +
  geom_area(position = "fill", size = 1) +
  scale_fill_manual(values = c(awayColor, homeColor),
                     labels = c(paste0(awayTeam, " Win Prob", sep = ""),
                                paste0(homeTeam, " Win Prob", sep = ""))) +
  geom_hline(aes(yintercept = 0.5), color = "#000000", linetype = "dashed",
             size = 1) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1),
                     labels = paste0(seq(0, 100, by = 10), "%")) +
  scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 4),
                     labels = paste0(seq(60, 0, -4))) +
  labs(y = "Win Probability", x = "Minutes Remaining", 
       title = paste0(awayTeam, " vs ", homeTeam, " Win Probability", 
                      sep = "")) +
  #theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        panel.background = element_rect(fill = 'gray', colour = 'gray'),
        plot.title = element_text(hjust = 0.5))
)

if(full_game$min_remain[nrow(full_game)] == 0 & 
   full_game$away_score != full_game$home_score) {
  continue <- FALSE
} else {
  Sys.sleep(120)
}
}
