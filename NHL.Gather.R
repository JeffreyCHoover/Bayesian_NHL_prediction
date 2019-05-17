library(rvest)
library(tidyverse)

source()

gamesList <- read_html("https://www.foxsports.com/nhl/odds")

tables <- html_nodes(gamesList, css = "table")
tables <- html_table(tables, fill = TRUE)

numGames <- length(tables)

gamesList <- matrix(0L, nrow = numGames, ncol = 3)

ii <- 1

while(ii < numGames + 1)
{
  teams <- tables[[ii]][2] %>%
    distinct()
  gamesList[ii, 1] <- substr(teams, 1, 3)
  gamesList[ii, 2] <- substr(teams, 4, 6)
  spread <- tables[[ii]][3] %>%
    distinct() %>%
    substr(., 5, 8)
  spread <- as.numeric(spread)
  gamesList[ii, 3] <- spread
  
  ii <- ii + 1
}

jj <- 1

while(jj < numGames + 1)
{
  plot(NHL_pregame(gamesList[jj, 1], gamesList[jj, 2], 
                   as.numeric(gamesList[jj, 3])))
  jj <- jj + 1
}
