period <- function(p, tables)
{
  period <- as.data.frame(tables[[6]])
  period <- period[, !duplicated(colnames(period), fromLast = TRUE)] 
  period <- period[1:4] 
    
  period[[3]] <- na.locf(period[[3]], na.rm = FALSE, fromLast = TRUE)
  period[[4]] <- na.locf(period[[4]], na.rm = FALSE, fromLast = TRUE)
    
  period <- period %>%
    mutate(
      minute = gsub(":.*", "", Time) %>% as.numeric(),
      second = gsub(".*:", "", Time) %>% as.numeric(),
      min_played = (minute + (second / 60)) + (p - 1) * 20,
      min_remain = 60 - min_played,
      away_score = period[[3]],
      home_score = period[[4]],
      period = p
    ) %>%
    select(period, minute, second, min_played, min_remain, away_score,
           home_score) %>%
    arrange(min_played)
  
  period <- as.data.frame(period)
  
  return(period)
}

time_convert <- function(minutes)
{
  if(minutes <= 60) {
    return(0:60)
  } else {
    if(minutes == 80) {
      return(1:20)
    } else {return(1:40)}
  }
}
