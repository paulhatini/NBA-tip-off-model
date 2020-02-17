library(stringr)
game.summary <- function(code){
  code <- toString(code)
  url <- paste("https://www.espn.com/nba/playbyplay?gameId=", code, sep='')
  game <- readLines(url)
  
  date.row <- grep(x=game, "Play-By-Play", value=TRUE)[1]
  date <- na.omit(str_match(date.row, '.*y\\s-\\s(.*?)\\s-\\sESPN.*'))[2]
  
  q1.row <- grep(x=game, 'href=\"#gp-quarter-1\"', value=TRUE)
  split <- unlist(strsplit(q1.row, split = "time-stamp"))
  q1.p1 <- split[2]
  make.split <- unlist(strsplit(q1.row, split = "make"))[1]
  make.split2 <- unlist(strsplit(make.split, split = "png"))
  score.first <- na.omit(str_match(make.split2[length(make.split2)-1], '.*500/(.*?)\\..*'))[2]
  shot.split <- unlist(strsplit(q1.row, split = c("make|miss")))[1]
  s.split <- unlist(strsplit(shot.split, split = "png"))
  shoot.first <- na.omit(str_match(s.split[length(s.split)-1], '.*500/(.*?)\\..*'))[2] 
  
  away.jumper <- na.omit(str_match(q1.p1, '.*game-details\\\">(.*?)\\svs.*'))[2]
  home.jumper <- na.omit(str_match(q1.p1, '.*\\svs.\\s(.*?)\\s\\(.*'))[2]
  winning.team <- na.omit(str_match(q1.p1, '.*500/(.*?).png.*'))[2]
  
  h.a.row <- grep(x=game, 'gamepackage-header-wrap', value=TRUE)
  split <- unlist(strsplit(h.a.row, split = "png"))
  away.s <- split[1]
  away.team <- na.omit(str_match(away.s, '.*500/(.*?)\\..*'))[2]
  home.s <- split[3]
  home.team <- na.omit(str_match(home.s, '.*500/(.*?)\\..*'))[2]

  row <- data.frame(date, away.jumper, home.jumper, winning.team, away.team, home.team, shoot.first, score.first, stringsAsFactors = FALSE)
  return(row)
}
