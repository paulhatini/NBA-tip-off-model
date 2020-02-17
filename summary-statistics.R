win.and.shoot.first.2018 <- 0

for(i in 1:nrow(reg.2018)){
  if(reg.2018$winning.team[i] == reg.2018$shoot.first[i]){
    win.and.shoot.first.2018 = win.and.shoot.first.2018 + 1
  }
}

win.and.shoot.2018 <- win.and.shoot.first.2018/nrow(reg.2018)
lose.and.shoot.2018 <- 1-win.and.shoot.2018

win.and.score.first.2018 <- 0

for(i in 1:nrow(reg.2018)){
  if(reg.2018$winning.team[i] == reg.2018$score.first[i]){
    win.and.score.first.2018 = win.and.score.first.2018 + 1
  }
}

win.and.score.2018 <- win.and.score.first.2018/nrow(reg.2018)
lose.and.score.2018 <- 1-win.and.score.2018

