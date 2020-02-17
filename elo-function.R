ELO <- function(home.jumper.ELO,away.jumper.ELO,k){
  EA <- (1 / (1 + 10^((away.jumper.ELO - home.jumper.ELO)/400)))
  EB <- (1 / (1 + 10^((home.jumper.ELO - away.jumper.ELO)/400)))
  
  new.home.ELO.win  <- home.jumper.ELO + k * (1 - EA)
  new.home.ELO.loss  <- home.jumper.ELO + k * (0 - EA)

  new.away.ELO.win  <- away.jumper.ELO + k * (1 - EB)
  new.away.ELO.loss  <- away.jumper.ELO + k * (0 - EB)

  chance.to.win <- data.frame(chance.to.win=c(EA, EB))
  home.jumper.wins  <- round(data.frame(home.jumper.wins=c(new.home.ELO.win, new.away.ELO.loss)), digits=0)
  road.jumper.wins  <- round(data.frame(road.jumper.wins=c(new.home.ELO.loss, new.away.ELO.win)), digits=0)

  df <- cbind(chance.to.win, home.jumper.wins, road.jumper.wins)
  rownames(df) <- c('home.jumper', 'away.jumper')
  return(df)
}
