# codes.2018 <- c(401070693:401070855, 401070857:401071903, 401070213:401070223, 401070233:401070240)
# twenty18 <- do.call(rbind, apply(as.array(codes.2018), 1, game.summary))
# twenty18$date <- as.Date(twenty18$date, format="%B %d, %Y")
# twenty18 <- twenty18[order(twenty18$date),]
# reg.2018 <- na.omit(twenty18)

# Data frame reading and cleaning
reg.2018 <- read.csv("reg.2018.csv", header=TRUE)
reg.2018$date <- as.Date(reg.2018$date, format="%B %d, %Y")
reg.2018 <- reg.2018[order(reg.2018$date),]
reg.2018$away.jumper <- as.character(reg.2018$away.jumper)
reg.2018$home.jumper <- as.character(reg.2018$home.jumper)

# Record initialization and population
for(i in 1:nrow(reg.2018)){
  if(reg.2018$winning.team[i] == reg.2018$home.team[i]){
    reg.2018$winning.jumper[i] <- reg.2018$home.jumper[i]
    reg.2018$losing.jumper[i] <- reg.2018$away.jumper[i]
  }
  else{
    reg.2018$winning.jumper[i] <- reg.2018$away.jumper[i]
    reg.2018$losing.jumper[i] <- reg.2018$home.jumper[i]
  }
}

# Helper/summary dataframe 'total' creation and related columns in 'reg.2018' populated
library(dplyr)
total1 <- as.data.frame(count(reg.2018, home.jumper))
names(total1)[1] <- "jumper"
total2 <- as.data.frame(count(reg.2018, away.jumper))
names(total2)[1] <- "jumper"

total <- merge(total1, total2, by="jumper", all.x=TRUE, all.y = TRUE)
total$total <- total$n.x + total$n.y

total.wins <- as.data.frame(count(reg.2018, winning.jumper))
names(total.wins)[1] <- "jumper"
total <- merge(total, total.wins, by="jumper", all.x=TRUE)
names(total)[5] <- "wins"
total$percentage <- total$wins/total$total

# Helper/summary dataframe 'record' creation and related columns in 'reg.2018' populated
jumpers <- total$jumper
wins <- rep(0, length(jumpers))
losses <- rep(0, length(jumpers))
record <- cbind(jumpers, wins, losses)
record <- as.data.frame(record)
record$wins <- 0
record$losses <- 0

reg.2018$home.jumper.wins <- 0
reg.2018$home.jumper.losses <- 0
reg.2018$away.jumper.wins <- 0 
reg.2018$away.jumper.losses <- 0

for(i in 1:nrow(reg.2018)){
  for(j in 1:nrow(record)){
    if(reg.2018$winning.jumper[i] == record$jumpers[j]){
      if(reg.2018$winning.jumper[i] == reg.2018$home.jumper[i]){
        reg.2018$home.jumper.wins[i] <- record$wins[j]
        record$wins[j] = record$wins[j] + 1
        reg.2018$home.jumper.losses[i] <- record$losses[j]
      }else{
        reg.2018$away.jumper.wins[i] <- record$wins[j]
        record$wins[j] = record$wins[j] + 1
        reg.2018$away.jumper.losses[i] <- record$losses[j]
      }
    }
  }
  for(k in 1:nrow(record)){
    if(reg.2018$losing.jumper[i] == record$jumpers[k]){
      if(reg.2018$losing.jumper[i] == reg.2018$home.jumper[i]){
        reg.2018$home.jumper.losses[i] <- record$losses[k]
        record$losses[k] = record$losses[k] + 1
        reg.2018$home.jumper.wins[i] <- record$wins[k]
      }else{
        reg.2018$away.jumper.losses[i] <- record$losses[k]
        record$losses[k] = record$losses[k] + 1
        reg.2018$away.jumper.wins[i] <- record$wins[k]
      }
    }
  }
}

# Helper dataframe 'ELO.df' creation and related columns in 'reg.2018' populated (ELO function required)
jumpers <- total$jumper
rating <- rep(0, length(jumpers))
ELO.df <- cbind(jumpers, rating)
ELO.df <- as.data.frame(ELO.df)
ELO.df$rating <- 1500

reg.2018$home.jumper.ELO.k20 <- 0
reg.2018$away.jumper.ELO.k20 <- 0
reg.2018$ELO.estimate.k20 <- 0 


for(i in 1:nrow(reg.2018)){
  home.index <- which(ELO.df$jumpers == reg.2018$home.jumper[i])
  away.index <- which(ELO.df$jumpers == reg.2018$away.jumper[i])
  reg.2018$home.jumper.ELO.k20[i] <- ELO.df$rating[home.index]
  reg.2018$away.jumper.ELO.k20[i] <- ELO.df$rating[away.index]
  match <- ELO(reg.2018$home.jumper.ELO.k20[i], reg.2018$away.jumper.ELO.k20[i], 20)
  reg.2018$ELO.estimate.k20[i] <- match[1,1]
  if(reg.2018$winning.team[i] == reg.2018$home.team[i]){
    ELO.df$rating[home.index] <- match[1,2]
    ELO.df$rating[away.index] <- match[2,2]
  }
  if(reg.2018$winning.team[i] == reg.2018$away.team[i]){
    ELO.df$rating[home.index] <- match[1,3]
    ELO.df$rating[away.index] <- match[2,3]
  }
}

# 'result' column for evaluation initialized and populated
reg.2018$result <- 0

for(i in 1:nrow(reg.2018)){
  if(reg.2018$winning.team[i] == reg.2018$home.team[i]){
    reg.2018$result[i] <- 1
  }
}

# 'relevant.2018.reg' dataframe created from 2018.reg with sufficient sample size
for(i in 1:nrow(reg.2018)){
  reg.2018$home.total[i] <- reg.2018$home.jumper.wins[i] + reg.2018$home.jumper.losses[i]
  reg.2018$away.total[i] <- reg.2018$away.jumper.wins[i] + reg.2018$away.jumper.losses[i]
}

relevent.2018.reg <- reg.2018[ which(reg.2018$home.total > 10 & reg.2018$away.total > 10),]

# ROC plot
library(AUC)
library(plotROC)
library(ggplot2)
k20 <- ggplot(relevent.2018.reg, aes(d = result, m = ELO.estimate.k20)) + geom_roc() + geom_abline(slope = 1, intercept = 0)
auc11 <- as.character(round(calc_auc(k20)$AUC, 2))
k20
print(paste("Area under curve:", auc11))



