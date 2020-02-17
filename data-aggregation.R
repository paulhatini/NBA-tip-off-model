# codes.2018 <- c(401070693:401070855, 401070857:401071903, 401070213:401070223, 401070233:401070240)
# twenty18 <- do.call(rbind, apply(as.array(codes.2018), 1, game.summary))
# twenty18$date <- as.Date(twenty18$date, format="%B %d, %Y")
# twenty18 <- twenty18[order(twenty18$date),]
# reg.2018 <- na.omit(twenty18)

reg.2018 <- read.csv("reg.2018.csv", header=TRUE)
reg.2018$date <- as.Date(reg.2018$date, format="%B %d, %Y")
reg.2018 <- reg.2018[order(reg.2018$date),]
reg.2018$away.jumper <- as.character(reg.2018$away.jumper)
reg.2018$home.jumper <- as.character(reg.2018$home.jumper)
