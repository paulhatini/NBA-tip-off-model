codes.2018 <- c(401070693:401070855, 401070857:401071903, 401070213:401070223, 401070233:401070240)
twenty18 <- do.call(rbind, apply(as.array(codes.2018), 1, game.summary))
twenty18$date <- as.Date(twenty18$date, format="%B %d, %Y")
twenty18 <- twenty18[order(twenty18$date),]
reg.2018 <- na.omit(twenty18)

codes.2019 <- c(401160623:401160632, 401160653:401160957)
twenty19 <- do.call(rbind, apply(as.array(codes.2019), 1, game.summary))
twenty19$date <- as.Date(twenty19$date, format="%B %d, %Y")
twenty19 <- twenty19[order(twenty19$date),]
reg.2019 <- na.omit(twenty19)
