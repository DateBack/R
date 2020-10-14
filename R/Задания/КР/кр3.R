{
  setwd('C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/БилюкинСергей')
  print(dir())


  for (shop in dir()) {
    in_ <- data.frame(Day = 1:7, Supply = round(runif(7, 1, 60)))
    write.table(in_, file = paste0(getwd(), '/', shop, '/', 'in.txt'), sep = ';', row.names=FALSE)
  }

}