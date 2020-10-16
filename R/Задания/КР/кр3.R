{
  setwd('C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/БилюкинСергей')


  for (shop in dir()) {
    in__ <- data.frame(Day = 1:7, Supply = round(runif(7, 1, 60)))
    write.table(in__, file = paste0(getwd(), '/', shop, '/', 'in.txt'), sep = ';', row.names=FALSE)
  }

  generate_supply_sale <- function (path='C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/БилюкинСергей/Магазин1', type='in', min_=1, max_=60, headers=FALSE) {

    if (type == 'in') {
      in_ <- data.frame(Day = 1:7, Supply = round(runif(7, min_, max_)))
      write.table(in_, file = paste0(path, '/', 'in.txt'), sep = ';', row.names=FALSE)
    } else {

      if (grepl("(in)", format(dir(path)[1], digits = 20))) {
        in_ <- read.table(paste0(path, '/', 'in.txt'), sep = ';', head=TRUE, encoding = 'UTF-8')$Supply
        out_ <- data.frame(Day = 1:7, Sale = round(runif(7, min_, max_)))

        for (i in 1:length(in_)) {
          if (out_$Sale[i] > in_[i]) {
            out_$Sale[i] <- in_[i]
          }
        }

        write.table(out_, file = paste0(path, '/', 'out.txt'), sep = ';', row.names=FALSE)

      } else {
        out_ <- data.frame(Day = 1:7, Supply = round(runif(7, min_, max_)))
        write.table(out_, file = paste0(getwd(), '/', shop, '/', 'out.txt'), sep = ';', row.names=FALSE)
      }

    }
  }

  generate_supply_sale(type = 'in')
  print('end')

}