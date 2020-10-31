{
  getwd()
  setwd('../БилюкинСергей')


  #for (shop in dir()) {
  #  in__ <- data.frame(Day = 1:7, Supply = round(runif(7, 1, 60)))
  #  write.table(in__, file = paste0(getwd(), '/', shop, '/', 'in.txt'), sep = ';', row.names=FALSE)
  #}

  generate_supply_sale <- function (path='../БилюкинСергей/Магазин1', type='in', min_=1, max_=60, days=7, headers=FALSE, all=FALSE) {

    if (type == 'in') {
      in_ <- data.frame(Day = 1:days, Supply = round(runif(days, min_, max_)))
      write.table(in_, file = paste0(path, '/', 'in.txt'), sep = ';', row.names=FALSE)
    } else {

      if (grepl("(in)", format(dir(path)[1], digits = 20))) {
        in_ <- read.table(paste0(path, '/', 'in.txt'), sep = ';', head=TRUE, encoding = 'UTF-8')$Supply
        out_ <- data.frame(Day = 1:days, Sale = round(runif(days, min_, max_)))

        for (i in 1:length(in_)) {
          if (out_$Sale[i] > in_[i]) {
            out_$Sale[i] <- in_[i]
          }
        }

        write.table(out_, file = paste0(path, '/', 'out.txt'), sep = ';', row.names=FALSE)

      } else {
        out_ <- data.frame(Day = 1:days, Supply = round(runif(days, min_, max_)))
        write.table(out_, file = paste0(getwd(), '/', shop, '/', 'out.txt'), sep = ';', row.names=FALSE)
      }

    }
  }

  generate_sale_level <- function (path='../БилюкинСергей/Магазин1/out.txt', all=FALSE) {

    if (all) {
      for (shop in dir()) {
        sale_levels <- vector()
        in_ <- read.table(paste0(getwd(), '/', shop, '/', 'in.txt'), sep = ';', head=TRUE, encoding = 'UTF-8')$Supply
        out_ <- read.table(paste0(getwd(), '/', shop, '/', 'out.txt'), sep = ';', head=TRUE, encoding = 'UTF-8')

        for (i in 1:length(in_)) {
          sale_levels <- append(sale_levels, round((out_$Sale[i] / in_[i]) * 100))
        }

        out_$SaleLevel <- sale_levels
        write.table(out_, file = paste0(getwd(), '/', shop, '/', 'out.txt'), sep = ';', row.names=FALSE)
      }
      print(out_)
    }
  }

  #generate_supply_sale(path = '../БилюкинСергей/Магазин3', type = 'out')
  generate_sale_level(all = TRUE)
  print('end')

}





