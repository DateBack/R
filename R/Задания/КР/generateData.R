setwd("/Users/sergejbilukin/Desktop/R/R/Задания/КР/SuperShop/БилюкинСергей")
{

  generate_all <- function (min_=1, max_=60, days=7, goods='Bread', saleLevel=50) {
    for (shop in dir()) {
      names <- 'Day'
      in_ <- data.frame(Day = 1:days)
      for (i in 1:length(goods)) {
        in_ <- cbind(in_, round(runif(days, min_, max_)))
        names <- append(names, goods[i])
      }

      out_ <- data.frame(Day = 1:days)
      for (col in 1:length(goods)) {
        current_good_sales <- vector()
        for (row in 1:days) {
          #sale <- round(runif(1, min_, in_[row, col + 1]))
          sale <- round(in_[row, col + 1] * (saleLevel / 100))
          current_good_sales <- append(current_good_sales, sale)
        }
        out_ <- cbind(out_, current_good_sales)
      }
      colnames(in_)<-names
      colnames(out_)<-names

      path <- getwd()
      write.table(in_, file = paste0(path, '/', shop, '/', 'in.txt'), sep = ';', row.names=FALSE)
      write.table(out_, file = paste0(path,'/', shop, '/', 'out.txt'), sep = ';', row.names=FALSE)

    }
  }

  generate_single <- function (shop, min_=1, max_=60, days=7, goods='Bread', saleLevel=50) {
    names <- 'Day'
    in_ <- data.frame(Day = 1:days)
    for (i in 1:length(goods)) {
      in_ <- cbind(in_, round(runif(days, min_, max_)))
      names <- append(names, goods[i])
    }

    out_ <- data.frame(Day = 1:days)
    for (col in 1:length(goods)) {
      current_good_sales <- vector()
      for (row in 1:days) {
        #sale <- round(runif(1, min_, in_[row, col + 1]))
        sale <- round(in_[row, col + 1] * (saleLevel / 100))
        current_good_sales <- append(current_good_sales, sale)
      }
      out_ <- cbind(out_, current_good_sales)
    }
    colnames(in_)<-names
    colnames(out_)<-names

    path <- getwd()
    write.table(in_, file = paste0(path, '/', 'Магазин', shop, '/', 'in.txt'), sep = ';', row.names=FALSE)
    write.table(out_, file = paste0(path,'/', 'Магазин', shop, '/', 'out.txt'), sep = ';', row.names=FALSE)
  }

  goods <- c('Bread', 'Apple', 'Milk')
  goods_prices <- c(200, 300, 400)
  generate_all(goods = goods)
  #generate_single(3, goods = goods)

}

