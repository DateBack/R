{
  setwd('C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Analysis')

  suppliers_price <- 100
  goods_price <- 150
  recovery_price <- 20

  in_ <- data.frame(Day = 1:7)
  out_ <- data.frame(Day = 1:7)


  for (i in dir()) {
    if (grepl("(in)", format(i, digits = 20))) {
      in_ <- cbind(in_, read.table(i, head=TRUE, encoding = 'UTF-8')$Supply)
    } else {
      out_ <- cbind(out_, read.table(i, head=TRUE, encoding = 'UTF-8')$Sale)
    }
  }
  colnames(in_)<-c('Day', 'store1', 'store2', 'store3', 'store4', 'store5', 'store6', 'store7', 'store8', 'store9', 'store10')
  colnames(out_)<-c('Day', 'store1', 'store2', 'store3', 'store4', 'store5', 'store6', 'store7', 'store8', 'store9', 'store10')


  profit_per_shop_v <- vector()
  sales_per_shop_v <- vector()
  revenue_per_shop_v <- vector()
  max_sale_per_day <- vector()
  max_sale_day <- vector()
  min_sale_per_day <- vector()
  min_sale_day <- vector()
  sd_on_sales_v <- vector()
  for (i in 2:length(out_)) {
    profit_per_shop <- sum(out_[i]) * goods_price - (sum(in_[i]) * suppliers_price + sum(in_[i] - out_[i]) * recovery_price)
    profit_per_shop_v <- append(profit_per_shop_v, profit_per_shop)

    revenue <- sum(out_[i]) * goods_price
    revenue_per_shop_v <- append(revenue_per_shop_v, revenue)

    max_sale_per_day <- append(max_sale_per_day, max(out_[i]))
    max_sale_day <- append(max_sale_day, which.max(out_[[i]]))

    min_sale_per_day <- append(min_sale_per_day, min(out_[i]))
    min_sale_day <- append(min_sale_day, which.min(out_[[i]]))

    sales_per_shop_v <- append(sales_per_shop_v, sum(out_[i]))

    sd_on_sales_v <- append(sd_on_sales_v, sd(out_[[i]]))
  }

  res.tab <- data.frame(Revenue = revenue_per_shop_v, Profit = profit_per_shop_v, Sales = sales_per_shop_v,
                        SD = sd_on_sales_v, MaxSale = max_sale_per_day, MaxDay = max_sale_day,
                        MinSale = min_sale_per_day, MinDay = min_sale_day)


  write.table(res.tab, 'C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Result/result.csv', sep = ';', row.names=FALSE)

}