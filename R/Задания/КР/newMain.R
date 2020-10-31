{
  setwd('C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Analysis')

  suppliers_price <- 100
  goods_price <- 220
  recovery_price <- 20
  goods <- c('Bread', 'Milk', 'Coca Cola')

  in_list <- list()
  in_list[[1]] <- read.table(dir()[1], header = TRUE, sep=';')
  #print(as.data.frame(in_list[1])[,1])


  in_ <- list()
  out_ <- list()

  for (i in dir()) {
    if (grepl("(in)", format(i, digits = 20))) {
      in_[[length(in_) + 1]] <- read.table(i, header = TRUE, sep = ';')
    } else {
      out_[[length(out_) + 1]] <- read.table(i, header = TRUE, sep=';')
    }
  }

  final_revenue_per_good <- vector()
  final_profit_per_good <- vector()
  final_max_good_sale_per_day <- vector()
  final_max_good_sale_day <- vector()
  final_min_good_sale_per_day <- vector()
  final_min_good_sale_day <- vector()
  final_sales <- vector()
  final_supply <- vector()
  final_write_downs <- vector()
  final_uniformity <- vector()

  for (i in 1:length(out_)) {
    revenue_per_good <- vector()
    profit_per_good <- vector()
    max_good_sale_per_day <- vector()
    max_good_sale_day <- vector()
    min_good_sale_per_day <- vector()
    min_good_sale_day <- vector()
    sales <- vector()
    supply <- vector()
    write_downs <- vector()
    uniformity <- vector()


    # собираем показатели по каждому магазину по каждому товару
    for (k in 2:length(as.data.frame(out_[i]))) {
      revenue_per_good <- append(revenue_per_good, sum(as.data.frame(out_[i])[, k]) * goods_price)
      profit_per_good <- append(profit_per_good, sum(as.data.frame(out_[i])[, k]) * goods_price - (sum(as.data.frame(in_[i])[, k]) * suppliers_price - sum(as.data.frame(in_[i])[, k] - as.data.frame(out_[i])[, k]) * recovery_price ))

      max_good_sale_per_day <- append(max_good_sale_per_day, max(as.data.frame(out_[i])[, k]))
      max_good_sale_day <- append(max_good_sale_day, which.max(as.data.frame(out_[i])[, k]))

      min_good_sale_per_day <- append(min_good_sale_per_day, min(as.data.frame(out_[i])[, k]))
      min_good_sale_day <- append(min_good_sale_day, which.min(as.data.frame(out_[i])[, k]))

      sales <- append(sales, sum(as.data.frame(out_[i])[, k]))
      supply <- append(supply, sum(as.data.frame(in_[i])[, k]))
      write_downs <- append(write_downs, sum(as.data.frame(in_[i])[, k]) - sum(as.data.frame(out_[i])[, k]))
      uniformity <- append(uniformity, round(sd(as.data.frame(out_[i])[, k])))

    }
    # Собираем финальные вектора
    final_revenue_per_good <- append(final_revenue_per_good, revenue_per_good)
    final_revenue_per_good <- append(final_revenue_per_good, sum(revenue_per_good))

    final_profit_per_good <- append(final_profit_per_good, profit_per_good)
    final_profit_per_good <- append(final_profit_per_good, sum(profit_per_good))

    final_max_good_sale_per_day <- append(final_max_good_sale_per_day, max_good_sale_per_day)
    final_max_good_sale_per_day <- append(final_max_good_sale_per_day, '')

    final_max_good_sale_day <- append(final_max_good_sale_day, c(max_good_sale_day, ''))

    final_min_good_sale_per_day <- append(final_min_good_sale_per_day, min_good_sale_per_day)
    final_min_good_sale_per_day <- append(final_min_good_sale_per_day, '')

    final_min_good_sale_day <- append(final_min_good_sale_day, c(min_good_sale_day, ''))
    final_sales <- append(final_sales, c(sales, sum(sales)))
    final_supply <- append(final_supply, c(supply, sum(supply)))
    final_write_downs <- append(final_write_downs, c(write_downs, sum(write_downs)))
    final_uniformity <- append(final_uniformity, c(uniformity, ''))
  }

  names_v <- vector()
  goods_v <- vector()


  for (i in 1:length(out_)) {
    names_v <- append(names_v, c(paste0('Магазин', i), rep('', length(as.data.frame(out_[i])) - 1)))
    goods_v <- append(goods_v, colnames(as.data.frame(out_[i]))[-(1:1)])
    goods_v <- append(goods_v, 'Total')
  }

  # Debug
  print(length(names_v))
  print(length(goods_v))
  print(length(final_revenue_per_good))
  print(length(final_profit_per_good))
  print(length(final_max_good_sale_per_day))
  print(length(final_max_good_sale_day))
  print(length(final_min_good_sale_per_day))
  print(length(final_min_good_sale_day))
  print(length(final_sales))
  print(length(final_supply))
  print(length(final_write_downs))
  print(length(final_uniformity))

  res.tab <- data.frame(Name = names_v, Good = goods_v, Revenue = final_revenue_per_good, Profit = final_profit_per_good,
                        Supply = final_supply, Sales = final_sales, Uniformity = final_uniformity, Wrte_Downs = final_write_downs,
                        Max_Sale_Per_Day = final_max_good_sale_per_day, Max_Sale_Day = final_max_good_sale_day,
                        Min_Sale_Per_Day = final_min_good_sale_per_day, Min_Sale_Day = final_min_good_sale_day)
  write.table(res.tab, 'C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Result/result.csv', sep = ';', row.names=FALSE)

}








{
  library(ggplot2)
  library(dplyr)
  graph1 <- function(in_, out_, shop, type='sales', goods_price=c(240, 454, 190, 550), suppliers_price=c(100, 130, 120, 115), recovery_price=c(20, 30, 15, 10)) {
    ID1 <- 1:length(as.data.frame(out_[shop])[, 1])

    days <- vector()
    goods_in <- vector()
    data <- vector()

    if (type == 'sales') {
      for (i in 2:length(as.data.frame(out_[shop]))) {
        days <- append(days, as.data.frame(out_[shop])[, 1])
        #goods_in <- append(goods_in, rep(goods[i - 1], length(as.data.frame(out_[shop])[, 1])))
        goods_in <- append(goods_in, rep(colnames(as.data.frame(out_[shop]))[i], length(as.data.frame(out_[shop])[, 1])))
        data <- append(data, as.data.frame(out_[shop])[, i])
      }
      title_ <- "Объем продаж по всем товарам по 1 магазину"
      y_ <- "Продажи"
    }

    if (type == 'profit') {
      for (i in 2:length(as.data.frame(out_[shop]))) {
        days <- append(days, as.data.frame(out_[shop])[, 1])
        goods_in <- append(goods_in, rep(colnames(as.data.frame(out_[shop]))[i], length(as.data.frame(out_[shop])[, 1])))
        data <- append(data, as.data.frame(out_[shop])[, i] * goods_price[i - 1] - as.data.frame(in_[shop])[, i] * suppliers_price[i - 1] - (as.data.frame(in_[shop])[, i] - as.data.frame(out_[shop])[, i]) * recovery_price[i - 1])
      }
      title_ <- "Прибыль по всем товарам по 1 магазину"
      y_ <- "Прибыль"
    }

    if (type == 'revenue') {
      for (i in 2:length(as.data.frame(out_[shop]))) {
        days <- append(days, as.data.frame(out_[shop])[, 1])
        goods_in <- append(goods_in, rep(colnames(as.data.frame(out_[shop]))[i], length(as.data.frame(out_[shop])[, 1])))
        data <- append(data, as.data.frame(out_[shop])[, i] * goods_price[i - 1])
      }
      title_ <- "Выручка по всем товарам по 1 магазину"
      y_ <- "Выручка"
    }

    if (type == 'write-downs') {
      for (i in 2:length(as.data.frame(out_[shop]))) {
        days <- append(days, as.data.frame(out_[shop])[, 1])
        goods_in <- append(goods_in, rep(colnames(as.data.frame(out_[shop]))[i], length(as.data.frame(out_[shop])[, 1])))
        data <- append(data, as.data.frame(in_[shop])[, i] - as.data.frame(out_[shop])[, i])
      }
      title_ <- "Списание по всем товарам по 1 магазину"
      y_ <- "Списание"
    }

    if (type == 'profitability') {
      for (i in 2:length(as.data.frame(out_[shop]))) {
        days <- append(days, as.data.frame(out_[shop])[, 1])
        goods_in <- append(goods_in, rep(colnames(as.data.frame(out_[shop]))[i], length(as.data.frame(out_[shop])[, 1])))
        data <- append(data, as.data.frame(in_[shop])[, i] - as.data.frame(out_[shop])[, i])
      }
      title_ <- "Рентабельность по всем товарам по 1 магазину"
      y_ <- "Рентабельность"
    }

    file_name <- paste0(type, '.png')

    data <- data.frame(days = days, goods_in = goods_in, data = data)
    print(data)

    data %>%
      ggplot(aes(x = days, y = data, color = goods_in)) +
      geom_line() + # отвечает за линии на графике
      geom_point() + # отвечает за точки на графике
      labs(title = title_,
           x = 'День',
           y = y_,
           color = 'Продукты') +
      scale_x_continuous("День", labels = as.character(ID1), breaks = ID1) #показывать все значения по оси x каждый
    ggsave(paste0("C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Plots/", file_name))
  }
  graph1(in_, out_, 2, type = 'write-downs')
}



{
  library(ggplot2)
  library(dplyr)
  graph2 <- function(out_, good, good_id) {
    shop_numbers <- 1:length(out_)
    goods <- rep(good[good_id], length(shop_numbers))
    sales_per_good <- vector()


    for (i in 1:length(shop_numbers)) {
      sales_per_good <- append(sales_per_good, sum(as.data.frame(out_[i])[, good_id + 1]))
    }

    data <- data.frame(shops = shop_numbers, goods = goods, sales = sales_per_good)

    data %>%
      ggplot(aes(x = shops, y = sales, fill = as.character(shops))) +
      geom_bar(stat='identity') +
      labs(title = "Объемы продаж 1 товара по всем магазинам",
           fill = "Магазины",
           x = "Магазины",
           y = "Объём продаж") +
      scale_x_continuous("Магазины", labels = as.character(shop_numbers), breaks = shop_numbers) #показывать все значения по оси x
    ggsave("C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Plots/plot2.png")
  }
  graph2(out_, goods, 1)
}



{
  library(ggplot2)
  library(dplyr)
  graph3 <- function(out_, goods) {
    shops <- 1:2
    days_index <- 1:length(as.data.frame(out_[1])[, 1])
    days <- vector()
    goods_names <- vector()
    sales <- vector()
    shops_index <- vector()

    for (i in 1:length(as.data.frame(out_[1])[, 1])) {

      for (good_id in 2:length(as.data.frame(out_[1]))) {
        #print(as.data.frame(out_[1]))
        for (shop in 1:length(shops)) {

          days <- append(days, i)
          goods_names <- append(goods_names, goods[good_id - 1])
          sales <- append(sales, as.data.frame(out_[shop])[, good_id][i])
          shops_index <- append(shops_index, shop)
        }
      }
    }


    data <- data.frame(days = days, goods_names = goods_names, sales = sales, shops = shops_index)
    print(data)

    data %>%
      ggplot(aes(x = days, y = goods_names, group = interaction(shops, goods_names), color = as.character(shops) )) +
      geom_line() + # отвечает за линии на графике
      geom_point(aes(shape = goods_names)) +
      scale_shape_manual(values = c(15, 16, 17)) +
      labs(title = "Динамика продаж товаров по магазинам",
           x = "День",
           y = "Объем продаж",
           color = "Магазин",
           shape = "Товар") +
      scale_x_continuous("День", labels = as.character(days_index), breaks = days_index) #показывать все значения по оси x каждый
    ggsave("C:/Users/DateBack/Desktop/R/R/Задания/КР/SuperShop/Plots/plot3.png")
  }
  graph3(out_, goods)
}