library(ggplot2)
library(dplyr)
setwd("C:/Users/ilyak/Documents/Study/R/KR7")
getwd()

graph1 <- function() {
  ID1 <- c(1:7)
  input_data1 <- read.csv("task1.csv", header=TRUE, sep=";")
  
  input_data1 %>%
    ggplot(aes(x = day, y = average_sales, color = product)) +
    geom_line() + # отвечает за линии на графике
    geom_point() + # отвечает за точки на графике
    labs(title = "Продажи товаров за неделю",
         subtitle="В какой день необходимо увеличить поставки?",
         x = 'День',
         y = 'Продажи',
         color = 'Продукты') +
    scale_x_continuous("День", labels = as.character(ID1), breaks = ID1) #показывать все значения по оси x каждый
  ggsave('C:/Users/ilyak/Documents/Study/R/KR7/result_task1.png')
}

graph2 <- function() {
  input_data2 <- read.csv("task2.csv", header=TRUE, sep=";")
  ID2 <- c(1:10)
  input_data2 %>%
    ggplot(aes(x = shops, y = amount_products, fill = as.character(shops))) +
    geom_bar(stat='identity') +
    labs(title = "Продажи магазинов за день по одному товару",
         subtitle = "Какой магазин приносит наибольшую прибыль?",
         fill = "Магазины",
         x = "Магазины",
         y = "Объём продаж") +
    scale_x_continuous("Магазины", labels = as.character(ID2), breaks = ID2) #показывать все значения по оси x
  ggsave('C:/Users/ilyak/Documents/Study/R/KR7/result_task2.png')
}

graph3 <- function() {
  ID3 <- c(1:7)
  input_data3 <- read.csv("task3.csv", header=TRUE, sep=";")
  
  input_data3 %>%
    ggplot(aes(x = day, y = average_sales, group = interaction(shops, product), color = as.character(shops) )) +
    geom_line() + # отвечает за линии на графике
    geom_point(aes(shape = product)) +
    scale_shape_manual(values = c(15, 16)) +
    labs(title = "Динамика продаж товаров по магазинам",
         x = "День",
         y = "Объем продаж",
         color = "Магазин",
         shape = "Товар") +
    scale_x_continuous("День", labels = as.character(ID3), breaks = ID3) #показывать все значения по оси x каждый
  ggsave('C:/Users/ilyak/Documents/Study/R/KR7/result_task3.png') 
}

graph1()
graph2()
graph3()