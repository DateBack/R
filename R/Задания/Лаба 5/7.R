{
  num <- readline("Enter number > ")
  num <- as.numeric(num)
  years <- 2020:2040
  months <- c('Январь', 'Февраль', 'Март', 'Апрель', 'Май', 'Июнь', 'Июль', 'Август', 'Сентябрь', 'Ноябрь', 'Декабрь')
  days <- c('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье')
  num_of_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  year <- num %/% 365 + 1
  curent_days_in_month <- num %% 365
  num_of_month <- 1

  for (elem in num_of_days)
    if (curent_days_in_month > elem) {
      curent_days_in_month <- curent_days_in_month - elem
      num_of_month <- num_of_month + 1
    }

  day <- if (num %% 7 != 0) num %% 7 else 7
  message('Введенное значение ', num, ' соответсвует ', curent_days_in_month, ' ', days[day], ' ', months[num_of_month], ' ', years[year])
}

print(1033 %% 7)
