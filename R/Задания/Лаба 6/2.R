{
  print_day <- function (num) {
    num <- as.integer(num)
    if (num < 1) {
      return(print(' '))
    }
    day <- if (num %% 7 != 0) num %% 7 else 7
    days <- c('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье')
    return(days[day])
  }

  b <- print_day(123)
  b
}