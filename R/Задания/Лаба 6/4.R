{
  print_day <- function (num, lang='ru', size='short') {

    if (tolower(lang) %in% c('ru', 'rus', 'рус', 'ру')) {
      lang <- 'ru'
    } else if (tolower(lang) %in% c('eng', 'english', 'англ', 'анг')) {
      lang <- 'en'
    }


    num <- as.integer(num)
    if (num < 1) {
      return(print(' '))
    }
    day <- if (num %% 7 != 0) num %% 7 else 7


    days_ru <- c('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье')
    days_en <- c('Monday', 'Thusday', 'Wensdday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

    days_ru_short <- c('Понед', 'Втн', 'Ср', 'Чет', 'Пятн', 'Субб', 'Воскресе')
    days_en_short <- c('Mon', 'Thus', 'Wensd', 'Thursd', 'Fri', 'Sat', 'Sun')

    days <- switch(lang, ru=days_ru, en=days_en)
    size <- switch(size,
                   short={
                     days <- switch(lang, ru=days_ru_short, en=days_en_short)
                   },
                   long={
                     days <- switch(lang, ru=days_ru, en=days_en)
                   })


    #if (lang == 'ru') {
    #  days <- c('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье')
    #} else if (lang == 'en') {
    #  days <- c('Monday', 'Thusday', 'Wensdday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
    #}

    return(print(days[day]))
  }

  print_day(10, 'en', 'long')

}