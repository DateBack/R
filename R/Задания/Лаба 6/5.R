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
    day <- if (num %% 12 != 0) num %% 12 else 12

    months_ru <- c('Январь', 'Февраль', 'Март', 'Апрель', 'Май', 'Июнь', 'Июль', 'Август', 'Сентябрь', 'Октябрь', 'Ноябрь', 'Декабрь')
    months_en <- c('January', 'February', 'March', 'April', 'May', 'June', 'July', 'Augost', 'September', 'October', 'November', 'December')

    months_ru_short <- c('Янв', 'Фев', 'Ма', 'Ап', 'Май', 'Июн', 'Ию', 'Авгу', 'Сентяб', 'Октяб', 'Ноя', 'Дека')
    months_en_short <- c('Jan', 'Feb', 'Ma', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Septe', 'Octo', 'Novem', 'Dec')



    days_ru <- c('Понедельник', 'Вторник', 'Среда', 'Четверг', 'Пятница', 'Суббота', 'Воскресенье')
    days_en <- c('Monday', 'Thusday', 'Wensdday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

    days_ru_short <- c('Понед', 'Втн', 'Ср', 'Чет', 'Пятн', 'Субб', 'Воскресе')
    days_en_short <- c('Mon', 'Thus', 'Wensd', 'Thursd', 'Fri', 'Sat', 'Sun')

    days <- switch(lang, ru=months_ru, en=months_en)
    size <- switch(size,
                   short={
                     days <- switch(lang, ru=months_ru_short, en=months_en_short)
                   },
                   long={
                     days <- switch(lang, ru=months_ru, en=months_en)
                   })



    return(print(days[day]))
  }

  print_day(13, 'en', 'short')

}

