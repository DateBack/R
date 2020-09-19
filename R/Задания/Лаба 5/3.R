{
  while (TRUE) {
    num <- readline("Enter number > ")
    if (tolower(num) == 'стоп' || tolower(num) == 'stop') {
      break
    } else {
      num <- as.numeric(num)
    }

  }
}

