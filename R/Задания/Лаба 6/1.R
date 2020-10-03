{
  pow_c <- function (x, y, z) {
    if (z != 0) {
      if (is.logical(z) | is.character(z) | is.logical(y) | is.character(y) |
      is.logical(x) | is.character(x)) {
        return('Один из параметров не число')
      } else {
        return((x^y)/z)
      }

    } else {
      return('ДЕЛЕНИЕ НА НОЛЬ')
    }

  }

  b <- pow_c(4, 'ttt', 2)
  b
}