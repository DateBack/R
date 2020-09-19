{
  g <- vector()
  for (i in 1:8) {
    num <- readline("Enter number > ")
    num <- as.numeric(num)
    g <- append(g, num)
  }
  g <- sort(g, decreasing=TRUE)
  v <- vector()

  for (i in 1:8) {
    if (i == 8) {
      v <- append(v, g[i])
      break
    }
    if (g[i] > g[i+1]) {
      v <- append(v, g[i])
      v <- append(v, '>')
    }
    if (g[i] == g[i+1]) {
      v <- append(v, g[i])
      v <- append(v, '=')
    }
  }



  cat(v)
}

