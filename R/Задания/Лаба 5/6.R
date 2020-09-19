{
  start <- readline("Enter start > ")
  start <- as.numeric(start)

  end <- readline("Enter end > ")
  end <- as.numeric(end)

  if (start > end) {
    bufferr <- start
    start <- end
    end <- bufferr
  }
}
{
  if (start %% 3 != 0) {
    start <- start + (3 - (start %% 3))
  }

  c <- seq(start, end, by=3)
  c <- sort(c, decreasing=TRUE)
  print(c)
}
