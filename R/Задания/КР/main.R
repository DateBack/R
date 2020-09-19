{
  setwd('SuperShop')
  setwd('Analysis')
  in1 <- read.table(in.txt, head = TRUE)
  out1 <- read.table(out.txt, head = TRUE)

  rev <- rep(0, 12)
  res.tab <- data.frame(Revenue = rev, "Profit" = profit)

  sale <-rep(0, nrow(res.tab))
  print(res.tab)

}

