portfolio <- list()
portfolio[[1]] <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=AAPL&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)
portfolio[[2]] <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=MSFT&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)
portfolio[[3]] <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GOOG&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)
portfolio[[4]] <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=AMZN&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)
portfolio[[5]] <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=SBUX&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)

AEE <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=AEE&a=00&b=1&c=2000&d=03&e=1&f=2013&g=m&ignore=.csv", stringsAsFactors = FALSE)
write.csv(AEE, "AEE.csv")

BestPrediction = vector()
for(i in 1:length(portfolio))
{
  cat("\n*** predicting stock number: ",i) ;
  BestPrediction[i] = findBestPrediction(portfolio[[i]])
  
}
cat("\n\n!!! winning models:", BestPrediction)
