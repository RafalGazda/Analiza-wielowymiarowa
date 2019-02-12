library(vars)
library(imputeTS)
library(tseries)
library(dplyr)

ger <- read.csv('C:/Users/Gazi/Desktop/Projekt AW/^dax_d.csv', sep=',')
uk <- read.csv('C:/Users/Gazi/Desktop/Projekt AW/^ukx_d.csv', sep=',')
russ <- read.csv('C:/Users/Gazi/Desktop/Projekt AW/^rts_d.csv', sep=',')
pol <- read.csv('C:/Users/Gazi/Desktop/Projekt AW/wig_d.csv', sep=',')
fr<- read.csv('C:/Users/Gazi/Desktop/Projekt AW/^cac_d.csv', sep=',')

ger <- select(ger, 'Date','Close')
uk <- select(uk, 'Date','Close')
russ <- select(russ, 'Date','Close')
pol <- select(pol, 'Date','Close')
fr <- select(fr, 'Date','Close')


data <- 0
data <- merge(ger,uk, by="Date", all=TRUE, sort = TRUE)
names(data) <- c("Date", "Germany", "UnitedKingdom")
data <- merge(data,russ, by="Date", all=TRUE, sort = TRUE)
names(data)[4] <- "Russia"
data <- merge(data,pol, by="Date", all=TRUE, sort = TRUE)
names(data)[5] <- "Poland"
data <- merge(data,fr, by="Date", all=TRUE, sort = TRUE)
names(data)[6] <- "France"

#usuniêcie wierszy z brakujacymi danymi

data <- na.omit(data)

# stopy zwrotu
icol <- ncol(data)
irow <- nrow(data)
RdataOr <- as.data.frame(matrix(ncol=icol,nrow=irow))
for (j in 2:icol)
{
  for (i in 1:irow-1)
  {
    RdataOr[i,j] <- log(data[i+1,j]/data[i,j])
  }
}
rates <- RdataOr[-2337,-1]
names(rates) <- c("Germany","UK","Russia","Poland","France")


ladf <- lapply(rates,adf.test)
ladf

VARselect(rates)

VAR <- VAR(rates,1)
VAR


serial.test(VAR)

causality(VAR,"Germany")
causality(VAR,"UK")
causality(VAR,"Russia")
causality(VAR,"Poland")
causality(VAR,"France")


grangertest(Poland~Germany, data=rates, order=2)
grangertest(Poland~UK, data=rates, order=2)
grangertest(Poland~Russia, data=rates, order=2)
grangertest(Poland~France, data=rates, order=2)


IRF <- irf(VAR)
plot(IRF)
