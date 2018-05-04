#!/usr/bin/env Rscript

linearModel <- function(dataX, dataY) {
    lsrl = lm(dataY ~ dataX)

    x11()
    plot(dataX, dataY, col = "blue")
    abline(lsrl, col = "green")

    return(lsrl)
}

data4cim = read.csv("data/4cim.csv", header = TRUE)
data6cim = read.csv("data/6cim.csv", header = TRUE)
dataCombinedVolts = c(data4cim$VOLT, data6cim$VOLT)
dataCombinedAmps  = c(data4cim$TOTAL.AMP, data6cim$TOTAL.AMP)

linearModel(data4cim$TOTAL.AMP, data4cim$VOLT)
linearModel(data6cim$TOTAL.AMP, data6cim$VOLT)
linearModel(dataCombinedAmps, dataCombinedVolts)

# Residuals Plot
#x11()
#plot(data4cim$TOTAL.AMP, lsrl$residuals)

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
