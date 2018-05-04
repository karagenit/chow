#!/usr/bin/env Rscript

linearModel <- function(dataset) {
    lsrl = lm(dataset$VOLT ~ dataset$TOTAL.AMP)

    x11()
    plot(dataset$TOTAL.AMP, dataset$VOLT, col = "blue")
    abline(lsrl, col = "green")

    return(lsrl)
}

data4cim = read.csv("data/4cim.csv", header = TRUE)
data6cim = read.csv("data/6cim.csv", header = TRUE)

linearModel(data4cim)
linearModel(data6cim)

# Residuals Plot
#x11()
#plot(data4cim$TOTAL.AMP, lsrl$residuals)

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
