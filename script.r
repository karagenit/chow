#!/usr/bin/env Rscript

linearModel <- function(dataX, dataY) {
    lsrl = lm(dataY ~ dataX)

    x11()
    plot(dataX, dataY, col = "blue")
    abline(lsrl, col = "green")

    # Residuals Plot
    #x11()
    #plot(dataX, lsrl$residuals)

    #cat("Pearson's R:", cor(dataX, dataY))
    #print(lsrl)
    #cat("SSR:", sum(resid(lsrl) ^ 2))
    #cat("\n========================\n")

    return(lsrl)
}

data4cim = read.csv("data/4cim.csv", header = TRUE)
data6cim = read.csv("data/6cim.csv", header = TRUE)
dataCombinedVolts = c(data4cim$VOLT, data6cim$VOLT)
dataCombinedAmps  = c(data4cim$TOTAL.AMP, data6cim$TOTAL.AMP)

lm4cim = linearModel(data4cim$TOTAL.AMP, data4cim$VOLT)
lm6cim = linearModel(data6cim$TOTAL.AMP, data6cim$VOLT)
lmCombined = linearModel(dataCombinedAmps, dataCombinedVolts)

ssr4cim = sum(resid(lm4cim) ^ 2)
ssr6cim = sum(resid(lm6cim) ^ 2)
ssrCombo = sum(resid(lmCombined) ^ 2)

k = 3
n4cim = length(data4cim$VOLT)
n6cim = length(data6cim$VOLT)
df = n4cim + n6cim - (2 * k)

fstat = ((ssrCombo - (ssr4cim + ssr6cim)) / k) / ((ssr4cim + ssr6cim) / (df))
pval = pf(fstat, k, df)

cat("SSR 4CIM:", ssr4cim, "\n")
cat("SSR 6CIM:", ssr6cim, "\n")
cat("SSR Combo:", ssrCombo, "\n")
cat("k:", k, "\n")
cat("df:", df, "\n")
cat("F:", fstat, "\n")
cat("P-Value:", pval, "\n")

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
