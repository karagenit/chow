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

# Load CIM Data
data4cim = read.csv("data/4cim.csv", header = TRUE)
data6cim = read.csv("data/4minicim.csv", header = TRUE)

dataAamps = data4cim$TOTAL.AMP
dataAvolts = data4cim$VOLT
dataBamps = data6cim$TOTAL.AMP
dataBvolts = data6cim$VOLT

# Only keep every 5th element
dataAamps = dataAamps[seq(1, length(dataAamps), 5)]
dataAvolts = dataAvolts[seq(1, length(dataAvolts), 5)]
dataBamps = dataBamps[seq(1, length(dataBamps), 5)]
dataBvolts = dataBvolts[seq(1, length(dataBvolts), 5)]

# Find combined data set
dataCombinedVolts = c(dataAvolts, dataBvolts)
dataCombinedAmps  = c(dataAamps, dataBamps)

# Find linear models
lm4cim = linearModel(dataAamps, dataAvolts)
lm6cim = linearModel(dataBamps, dataBvolts)
lmCombined = linearModel(dataCombinedAmps, dataCombinedVolts)

# Find SSRs
ssr4cim = sum(resid(lm4cim) ^ 2)
ssr6cim = sum(resid(lm6cim) ^ 2)
ssrCombo = sum(resid(lmCombined) ^ 2)

# Do Chow Test to find F statistic
k = 3
n4cim = length(dataAvolts)
n6cim = length(dataBvolts)
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
