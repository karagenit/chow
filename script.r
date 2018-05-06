#!/usr/bin/env Rscript

removeOutliers <- function(dataset) {
    datasetRemoved = list()

    lsrl = lm(dataset$y ~ dataset$x)

    # Find Residual Statistics
    resMean = mean(lsrl$residuals)
    resSD = sd(lsrl$residuals)
    resZ = (lsrl$residuals - resMean) / resSD

    # Keep Only Points where Residual Z-Score < 3
    datasetRemoved$x = dataset$x[abs(resZ) < 3]
    datasetRemoved$y = dataset$y[abs(resZ) < 3]

    return(datasetRemoved)
}

getOutliers <- function(dataset) {
    datasetOutliers = list()

    lsrl = lm(dataset$y ~ dataset$x)

    # Find Residual Statistics
    resMean = mean(lsrl$residuals)
    resSD = sd(lsrl$residuals)
    resZ = (lsrl$residuals - resMean) / resSD

    # Keep Only Points where Residual Z-Score > 3
    datasetOutliers$x = dataset$x[abs(resZ) > 3]
    datasetOutliers$y = dataset$y[abs(resZ) > 3]

    return(datasetOutliers)
}

# Datasets
dataA = list()
dataB = list()
dataAB = list()

# Read Files
dataAfile = read.csv("data/4cim.csv", header = TRUE)
dataBfile = read.csv("data/4minicim.csv", header = TRUE)

# Assign to Datasets
dataA$x = dataAfile$TOTAL.AMP
dataA$y = dataAfile$VOLT
dataB$x = dataBfile$TOTAL.AMP
dataB$y = dataBfile$VOLT

# Filter Outliers (optional)
dataA = removeOutliers(dataA)
dataB = removeOutliers(dataB)

# Only keep every 5th element (optional)
#dataAamps = dataAamps[seq(1, length(dataAamps), 5)]
#dataAvolts = dataAvolts[seq(1, length(dataAvolts), 5)]
#dataBamps = dataBamps[seq(1, length(dataBamps), 5)]
#dataBvolts = dataBvolts[seq(1, length(dataBvolts), 5)]

# Find combined data set
dataAB$x = c(dataA$x, dataB$x)
dataAB$y = c(dataA$y, dataB$y)

# TODO correlation coefficient for each dataset?
# TODO display LSRL for each set?
# TODO keep original dataset, plot outliers

# Plot Datasets (optional)
x11()
plot(dataA$x, dataA$y, main="Motor Comparison, Amps vs. Volts", xlab="Amps", ylab="Volts", col="blue")
points(dataB$x, dataB$y, col="green")
abline(lm(dataA$y ~ dataA$x), col="blue")
abline(lm(dataB$y ~ dataB$x), col="green")
abline(lm(dataAB$y ~ dataAB$x), col="black")

# TODO avoid recomputing Linear Models, store in dataA$lm? Also store dataA$residuals? (Or just dataA$lm$residuals)

# Find SSRs
dataA$ssr = sum(resid(lm(dataA$y ~ dataA$x)) ^ 2)
dataB$ssr = sum(resid(lm(dataB$y ~ dataB$x)) ^ 2)
dataAB$ssr = sum(resid(lm(dataAB$y ~ dataAB$x)) ^ 2)

# Do Chow Test to find F statistic
k = 3
df = length(dataA$x) + length(dataB$x) - (2 * k)

fstat = ((dataAB$ssr - (dataA$ssr + dataB$ssr)) / k) / ((dataA$ssr + dataB$ssr) / (df))
pval = pf(fstat, k, df)

# Print Info
cat("SSR A:", dataA$ssr, "\n")
cat("SSR B:", dataB$ssr, "\n")
cat("SSR AB:", dataAB$ssr, "\n")
cat("k:", k, "\n")
cat("df:", df, "\n")
cat("F:", fstat, "\n")
cat("P-Value:", pval, "\n")

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
