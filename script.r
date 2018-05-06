#!/usr/bin/env Rscript

removeOutliers <- function(dataset) {
    datasetRemoved = list()

    lsrl = lm(dataset$y ~ dataset$x)

    # Find Residual Statistics
    resMean = mean(lsrl$residuals)
    resSD = sd(lsrl$residuals)
    resZ = (lsrl$residuals - resMean) / resSD

    # Keep Only Points where Residual Z-Score < 3
    datasetRemoved$x = dataset$x[abs(resZ) < 1.25]
    datasetRemoved$y = dataset$y[abs(resZ) < 1.25]

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

removeDuplicates <- function(dataset) {
    datasetRemoved = list()
    datasetRemoved$x = dataset$x[c(TRUE, diff(dataset$x) != 0)]
    datasetRemoved$y = dataset$y[c(TRUE, diff(dataset$x) != 0)]
    return(datasetRemoved)
}

# Datasets
dataA = list()
dataB = list()
dataAB = list()

# Read Files
# NOTE: we can't just readline() b/c running this script w/ Rscript is "non-interactive"...
cat("First File to Read: ")
fileA = readLines(file("stdin"), 1)
dataAfile = read.csv(paste("data/", fileA, ".csv", sep=""), header = TRUE)
cat("Second File to Read: ")
fileB = readLines(file("stdin"), 1)
dataBfile = read.csv(paste("data/", fileB, ".csv", sep=""), header = TRUE)
closeAllConnections()

# Assign to Datasets
dataA$x = dataAfile$TOTAL.AMP
dataA$y = dataAfile$VOLT
dataB$x = dataBfile$TOTAL.AMP
dataB$y = dataBfile$VOLT

# Filter Outliers (optional)
dataA = removeOutliers(dataA)
dataB = removeOutliers(dataB)

# Remove Duplicate Current Readings (optional)
dataA = removeDuplicates(dataA)
dataB = removeDuplicates(dataB)

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
k = 4
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

# Basically a bunch of hacks
cat("Press [Enter] to Quit...")
enter = readLines(file("stdin"), 1)
closeAllConnections()
