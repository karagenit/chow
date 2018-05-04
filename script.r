#!/usr/bin/env Rscript

data4cim = read.csv("data/4cim.csv", header = TRUE)

print(data4cim$VOLT)
print(data4cim$TOTAL.AMP)
correl = cor(data4cim$TOTAL.AMP, data4cim$VOLT)
lsrl = lm(data4cim$VOLT ~ data4cim$TOTAL.AMP)
print(correl)
print(lsrl)

x11()
plot(data4cim$TOTAL.AMP, data4cim$VOLT, col = "blue")
abline(lsrl, col = "green")

# Residuals Plot
#x11()
#plot(data4cim$TOTAL.AMP, lsrl$residuals)

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
