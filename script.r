#!/usr/bin/env Rscript

data4cim = read.csv("data/4cim.csv", header = TRUE)

print(data4cim$VOLT)
print(data4cim$TOTAL.AMP)
correl = cor(data4cim$TOTAL.AMP, data4cim$VOLT)
lsrl = lm(data4cim$TOTAL.AMP ~ data4cim$VOLT)
print(correl)
print(lsrl)

x11()
plot(data4cim$TOTAL.AMP, data4cim$VOLT, col = "blue")

Sys.sleep(1000)
#readline(prompt="Press [enter] to quit")
