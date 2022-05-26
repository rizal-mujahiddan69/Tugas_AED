library(ggplot2)
str(midwest)
df <- midwest[c('percwhite', 'percblack', 'percamerindan',
                'percasian', 'percother')]
df <- as.data.frame(df)
head(df)

# Pair Plot ####
plot(df)
pairs(df)

pairs(df, col = "orange", pch = 16)
plot(df, col = ifelse(midwest$inmetro == 1, "blue", "red"), cex = .8)

library(ggplot2)
library(GGally)
ggpairs(df)

ggpairs(df, mapping = aes(color = as.factor(midwest$inmetro)))

# Radar Chart ####
library(fmsb)
minVec <- rep(0, 5)
maxVec <- rep(100, 5)

radarDF <- rbind(maxVec, minVec, df[c(1), ])
radarDF


radarchart(radarDF)
radarchart(radarDF, pcol = "red")
radarchart(radarDF, pfcol = rgb(1, 0, 0, alpha = .25))
radarchart(radarDF, pfcol = "#aabbcc")
radarchart(radarDF, cglcol = "orange")

# axis label
radarchart(radarDF, axistype = 1)
radarchart(radarDF, axistype = 2)
radarchart(radarDF, axistype = 3)
radarchart(radarDF, axistype = 4)
radarchart(radarDF, axistype = 5)

radarchart(radarDF, axistype = 1,
           caxislabels = c("0", "25", "50", "75", "100 (%)"),
           axislabcol = "coral", calcex = .8)


radarDF2 <- rbind(maxVec, minVec, df[c(1, 2), ])

par(mfrow = c(2, 1))
radarchart(radarDF2, pcol = c("red", "blue"), plwd = c(3, 3))
plot.new()
legend("center", legend = c("County 1", "County 2"), col = c("red", "blue"),
       pch = 16, lty = 1)
par(mfrow = c(1, 1))

str(midwest)
df2 <- cbind(df, midwest$percpovertyknown, midwest$percbelowpoverty)
head(df2)

minVec <- rep(0, 7)
maxVec <- rep(100, 7)

radarchart(rbind(maxVec, minVec, df2[c(1, 2), ]))
radarchart(df2[c(1, 2, 3), ], maxmin = F)


#remove.packages('rlang')
#install.packages('rlang')
#install.packages('devtools')
#devtools::install_github("ricardo-bion/ggradar")
library(ggplot2)
library(ggradar)
ggradar(df)