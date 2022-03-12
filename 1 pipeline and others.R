library(MASS)
df.boston <- Boston
head(Boston)

# Pipeline
library(dplyr)
x <- c(1, 3, 2)
x %>% mean();    mean(x)

x %>% quantile(c(.25, .50, .75));    quantile(x, c(.25, .50, .75))

head(
    round(
    cor(
        df.boston), 2))

df.boston %>%
    cor() %>%
    round(2) %>%
    head()

plot(quantile(df.boston$age, c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)),
     main = "Grafik", xlab = "", ylab = "age")

library(dplyr)
df.boston$age %>%
  quantile(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)) %>%
  plot(main = "Grafik", xlab = "", ylab = "age")



# Help
help(runif)
help("data.frame")
