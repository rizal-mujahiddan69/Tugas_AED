library(dplyr)
library(ggplot2)
library(rcompanion)
library(MASS)

data_Life_Expect <- read.csv("Life Expectancy Data.csv")

# Handling Missing Value
kolom_NA <- colSums(is.na(data_Life_Expect)) / dim(data_Life_Expect)[1] * 100
kolom_NA_delete <- names(which(kolom_NA >= 10))
kolom_NA_imputation <- names(which((kolom_NA > 0) & (kolom_NA <  10)))


## delete Many column because missing value > 10%
data_Life_Expect <- data_Life_Expect %>% select(-c(kolom_NA_delete))


kolom_NA_numeric <- names(which(lapply(data_Life_Expect %>% select(kolom_NA_imputation) ,
                           class) == "numeric" ))

kolom_NA_integer <- names(which(lapply(data_Life_Expect %>% select(kolom_NA_imputation) ,
                                       class) == "integer" ))

## mean Imputation
for(j in kolom_NA_numeric){
  data_Life_Expect[[j]][is.na(data_Life_Expect[[j]])] <- mean(data_Life_Expect[[j]],
                                                              na.rm=T)
}

for(j in kolom_NA_integer){
  data_Life_Expect[[j]][is.na(data_Life_Expect[[j]])] <- as.integer(mean(data_Life_Expect[[j]],
                                                              na.rm=T))
}

# Setting datatype numeric itself
data_Life_Expect <- data_Life_Expect[names(which(lapply(data_Life_Expect
                                                                ,class)=="numeric"))]


lin_reg_expect <-lm("Life.expectancy ~ Schooling",data_Life_Expect)
summary(lin_reg_expect)

ggplot(data_Life_Expect,aes(x=Schooling,y=Life.expectancy)) +
  geom_point() + ggtitle("Relationship Schooling with Life.expectancy") +
  geom_smooth(method="lm") 

par(mfrow = c(2, 2))
plot(lin_reg_expect)

sisaan <- resid(lin_reg_expect)
par(mfrow=c(1,1))
plot(sisaan,type="l")

ggplot(data_Life_Expect,aes(x=Schooling)) + geom_density()
ggplot(data_Life_Expect,aes(x=Life.expectancy)) + geom_density()

data_Life_Expect_cp = data_Life_Expect
lambda_tukey_Life <- transformTukey(data_Life_Expect_cp$Life.expectancy,
                                                      returnLambda = TRUE)

lambda_tukey_Life <- unname(lambda_tukey_Life)

lambda_tukey_Schooling <- transformTukey(data_Life_Expect_cp$Schooling,
                                         returnLambda = TRUE)

lambda_tukey_Schooling <- unname(lambda_tukey_Schooling)

lin_reg_expect_tukey <-lm(I(Life.expectancy^(lambda_tukey_Life)) ~ I(Schooling^(lambda_tukey_Schooling)),
                          data_Life_Expect_cp)

#find optimal lambda for Box-Cox transformation 
bc <- boxcox(lin_reg_expect)
lambda <- bc$x[which.max(bc$y)]

# lin_reg_expect_boxcox <- lm(((Life.expectancy^lambda-1)/lambda) ~ Schooling,
#                             data_Life_Expect)

lin_reg_expect_boxcox <- lm(((Life.expectancy^lambda)) ~ Schooling,
                            data_Life_Expect)

# Multiple R-squared:  0.5281,	Adjusted R-squared:  0.5279
summary(lin_reg_expect_boxcox)

# Multiple R-squared:  0.571,	Adjusted R-squared:  0.5709 
summary(lin_reg_expect_tukey)

# Multiple R-squared:  0.5113,	Adjusted R-squared:  0.5112 
summary(lin_reg_expect)

ggplot(data_Life_Expect_cp,aes(x=Schooling^(lambda_tukey_Schooling),
                               y=Life.expectancy^(lambda_tukey_Life))) +
  geom_point() + geom_smooth(method="lm") + ggtitle("linear regression with Tukey")

ggplot(data_Life_Expect,aes(x=Schooling,y=Life.expectancy^(lambda))) +
  geom_point() + geom_smooth(method="lm") + ggtitle("linear regression with Box Cox (lambda = 2)")

ggplot(data_Life_Expect,aes(x=Schooling,y=Life.expectancy)) +
  geom_point() + geom_smooth(method="lm") + ggtitle("linear regression Original")


data_Life_Expect_samp <- data_Life_Expect[1:10,]
ggplot(data_Life_Expect_samp,aes(x=Schooling,y=Life.expectancy)) +
  geom_point() + geom_smooth(method="lm") +
  ggtitle("linear regression Original Sampling")

lin_reg_expect_samp <- lm("Life.expectancy ~ Schooling",
                          data_Life_Expect_samp)

summary(lin_reg_expect_samp)

ggplot(data_Life_Expect_samp,aes(x=Schooling,y=Life.expectancy)) +
  geom_point() +
  geom_smooth(method="loess") + 
  ggtitle("Loess Dengan ggplot pada 10 amatan")


# par(mfrow=c(2,2))
# plot(lin_reg_expect_samp)