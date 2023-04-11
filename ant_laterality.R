library(readxl)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(Rmisc)
library(lmtest)
theme_set(theme_pubr())

data <- read_xlsx("ant_final.xlsx")

## remove outliers (for efficiency)
data_int <- data[-16,]
data1 <- data_int[-48,] 

## change of constraints for global mean and inspection
options(contrasts=c("contr.sum","contr.sum"))

## efficiency ~ ant size
plot1 <- ggplot(data1, aes(x = ant_size, y = efficiency_for_each_ant)) + 
  geom_point() + 
  stat_smooth(method = lm)
plot1
## there is a positive trend

## check correlation between efficiency and ant size
cor(data1$efficiency_for_each_ant, data1$ant_size)

## build linear model based on efficiency ~ ant size
model1 <- lm(efficiency_for_each_ant ~ ant_size, data = data1)
anova(model1)
summary(model1)
## model does not appear to be a particularly good fit -> low adj.R^2
## probably due to the outliers (remove?)

    ## inspection of the residuals
    par(mfrow=c(2,2)) 
    plot(model1)
    shapiro.test(residuals(model1)) ## NOT NORMAL (with outliers) 
    bptest(model1)
    dwtest(model1)

## take the models residuals to generate an effciency measure adjusted for body size
model1_res <- resid(model1)
model1_res <- data.frame(model1_res)
colnames(model1_res) <- "residuals"
data1 <- cbind(model1_res, data1) 

## new efficiency ~ laterality
plot2 <- ggplot(data1, aes(x = laterality, y = residuals)) +
  geom_jitter(height = 0.008, width = 0.008) + 
  stat_smooth(method = lm)
plot2

## build linear model based on new efficiency ~ laterality
model2 <- lm(residuals ~ laterality, data = data1)
anova(model2)
summary(model2)

    ## inspection of the residuals
    par(mfrow=c(2,2)) 
    plot(model2)
    shapiro.test(residuals(model2))
    bptest(model2)
    dwtest(model2)

## new efficiency ~ laterality + ant size
plot3 <- ggplot(data1, aes(x = laterality, y = residuals, col = ant_size)) +
  geom_jitter(height = 0.008, width = 0.008) + 
  stat_smooth(method = lm)
plot3
    
## build linear model based on efficiency ~ laterality + ant size
model3 <- lm(efficiency_for_each_ant ~ laterality + ant_size, data = data1)
anova(model3)
summary(model3)

    ## inspection of the residuals
    par(mfrow=c(2,2)) 
    plot(model3)
    shapiro.test(residuals(model3))
    bptest(model3)
    dwtest(model3)

## build linear model based on efficiency ~ laterality + ant size + interaction
model4 <- lm(efficiency_for_each_ant ~ laterality + ant_size + laterality*ant_size, data = data1)
anova(model4)
summary(model4)

    ## inspection of the residuals
    par(mfrow=c(2,2)) 
    plot(model4)
    shapiro.test(residuals(model4))
    bptest(model4)
    dwtest(model4)





