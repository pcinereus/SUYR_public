## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects) #for partial effects plots
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots


## ----readData, results='markdown', eval=TRUE----------------------------------
peake = read_csv('../data/peakquinn.csv', trim_ws=TRUE)
glimpse(peake)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(peake, aes(y=INDIV, x=AREA)) + geom_point()+
    geom_smooth()

ggplot(peake, aes(y=INDIV)) + geom_boxplot()

ggplot(peake, aes(y=AREA)) + geom_boxplot()

ggplot(peake, aes(y=INDIV, x=log(AREA))) + geom_point()+
    geom_smooth() +
    scale_y_log10()



## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
peake.lm<-lm(INDIV~AREA, data=peake)


## ----validateModel1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(peake.lm, which=1:6)
influence.measures(peake.lm)
peake.resid <- simulateResiduals(peake.lm)
plot(peake.resid)
testResiduals(peake.resid)


## ----fitMode2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.glm <- glm(INDIV ~ log(AREA), data=peake, family=poisson(link='log'))


## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(peake.glm, which=1:6)
## hmmm - some very high Cook's D values
influence.measures(peake.glm)
peake.resid <- simulateResiduals(peake.glm)
plot(peake.resid)
testResiduals(peake.resid)


## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
peake.ss <- sum(resid(peake.glm, type = "pearson")^2)
1 - pchisq(peake.ss, peake.glm$df.resid)
##Evidence of a lack of fit

#Deviance
1-pchisq(peake.glm$deviance, peake.glm$df.resid)
#Evidence of a lack of fit


## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.ss/peake.glm$df.resid
peake.glm$deviance/peake.glm$df.resid
testDispersion(peake.resid)
testZeroInflation(peake.resid)


## ----fitMode3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.glm1 <- glm.nb(INDIV ~ log(AREA), data=peake)


## ----validateModel5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(peake.glm1, which=1:6)
## hmmm - some very high Cook's D values
influence.measures(peake.glm1)
peake.resid <- simulateResiduals(peake.glm1)
plot(peake.resid)
testResiduals(peake.resid)


## ----validateModel6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
peake.ss <- sum(resid(peake.glm1, type = "pearson")^2)
1 - pchisq(peake.ss, peake.glm1$df.resid)
##Evidence of a lack of fit

#Deviance
1-pchisq(peake.glm1$deviance, peake.glm1$df.resid)
#No evidence of a lack of fit


## ----validateModel7, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.ss/peake.glm1$df.resid
peake.glm1$deviance/peake.glm1$df.resid

testDispersion(peake.resid)


## ----aicModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
AIC(peake.glm, peake.glm1)
AICc(peake.glm, peake.glm1)


## ----validateModel8, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(allEffects(peake.glm1, residuals=TRUE), type='response')

## ----validateModel9, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot_model(peake.glm1, type='eff', show.data=FALSE,  terms='AREA [exp]')


## ----validateModel9a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
#ggemmeans(peake.glm1,  ~AREA, back.transform =FALSE) %>% plot()


## ----summaryModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
summary(peake.glm1)
confint(peake.glm1)

exp(coef(peake.glm1)[2])
## For every one unit change in log(AREA) the number of individuals increases by a factor of 2.28
                                        #tidy(peake.glm1, conf.int=TRUE, exponentiate=TRUE)
tidy(peake.glm1, conf.int=TRUE)
tidy(peake.glm1, conf.int=TRUE, exponentiate=TRUE)
glance(peake.glm1)


## ----predictModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
#R2
1-(peake.ss/peake.glm1$null)

1-(peake.glm1$deviance/peake.glm1$null)

##R^2 = 1 - exp(-2/n * logL(x) - logL(0))
## and adjusted where max(R^2) = 1 - exp(2 / n * logL(0)) because
## sometimes the max R2 is less than one
r.squaredLR(peake.glm1)


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
peake.grid = with(peake, list(AREA=seq(min(AREA), max(AREA), len=100)))
peake.grid = peake %>% data_grid(AREA=seq_range(AREA,  n=100))
newdata = emmeans(peake.glm1, ~AREA, at=peake.grid, type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    theme_classic()
	
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=peake, aes(y=INDIV)) +
    scale_x_log10() +
    scale_y_log10() +
    theme_classic()	
	
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=peake, aes(y=INDIV)) +
    scale_x_log10(breaks = as.vector(c(1,2,5,10) %o% 10^(-1:4))) +
    scale_y_log10() +
    theme_classic()

partial.obs = emmeans(peake.glm1, ~AREA, at=peake, type='response') %>%
    as.data.frame %>%
    mutate(response=response+resid(peake.glm1,type='response'))

## response residuals are just resid * mu.eta(predict)
	
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=peake, aes(y=INDIV)) +
    geom_point(data=partial.obs, aes(y=response), color='green') + 
    scale_x_log10(breaks = as.vector(c(1,2,5,10) %o% 10^(-1:4))) +
    scale_y_log10() +
    theme_classic()	


