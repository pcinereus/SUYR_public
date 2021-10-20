## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(glmmTMB)
library(nlme)
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(DHARMa)    #for residual diagnostics plots
library(modelr)    #for auxillary modelling functions
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
reed = read_csv('../data/reed.csv', trim_ws=TRUE)
glimpse(reed)


## ----name, results='markdown', eval=FALSE, hidden=TRUE------------------------
## ggplot(reed, aes(y=Moorhen.Kauai, x= Year)) + geom_point()
## ggplot(reed, aes(y=Moorhen.Kauai, x= Rainfall)) + geom_point()
## ggplot(reed, aes(y=Moorhen.Kauai,x=1)) + geom_boxplot()
## ggplot(reed, aes(y=Rainfall,x=1)) + geom_boxplot()
## reed = reed %>% filter(!is.na(Moorhen.Kauai), !is.na(Rainfall))
## 
## reed.glm <- glm(Moorhen.Kauai ~ log(Rainfall) + Year,  data=reed)
## autoplot(reed.glm)
## performance::check_model(reed.glm)
## acf(resid(reed.glm,  type='pearson'))
## reed.resid <- simulateResiduals(reed.glm,  plot=TRUE)
## testTemporalAutocorrelation(reed.resid,  time=reed$Year)
## 
## plot(allEffects(reed.glm))
## 
## reed = reed %>% mutate(unit=factor(rep(1, n())),
##                        Yr=Year-min(Year))
## 
## reed.glmmTMB <- glmmTMB(Moorhen.Kauai ~ log(Rainfall) + Yr +
##                           ar1(-1+Yr|unit),
##                         data=reed, family=nbinom2(),
##                         control = glmmTMBControl(optimizer=optim,
##                                                  optArgs = list(method='BFGS')))
##                         #REML=TRUE)
## reed.glmmTMB <- glmmTMB(Moorhen.Kauai ~ log(Rainfall) + Year, #+
##                           ar1(-1+Year|unit),
##                         data=reed, family=poisson())
##                         REML=TRUE)
## acf(resid(reed.glmmTMB, type='response'))
## preds <- predict(reed.glmmTMB,  type='response')
## sims <- simulate(reed.glmmTMB,  n=250)
## reed.resid <- createDHARMa(simulatedResponse = as.matrix(sims),
##                            observedResponse = reed$Moorhen.Kauai,
##                            fittedPredictedResponse = preds,
##                            integerResponse = TRUE)
## plot(reed.resid)
## #https://cran.r-project.org/web/packages/itsadug/vignettes/acf.html
## testTemporalAutocorrelation(reed.resid,  time=reed$Year)
## testTemporalAutocorrelation(reed.resid,  time=reed$Year)
## 
## ggemmeans(reed.glmmTMB, ~Year)
## ggpredict(reed.glmmTMB, terms='Year') %>% plot
## plot(allEffects(reed.glmmTMB))
## 
## emmeans(reed.glmmTMB,  ~Year,  at=list(Year=modelr::seq_range(reed$Year, 100)))
## 
## 
## reed.lm =lm(log(Moorhen.Kauai) ~ log(Rainfall), data=reed)
## acf(resid(reed.lm), lag=40)
## 
## reed.lm =lm(log(Moorhen.Kauai) ~ log(Rainfall) + Year, data=reed)
## acf(resid(reed.lm), lag=40)
## 
## 
## reed.gls = gls(log(Moorhen.Kauai) ~ log(Rainfall) + Year, data=reed, na.action=na.omit)
## plot(reed.gls)
## #autoplot(reed.gls)
## 
## acf(resid(reed.gls, type='normalized'),lag=40)
## reed.gls1 = update(reed.gls,correlation=corAR1(form=~Year))
## plot(reed.gls1)
## acf(resid(reed.gls1, type='normalized'),lag=40)
## summary(reed.gls1)
## 
## reed.glm = glmmTMB(Moorhen.Kauai ~ log(Rainfall),  data=reed,  family=nbinom2())
## acf(resid(reed.glm), lag=40)
## 
## reed = reed %>% mutate(unit=factor(rep(1, n())))
## reed.glmm1 = glmmTMB(Moorhen.Kauai ~ log(Rainfall) + ar1(0+Year|unit), data=reed, family=nbinom2(),
##                      control = glmmTMBControl(optCtrl = list(iter.max = 3000, eval.max = 400)))
## acf(resid(reed.glmm1, type='response'), lag=40)
## #https://cran.r-project.org/web/packages/glmmTMB/vignettes/covstruct.html
## #summary(reed.glmm1)

