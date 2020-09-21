## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(tinytex.engine = 'xelatex')


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(DHARMa)    #for residual diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for partial effects plots
library(emmeans)   #for estimating marginal means
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots


## ----readData, results='markdown', eval=TRUE----------------------------------
fert = read_csv('../data/fertilizer.csv', trim_ws=TRUE)
glimpse(fert)
head(fert)
str(fert)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(fert, aes(y=YIELD, x=FERTILIZER)) + geom_point() + geom_smooth()
ggplot(fert, aes(y=YIELD, x=FERTILIZER)) + geom_point() + geom_smooth(method='lm')
ggplot(fert, aes(y=YIELD)) + geom_boxplot(aes(x=1))


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
fert.lm<-lm(YIELD~1+FERTILIZER, data=fert)
fert.lm<-lm(YIELD~FERTILIZER, data=fert)

## ----summaryModel, results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE----
## attributes(fert.lm)
## str(fert.lm)
## fert.lm$model
## fert.lm$coefficients
## coef(fert.lm)
## fitted(fert.lm)
## resid(fert.lm)
## args(residuals.lm)
## 
## model.matrix(fert.lm)
## model.matrix(~FERTILIZER, data=fert)


## ----validateModel, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(fert.lm, which=1:6, ncol=2, label.size=3)

## ----validateModela, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
influence.measures(fert.lm)

## ----validateModelb, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
## DHARMa uses simulations to generate standardized residuals
## Diagnostics for HierArchical Regression Models
## - tests for patterns in residuals
##   - homogeneity
##   - dispersion
##   - zero inflation
##   - outliers
##   - conforming to distributions
##   - autocorrelations
## - interpretable residuals for binary and poisson data
## Steps:
## 1. simulate new data from the model for each observation
## 2. for each observation calculate CDF of simulated obs
##    - this should describe the expected  probability profile of each observation
## 3. the residual for an observation is calculated as the value of the CDF that corresponds to the actual observed value
##    - a value of 0 indicates that the observed value was less than all simulated values
##    - a value of 1 indicates that the observed value was greater than all simulated values
##    - a value of 0.5 indicates that have the observed value were greater than all simulated values
##    - from a correctly specified model,  these quantile residuls should be uniformly distributed
## This ensures that all residuals have the same interpretation irrespective of the model and distribution selected.
fert.resid <- simulateResiduals(fert.lm)
plot(fert.resid)
## KS test of the distribution
## Outliers are those with quantile residual of 0 or 1
## Outliers indicated as red stars

## To run tests of KS (uniformity),  dispersion and outliers
testResiduals(fert.resid)
## OR individually
testUniformity(fert.resid)
testDispersion(fert.resid)
testOutliers(fert.resid)
## Other useful tests
testOverdispersion(fert.resid) #same as testDispersion
testQuantiles(fert.resid)
## The above fits quantile gams at 0.25,  0.5 and 0.75
## testSpatialAutocorrelation(fert.resid,  x=,  y=) # needs x and y coordinates
## testTemporalAutocorrelation(fert.resid,  time=) # needs time
testZeroInflation(fert.resid)

## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
plot_grid(plot_model(fert.lm, type='diag'))

## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
augment(fert.lm) %>%
    ggplot() +
    geom_point(aes(y=.resid, x=.fitted))

## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
plot(allEffects(fert.lm, residuals=TRUE))

## ----validateModel5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
plot_model(fert.lm, type='eff', show.data=TRUE)


## ----validateModel6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
ggpredict(fert.lm) %>% plot(add.data=TRUE)
ggemmeans(fert.lm, ~FERTILIZER) %>% plot(add.data=TRUE) + theme_grey()


## ----summaryModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
summary(fert.lm)
confint(fert.lm)
tidy(fert.lm, conf.int=TRUE)

## ----summaryModel2a, results='asis', eval=TRUE, hidden=TRUE-------------------
fert.lm %>% tidy(conf.int=TRUE) %>% kable

## ----summaryModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
# warning this is only appropriate for html output
sjPlot::tab_model(fert.lm,show.se=TRUE,show.aic=TRUE)


## ----predictModel, results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE----
## # Predict Yield for Fertilizer conc of 110
## coef(fert.lm)
## 51.93 + 0.811*110
## 51.93*1 + 0.811*110
## ## can we automate this a little
## newdata = data.frame(FERTILIZER=110)
## Xmat = model.matrix(~FERTILIZER, data=newdata)
## coef(fert.lm) %*% t(Xmat)

## ----predictModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
## using the predict function
newdata = data.frame(FERTILIZER=110)
predict(fert.lm, newdata=newdata)

## ----predictModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
## using emmeans
newdata = data.frame(FERTILIZER=110)
ref_grid(fert.lm, cov.keep='FERTILIZER', at=newdata) %>%
    confint
#OR the one stop wrapper
emmeans(fert.lm, ~FERTILIZER, at=newdata)
#emtrends(fert.lm, ~1, var='FERTILIZER')

## ----predictModel4, results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE----
## ## testing a specific hypothesis
## ## Probabiliy of getting our estimate if slope was 1
## multcomp::glht(fert.lm, linfct=c('FERTILIZER == 1')) %>% summary
## ## Cant ask probability that the slope is equal to something in frequentist
## ## If we wanted to know the probability that the slope was greater than
## ## 1, the closest we could get is
## glht(fert.lm, linfct=c('FERTILIZER >= 1')) %>% summary


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
fert_grid = with(fert, list(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len=100)))
## OR
fert_grid = fert %>% data_grid(FERTILIZER=seq_range(FERTILIZER,  n=100))
newdata = emmeans(fert.lm, ~FERTILIZER, at=fert_grid) %>% as.data.frame
ggplot(newdata, aes(y=emmean, x=FERTILIZER))+
    geom_point(data=fert, aes(y=YIELD)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL),fill='blue', alpha=0.3) + 
    geom_line() +
    scale_y_continuous(expression(Grass~yield~(g.m^-3)))+
    scale_x_continuous(expression(Fertilizer~concentration~(g.ml^-1)))+
    theme_classic()

