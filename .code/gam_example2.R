## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, mesage=FALSE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(broom)     #for tidy output#
library(emmeans)   #for marginal means etc
library(MuMIn)     #for model selection and AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for simulated residuals
library(performance) #for residual disagnostics
library(see)        # to visualize residual diagnostics


## ----readData, results='markdown', eval=TRUE----------------------------------
reed = read_csv('../data/reed.csv', trim_ws=TRUE)
glimpse(reed)


## ----EDA1a, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point()


## ----EDA1b, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point() +
    geom_smooth(method='gam', formula=y~s(x),
                method.args=list(family='poisson'))


## ----EDA1c, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(reed, aes(y=Moorhen.Kauai, x=Rainfall)) +
    geom_point() +
    geom_smooth(method='gam', formula=y~s(x),
                method.args=list(family='poisson'))



## ----basis, results='markdown', eval=TRUE, hidden=TRUE------------------------
basis(s(Year, bs='tp'),  data=reed) %>% draw
basis(s(Year, bs='cr'),  data=reed) %>% draw


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
reed.gam1 <- gam(Moorhen.Kauai ~ s(Year, bs = 'cr') +
                     s(Rainfall, bs = 'cr'),
                 data = reed,
                 family = poisson(link = 'log'),
                 method = 'REML')
reed.gam1a <- gam(Moorhen.Kauai ~ s(Year, bs = 'cr') +
                     s(Rainfall, bs = 'cr'),
                 data = reed,
                 family = poisson(link = 'log'),
                 method = 'REML',
                 select=TRUE)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE------------
k.check(reed.gam1)


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
appraise(reed.gam1)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
reed.gam2 = gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr') +
                    s(Rainfall, bs = 'cr'),
                data = reed,
                family = poisson(link = 'log'),
                method = 'REML')


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE------------
k.check(reed.gam2)


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
appraise(reed.gam2)


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
concurvity(reed.gam2)


## ----modelValidation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
performance::check_model(reed.gam2)


## ----modelValidation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.resids <- simulateResiduals(reed.gam2,  plot=TRUE)
testZeroInflation(reed.resids)


## ----fitModel3, results='markdown', eval=TRUE, hidden=TRUE--------------------
reed.gam3 <- gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr') +
                   s(Rainfall, bs = 'cr'),
                data = reed,
                family = nb(link = 'log'),
                method = 'REML')
## get the final theta estimate
(theta <- reed.gam3$family$getTheta(TRUE))
## or with supplied theta
reed.gam3 = gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr') +
                    s(Rainfall, bs = 'cr'),
                data = reed,
                family = negbin(link = 'log', theta = theta),
                method = 'REML')


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
k.check(reed.gam3)


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
appraise(reed.gam3)


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
concurvity(reed.gam3)


## ----modelValidation3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
performance::check_model(reed.gam3)


## ----modelValidation3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.resids <- simulateResiduals(reed.gam3,  plot=TRUE)
testZeroInflation(reed.resids)


## ----modelValidation3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
testTemporalAutocorrelation(reed.resids, time=reed.gam3$model$Year)
# OR
Year = reed %>%
  filter(!is.na(Moorhen.Kauai)) %>%
  pull(Year)
testTemporalAutocorrelation(reed.resids, time=Year)


## ----ACF, results='markdown', eval=TRUE, hidden=TRUE--------------------------
plot(acf(residuals(reed.gam3,type='pearson')))


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
draw(reed.gam3)
draw(reed.gam3, residuals = TRUE, scales = 'free') 


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
#plot(reed.gam3, pages=1)
#plot(reed.gam3, pages=1, shift=coef(reed.gam3)[1])
#plot(reed.gam3, pages=1, shift=coef(reed.gam3)[1], scale=0)
plot(reed.gam3, pages=1, shift=coef(reed.gam3)[1], trans=exp,
     resid=TRUE, cex=4, scale=0)


## ----modelSummary1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
summary(reed.gam3)


## ----modelSummary1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
tidy(reed.gam3)


## ----fitModel4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.gam4 <- gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr') +
                    Rainfall,
                data = reed,
                family = nb(link = 'log'),
                method = 'REML')
(theta <- reed.gam4$family$getTheta(TRUE))
reed.gam4 <- gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr') +
                     Rainfall,
                 data = reed,
                 family = negbin(link = 'log', theta = theta),
                 method = 'REML')
AICc(reed.gam3,  reed.gam4)


## ----fitModel4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
k.check(reed.gam4)
appraise(reed.gam4)


## ----fitModel4c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.resid <- simulateResiduals(reed.gam4,  plot=TRUE)
testDispersion(reed.resid)
testZeroInflation(reed.resid)

testTemporalAutocorrelation(reed.resid, time=reed.gam4$model$Year)
Year = reed %>%
  filter(!is.na(Moorhen.Kauai)) %>%
  pull(Year)
testTemporalAutocorrelation(reed.resid, time=Year)


## ----fitModel4d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
summary(reed.gam4)


## ----fitModel5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.gam5 <- gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr'), 
                data = reed,
                family = nb(link = 'log'),
                method = 'REML')
(theta=reed.gam5$family$getTheta(TRUE))
reed.gam5 <- gam(Moorhen.Kauai ~ s(Year, k = 20, bs = 'cr'), 
                 data = reed,
                 family = negbin(link = 'log', theta = theta),
                 method = 'REML')
AICc(reed.gam3,  reed.gam4,  reed.gam5)


## ----fitModel5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
k.check(reed.gam5)
appraise(reed.gam5)


## ----fitModel5c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
reed.resid <- simulateResiduals(reed.gam5,  plot=TRUE)
testDispersion(reed.resid)
testZeroInflation(reed.resid)
Year = reed %>%
  filter(!is.na(Moorhen.Kauai)) %>%
  pull(Year)
testTemporalAutocorrelation(reed.resid, time=Year)


## ----fitModel5d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
summary(reed.gam5)


## ----summaryFigure1a, results='markdown', eval=TRUE, hidden=TRUE--------------
reed.list <- with(reed, list(Year = modelr::seq_range(Year, n=100)))
newdata <- emmeans(reed.gam3, ~Year, at = reed.list, type = 'response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y = response, x = Year)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = 'blue', alpha = 0.3) +
    geom_line() +
    theme_bw()


## ----summaryFigure1b, results='markdown', eval=TRUE, hidden=TRUE--------------
reed.obs <- with(reed.gam3$model,
                 data.frame(Year = Year,
                            Rainfall = mean(Rainfall))) %>%
    mutate(Pred = predict(reed.gam3, newdata=., type='link'),
           Resid = reed.gam3$residuals,
           Presid = exp(Pred + Resid))
    
## reed.presid <- data.frame(Year = reed.gam3$model$Year,
##                           Rainfall = mean(reed.gam3$model$Rainfall)) %>%
##     mutate(Pred = predict(reed.gam3, newdata=., type='link'),
##            Resid = reed.gam3$residuals,
##            Presid = exp(Pred + Resid))
head(reed.obs)
ggplot(newdata, aes(y=response, x=Year)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
  geom_point(data=reed.obs, aes(y=Presid)) +
  geom_point(data=reed, aes(y = Moorhen.Kauai, x=Year), color='red')


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE--------------
reed.list= with(reed, list(Year=seq(min(Year), max(Year), len=100)))

newdata = emmeans(reed.gam5, ~Year, at=reed.list, type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=Year)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    theme_bw() +
    geom_point(data=reed, aes(y = Moorhen.Kauai, x=Year))

