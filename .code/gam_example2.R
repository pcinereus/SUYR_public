## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(emmeans)   #for marginal means etc
library(MuMIn)     #for model selection and AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for simulated residuals
library(performance) #for residual disagnostics
library(see)        # to visualize residual diagnostics


## ----readData, results='markdown', eval=TRUE----------------------------------
reed = read_csv('../data/reed.csv', trim_ws=TRUE)
glimpse(reed)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point()

ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point() + geom_smooth()

ggplot(reed, aes(y=Moorhen.Kauai, x=Year)) +
    geom_point() +
    geom_smooth(method='gam', formula=y~s(x),
                method.args=list(family='poisson'))

ggplot(reed, aes(y=Moorhen.Kauai, x=Rainfall)) +
    geom_point() +
    geom_smooth()



## ----basis, results='markdown', eval=TRUE, hidden=TRUE------------------------
basis(s(Year, bs='tp'),  data=reed) %>% draw
basis(s(Year, bs='cr'),  data=reed) %>% draw


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
reed.gam = gam(Moorhen.Kauai ~ s(Year,bs='cr') +
                   s(Rainfall,bs='cr'),
               data=reed, family=poisson(link='log'), method='REML')
k.check(reed.gam)
appraise(reed.gam)


## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
reed.gam1 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr') +
                   s(Rainfall,bs='cr'),
               data=reed, family=poisson(link='log'), method='REML')
k.check(reed.gam1)
concurvity(reed.gam1)
appraise(reed.gam1)
performance::check_model(reed.gam1)
reed.resids <- simulateResiduals(reed.gam1,  plot=TRUE)
testDispersion(reed.resids)
testZeroInflation(reed.resids)



## ----modelValidation, results='markdown', eval=TRUE, hidden=TRUE--------------
dev=deviance(reed.gam1)
rdf = df.residual(reed.gam1)
dev/rdf
1-pchisq(dev,rdf)


## ----fitModel2, results='markdown', eval=TRUE, hidden=TRUE--------------------
## In mgcv,  there are two methods for fitting negative binomial:
## 1. using the nb() function.  This method estiates the dispersion parameter (theta)
##    as part of the likelihood.  Unfortunately,  DHARMa is not compatible with
##    mgcv's version.
## 2. using the negbin() function and supplying theta.  This approach does support
##    DHARMa residuals.
reed.gam2 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr') +
                   s(Rainfall,bs='cr'),
                data=reed, family=nb(link='log'), method='REML')
## get the final theta estimate
(theta=reed.gam2$family$getTheta(TRUE))
## or with supplied theta
reed.gam2 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr') +
                   s(Rainfall,bs='cr'),
                data=reed, family=negbin(link='log', theta=theta), method='REML')
k.check(reed.gam2)
appraise(reed.gam2)
reed.resid <- simulateResiduals(reed.gam2,  plot=TRUE)
testDispersion(reed.resid)
testZeroInflation(reed.resid)
Year = reed %>%
  filter(!is.na(Moorhen.Kauai)) %>%
  pull(Year)
testTemporalAutocorrelation(reed.resid, time=Year)

## dev=deviance(reed.gam2)
## rdf = df.residual(reed.gam2)
## dev/rdf
## 1-pchisq(dev,rdf)


## ----partialPlot, results='markdown', eval=TRUE, hidden=TRUE------------------
draw(reed.gam2)
plot(reed.gam2, pages=1)
plot(reed.gam2, pages=1, shift=coef(reed.gam2)[1])
plot(reed.gam2, pages=1, shift=coef(reed.gam2)[1], scale=0)
plot(reed.gam2, pages=1, shift=coef(reed.gam2)[1], trans=exp,
     resid=TRUE, cex=4, scale=0)



## ----ACF, results='markdown', eval=TRUE, hidden=TRUE--------------------------
plot(acf(residuals(reed.gam2,type='pearson')))


## ----modelSummary, results='markdown', eval=TRUE, hidden=TRUE-----------------
summary(reed.gam2)


## ----fitModel3, results='markdown', eval=TRUE, hidden=TRUE--------------------
reed.gam3 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr') +
                   Rainfall,
                data=reed, family=nb(link='log'), method='REML')
(theta=reed.gam3$family$getTheta(TRUE))
reed.gam3 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr') +
                   Rainfall,
                data=reed, family=negbin(link='log', theta=theta), method='REML')
k.check(reed.gam3)
appraise(reed.gam3)
reed.resid <- simulateResiduals(reed.gam3,  plot=TRUE)
testDispersion(reed.resid)
testZeroInflation(reed.resid)
Year = reed %>%
  filter(!is.na(Moorhen.Kauai)) %>%
  pull(Year)
testTemporalAutocorrelation(reed.resid, time=Year)

summary(reed.gam3)


## ----fitModel4, results='markdown', eval=TRUE, hidden=TRUE--------------------
reed.gam4 = gam(Moorhen.Kauai ~ s(Year, k=20, bs='cr'),
               data=reed, family=nb(link='log'), method='REML')
k.check(reed.gam4)
appraise(reed.gam4)
summary(reed.gam4)
AICc(reed.gam1, reed.gam2, reed.gam3, reed.gam4)



## ----summaryFigure, results='markdown', eval=FALSE, hidden=TRUE---------------
## reed.list= with(reed, list(Year=seq(min(Year), max(Year), len=100)))
## 
## newdata = emmeans(reed.gam3, ~Year, at=reed.list, type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=response, x=Year)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
##     geom_line() +
##     theme_bw()
## 
## ggplot(newdata, aes(y=response, x=Year)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
##     geom_line() +
##     theme_bw() +
##     geom_point(data=reed, aes(y = Moorhen.Kauai, x=Year))
## 
## reed.presid = data.frame(Year=reed.gam3$model$Year,
##                          Rainfall=mean(reed.gam3$model$Rainfall)) %>%
##     mutate(Pred = predict(reed.gam3, newdata=., type='link'),
##            Resid = reed.gam3$residuals,
##            Presid = exp(Pred + Resid))
## head(reed.presid)
## ggplot(newdata, aes(y=response, x=Year)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
##     geom_line() +
##   geom_point(data=reed.presid, aes(y=Presid)) +
##   geom_point(data=reed, aes(y = Moorhen.Kauai, x=Year), color='red')
## 
## (reed.presid = data.frame(Year=reed.gam4$model$Year))
## 
## ## ggplot(newdata, aes(y=response, x=Year)) +
## ##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
## ##     geom_line() +
## ##     geom_point(data=reed.presid, aes(y=Resid)) +
## ##     theme_bw()
## 
## ## reed.presid = list(Year=reed.gam4$model$Year)
## ## newdata.resid = emmeans(reed.gam4, ~Year, at=reed.presid, type='link') %>%
## ##     as.data.frame %>%
## ##    # mutate(Resid = exp(emmean + resid(reed.gam4)))
## ##    mutate(Resid = exp(emmean + reed.gam4$residual))
## ## ggplot(newdata, aes(y=response, x=Year)) +
## ##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
## ##     geom_line() +
## ##     geom_point(data=newdata.resid, aes(y=Resid)) +
## ##     theme_bw()

