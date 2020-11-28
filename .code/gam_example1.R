## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
library(broom)     #for tidy results
library(gratia)    #for GAM plots
library(DHARMa)    #for residual diagnostics
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(emmeans)   #for marginal means etc
library(MuMIn)     #for model selection and AICc
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
data_gam = read_csv('../data/data_gam.csv', trim_ws=TRUE)
glimpse(data_gam)


## ----EDA1a, results='markdown', eval=TRUE, hidden=TRUE------------------------
ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_line()


## ----EDA1b, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_smooth()


## ----EDA1c, results='markdown', eval=TRUE, hidden=TRUE,warning=FALSE,message=FALSE----
ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_smooth(method='lm')


## ----EDA1d, results='markdown', eval=TRUE, hidden=TRUE------------------------
ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_smooth(method='gam', formula=y~s(x,k=3))


## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
data.frame(smoothCon(s(x, k=3),  data=data_gam)[[1]]$X) %>%
  bind_cols(data_gam)


## ----fitModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
basis(s(x, k=3),  data=data_gam) %>% draw


## ----fitModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
newdata <-
data.frame(smoothCon(s(x, k=3),  data=data_gam)[[1]]$X) %>%
  bind_cols(data_gam)
ggplot(newdata,  aes(x=x)) +
  geom_line(aes(y=X1)) +
  geom_line(aes(y=X2)) +
  geom_line(aes(y=X3))


## ----fitModel14, results='markdown', eval=TRUE, hidden=TRUE,error=TRUE--------
data_gam.gam <- gam(y~s(x), data=data_gam)


## ----fitModel15, results='markdown', eval=TRUE, hidden=TRUE-------------------
data_gam.gam <- gam(y~s(x,k=3), data=data_gam, method='REML')
#data.gp.gam <- gam(y~s(x,k=3, bs='cr'), data=data.gp)
#data.gp.gam <- gam(y~s(x,k=3, bs='ps'), data=data.gp)


## ----modelValidation1a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
gam.check(data_gam.gam, pch=19)


## ----modelValidation1b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
k.check(data_gam.gam)


## ----modelValidation1c, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
appraise(data_gam.gam)


## ----modelValidation1d, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE, warning=FALSE, message=FALSE----
performance::check_model(data_gam.gam)


## ----modelValidation1e, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE, warning=FALSE, message=FALSE----
performance::check_distribution(data_gam.gam)


## ----modelValidationGAM1f, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE, warning=FALSE, message=FALSE, cache=FALSE----
resids <- DHARMa::simulateResiduals(data_gam.gam,  plot=TRUE)


## ----modelValidationGAM2, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE, cache=TRUE----
concurvity(data_gam.gam)
concurvity(data_gam.gam, full=FALSE)


## ----partialPlot, results='markdown', eval=TRUE, hidden=TRUE------------------
draw(data_gam.gam)
## or with the 'partial residuals' (quasi observations) included
draw(data_gam.gam, residuals=TRUE)


## ----modelSummary1a, results='markdown', eval=TRUE, hidden=TRUE---------------
summary(data_gam.gam)


## ----modelSummary1b, results='markdown', eval=TRUE, hidden=TRUE---------------
tidy(data_gam.gam)


## ----modelSummary1c, results='markdown', eval=TRUE, hidden=TRUE---------------
AIC(data_gam.gam)
AICc(data_gam.gam)


## ----Derivatives1a, results='markdown', eval=TRUE, hidden=TRUE----------------
derivatives(data_gam.gam,  order=1) %>% draw
## Find the approximate peak
d = derivatives(data_gam.gam,  order=1)
d
d %>% summarise(Value=data[which.min(abs(derivative))],
                lower=data[which.min(abs(lower))],
                upper=data[which.min(abs(upper))])


## ----Derivatives1a1, results='markdown', eval=TRUE, hidden=TRUE, echo=FALSE----
dd=d %>% summarise(Value=data[which.min(abs(derivative))],
                lower=data[which.min(abs(lower))],
                upper=data[which.min(abs(upper))]) %>%
  mutate(CI=paste0(round(lower, 2),'-',round(upper,2)))


## ----Derivatives1b, results='markdown', eval=TRUE, hidden=TRUE----------------
derivatives(data_gam.gam,  order=1) %>% draw
## Find the approximate peak
d = derivatives(data_gam.gam,  order=1)
d
d %>% summarise(
        maxDer = max(derivative), 
        Value=data[which.max(derivative)],
        lower=data[which.min(abs(maxDer-lower))],
        upper=data[which.min(abs(maxDer-upper))])


## ----Derivatives1b1, results='markdown', eval=TRUE, hidden=TRUE, echo=FALSE----
dd <- d %>% summarise(
              maxDer = max(derivative), 
              Value=data[which.max(derivative)],
              lower=data[which.min(abs(maxDer-lower))],
              upper=data[which.min(abs(maxDer-upper))]) %>%
  mutate(CI=paste0(round(lower, 2),'-',round(upper,2)))


## ----Derivatives1c, results='markdown', eval=TRUE, hidden=TRUE----------------
derivatives(data_gam.gam,  order=2) %>% draw
## Find the approximate peak
d = derivatives(data_gam.gam,  order=2)
d
d %>% summarise(Value=data[which.max(abs(derivative))],
                lower=data[which.max(abs(lower))],
                upper=data[which.max(abs(upper))])


## ----SummaryFig, results='markdown', eval=TRUE, hidden=TRUE-------------------
data_gam.list=with(data_gam, list(x=seq(min(x), max(x), len=100)))

newdata = emmeans(data_gam.gam, ~x, at=data_gam.list) %>%
    as.data.frame
head(newdata)

ggplot(newdata, aes(y=emmean, x=x)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=data_gam, aes(y=y,x=x))+
    theme_bw()

## Partials
#resid.list = list(x=data_gam$x)
newdata.partial = data_gam %>%
  mutate(Pred = predict(data_gam.gam,  type='link'),
         Res = resid(data_gam.gam),
         Resid = Pred + Res)
#newdata.partial = emmeans(data_gam.gam, ~x, at=resid.list) %>%
#    as.data.frame %>%
#    mutate(Resid = emmean + resid(data_gam.gam))

ggplot(newdata, aes(y=emmean, x=x)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=newdata.partial, aes(y=Resid,x=x))+
    theme_bw()



