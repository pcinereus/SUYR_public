## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
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


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_line()

ggplot(data_gam, aes(y=y, x=x))+ 
    geom_point()+
    geom_smooth(method='gam', formula=y~s(x,k=3))


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## data_gam.gam <- gam(y~s(x), data=data_gam)


## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
data_gam.gam <- gam(y~s(x,k=3), data=data_gam, method='REML')
#data.gp.gam <- gam(y~s(x,k=3, bs='cr'), data=data.gp)
#data.gp.gam <- gam(y~s(x,k=3, bs='ps'), data=data.gp)


## ----basis, results='markdown', eval=TRUE, hidden=TRUE------------------------
basis(s(x, k=3),  data=data_gam) %>% draw


## ----modelValidation, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
gam.check(data_gam.gam, pch=19)
k.check(data_gam.gam)
appraise(data_gam.gam)
performance::check_model(data_gam.gam)
performance::check_distribution(data_gam.gam)
resids <- DHARMa::simulateResiduals(data_gam.gam,  plot=TRUE)


## ----modelValidation2, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
##the nonlinear equivalent is called concurvity. Concurvity measures how well each
##smooth could be approximated by either a combination of the other smooth(s) or
##each individual other smooth.
## Three metrics generated - all bounded between 0 (no issue) and 1 (total lack of indentifiability)
## Essentially,  we can decompose a smooth term into the component that lies within the space (g) of other terms
## and the component that is unique to the terms own space (f).
## The three metrics are all variants on the ratio of g:f
## - worst - largest ratio possible from any estimated coefficients (data ignored) - worst case scenario
## - observed - ratio based on observed coefficients - can be over optimistic
## - estimate - a measure of the extent to which f basis can be explained by g basis.
##              It is a good compromise between over optimism and over pessimism,  yet is harder to understand
concurvity(data_gam.gam)
concurvity(data_gam.gam, full=FALSE)


## ----partialPlot, results='markdown', eval=TRUE, hidden=TRUE------------------
draw(data_gam.gam)
draw(data_gam.gam, residuals=TRUE)
## Note,  these plots are passed on to cowplot for multiple figures
## and thus it is not possible to use + etc


## ----modelSummary, results='markdown', eval=TRUE, hidden=TRUE-----------------
summary(data_gam.gam)
AIC(data_gam.gam)
AICc(data_gam.gam)


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



## ----Derivatives, results='markdown', eval=TRUE, hidden=TRUE------------------
derivatives(data_gam.gam,  order=1) %>% draw
## Find the approximate peak
d = derivatives(data_gam.gam,  order=1)
d
d %>%
  arrange(abs(derivative)) %>%
  slice(1)

