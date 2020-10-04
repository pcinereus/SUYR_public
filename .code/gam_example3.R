## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(mgcv)      #for GAMs
library(gratia)    #for GAM plots
library(emmeans)   #for marginal means etc
library(broom)     #for tidy output
library(MuMIn)     #for model selection and AICc
library(lubridate) #for processing dates
library(tidyverse) #for data wrangling
library(DHARMa)    #for residuals diagnostics
library(performance) #for residual disagnostics
library(see)        # to visualize residual diagnostics
library(patchwork)  #for grids of plots


## ----readData, results='markdown', eval=TRUE----------------------------------
wq = read_csv('../data/aims.wq.csv', trim_ws=TRUE)
glimpse(wq)


## ----prepareData, results='markdown', eval=TRUE-------------------------------
wq = wq %>% mutate(reef.alias=factor(reef.alias),
                   Region=factor(Region),
                   Subregion=factor(Subregion),
                   Season=factor(Season))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(wq, aes(y=NOx, x=Date)) + geom_point()


ggplot(wq, aes(y=NOx, x=Date)) +
    geom_point() +
    facet_wrap(~reef.alias)

ggplot(wq, aes(y=NOx, x=Date)) + geom_point() +
    scale_y_log10()

ggplot(wq, aes(y=NOx, x=Date)) + geom_point() +
    geom_smooth() +
    scale_y_log10()

ggplot(wq, aes(y=NOx, x=Date)) +
    geom_point() +
        geom_smooth() +
        scale_y_log10() +
        facet_wrap(~Subregion)

ggplot(wq, aes(y=NOx, x=Date)) +
  geom_point() +
  geom_smooth() +
  scale_y_log10() +
  facet_wrap(~reef.alias,  scales='free_y')



## ----prepareData2, results='markdown', eval=TRUE, hidden=TRUE-----------------
## Although we generally want to scale any continuous predictors, it appears that
## doing so with Date objects has downstream issues - the models fit ok, but
## partial plots are displaced along the x-axis.
## Date data cannot be directly modelled, it must be converted into a numeric.
## So as an alternative either dont scale, or do so with a routine that does not
## automatically back-trasform (such as sjmisc::std or sjmisc::center)
wq=wq %>% mutate(Dt.num=decimal_date(Date))
#wq=wq %>% mutate(sDt=scale(Date))


## ----Pandora, results='markdown', eval=TRUE, hidden=TRUE----------------------
wq.pandora=wq %>% filter(reef.alias=='Pandora', !is.na(NOx))

ggplot(wq.pandora, aes(y=NOx, x=Date)) + geom_point() +
    geom_smooth() +
    scale_y_log10()


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
wq.gam <- gam(NOx ~ s(Dt.num), data=wq.pandora, family='gaussian', method='REML')
wq.gam1 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=gaussian(link='log'), method='REML')
wq.gam2 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=Gamma(link='log'), method='REML')
wq.gam3 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=tw(link='log'), method='REML')
library(MuMIn)
AICc(wq.gam, wq.gam1, wq.gam2, wq.gam3)

#wq.gam <- gam(NOx ~ s(, data=wq.pandora, family='gaussian', method='REML')
#wq.gam1 <- gam(NOx ~ s(, data=wq.pandora, family=gaussian(link='log'), method='REML')
#wq.gam2 <- gam(NOx ~ s(, data=wq.pandora, family=Gamma(link='log'), method='REML')
#wq.gam3 <- gam(NOx ~ s(, data=wq.pandora, family=tw, method='REML')
#library(MuMIn)
#AICc(wq.gam, wq.gam1, wq.gam2, wq.gam3)


## ----modelValidation, results='markdown', eval=TRUE, hidden=TRUE--------------
k.check(wq.gam2)
appraise(wq.gam2)
draw(wq.gam2,  residuals=TRUE)
## not great predictive power


## ----modelValidation2, results='markdown', eval=TRUE, hidden=TRUE-------------
performance::check_model(wq.gam2)
wq.resid <- simulateResiduals(wq.gam2,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam2$model$Dt.num)
#dev=deviance(wq.gam2)
#rdf=df.residual(wq.gam2)
#1-pchisq(dev, rdf)
## Not a fantastic fit!
## Perhaps if we had another covariate..

acf(resid(wq.gam2))


## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
wq.gam2a <- gam(NOx ~ s(Dt.num, by=Season),
               data=wq.pandora,
               family=Gamma(link='log'), method='REML')
draw(wq.gam2a)
#wq.gam2a <- gam(NOx ~ s(sDt, by=Season),
#               data=wq.pandora,
#               family=Gamma(link='log'), method='REML')

wq.gam3 <- gam(NOx ~ s(Dt.num)+
                 s(Mnth,bs='cc',k=5,fx=F),
               knots=list(Mnth=seq(1,12,length=5)),
               data=wq.pandora,
               family=Gamma(link='log'), method='REML')
#wq.gam3 <- gam(NOx ~ s(sDt)+
#                   s(Mnth,bs='cc',k=5,fx=F),
#               knots=list(Mnth=seq(1,12,length=5)),
#               data=wq.pandora,
#               family=Gamma(link='log'), method='REML')

k.check(wq.gam3)
AICc(wq.gam2, wq.gam3)
appraise(wq.gam3)
wq.resid <- simulateResiduals(wq.gam3,  plot=TRUE)
draw(wq.gam3)


## ----modelSummary, results='markdown', eval=TRUE, hidden=TRUE-----------------
summary(wq.gam3)


## ----summaryFigure, results='markdown', eval=TRUE, hidden=TRUE----------------
wq.list = with(wq.pandora, list(Dt.num = seq(min(Dt.num), max(Dt.num), len=100)))
newdata = emmeans(wq.gam3, ~Dt.num, at=wq.list, type='response') %>% as.data.frame
head(newdata)
g1=ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw()

wq.list = with(wq.pandora, list(Mnth = seq(1, 12, len=100)))
newdata1 = emmeans(wq.gam3, ~Mnth, at=wq.list, type='response') %>% as.data.frame
head(newdata1)
g2=ggplot(newdata1, aes(y=response, x=Mnth)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    theme_bw()
#library(gridExtra)
#grid.arrange(g1, g2, nrow=1)
g1 + g2

## Partial residuals
wq.presid = data.frame(Dt.num=wq.gam3$model$Dt.num, Mnth=mean(wq.gam3$model$Mnth)) %>%
    mutate(Pred = predict(wq.gam3, newdata=., type='link'),
           Resid = wq.gam3$residuals,
           Presid = exp(Pred + Resid))
head(wq.presid)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw() +
    geom_point(data=wq.presid, aes(y=Presid)) +
    geom_point(data=wq.pandora, aes(y=NOx), color='red', alpha=0.5)

wq.presid=with(wq.gam3$model, data.frame(Dt.num=Dt.num, Mnth=mean(Mnth))) %>%
    mutate(Resid=exp(as.vector(predict(wq.gam3, newdata=., type='link')) + wq.gam3$residuals))

ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=wq.presid, aes(y=Resid)) +
    scale_x_datetime('') +
    theme_bw() 

#ggpredict(wq.gam3, ~Dt.num, full.data=TRUE) %>% plot
#ggemmeans(wq.gam3, ~Dt.num, full.data=TRUE) %>% plot



## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(wq, aes(y=NOx,x=Dt.num)) +
    geom_point() +
    facet_wrap(~reef.alias, scales='free_y')
## Some reefs dont have the full time series


## ----process3, results='markdown', eval=TRUE,hidden=TRUE----------------------
wq.sub <- wq %>%
  group_by(reef.alias) %>%
  mutate(Min=min(Dt.num)) %>%
  ungroup() %>%
  filter(Min<2012) %>%
  droplevels

## reef=wq %>%
##   group_by(reef.alias) %>%
##   dplyr:::summarise(Min=min(Dt.num)) %>%
##   filter(Min<2012) %>%
##   pull(reef.alias)
## reef
## wq2=wq %>% filter(reef.alias %in% reef) %>% droplevels
ggplot(wq.sub, aes(y=NOx,x=Dt.num)) +
    geom_point() +
    facet_wrap(~reef.alias)



## ----fitModel3, results='markdown', eval=TRUE,hidden=TRUE---------------------
wq.gamm <- gamm(NOx ~ s(Dt.num), random=list(reef.alias=~1),
                data=wq.sub, family=Gamma(link='log'), method='REML')
k.check(wq.gamm$gam)
appraise(wq.gamm$gam)
draw(wq.gamm$gam)
                                        #wq.resid <- simulateResiduals(wq.gamm,  plot=TRUE)
wq.gamm1 <- gam(NOx ~ s(Dt.num) + s(reef.alias, bs='re'),
                data=wq.sub, family=Gamma(link='log'), method='REML')
k.check(wq.gamm1)
appraise(wq.gamm1)
wq.resid <- simulateResiduals(wq.gamm1,  plot=TRUE)
draw(wq.gamm1)

## wq.gamm <- gamm(NOx ~ s(Dt.num,k=20), random=list(reef.alias=~1),
##                 data=wq.sub, family=Gamma(link='log'),method='REML')
## k.check(wq.gamm$gam)
## ##appraise(wq.gamm$gam) #does not yet handle gamm
## gam.check(wq.gamm$gam)
## draw(wq.gamm$gam)

wq.gamm1 <- gam(NOx ~ s(Dt.num,  k=20) + s(reef.alias, bs='re'),
                data=wq, family=Gamma(link='log'), method='REML')
k.check(wq.gamm1)
appraise(wq.gamm1)
wq.resid <- simulateResiduals(wq.gamm1,  plot=TRUE)
draw(wq.gamm1)
#wq.gamm2 <- gam(NOx ~ s(Dt.num,  k=20) + s(reef.alias, bs='re'),
#                data=wq, family=tw(), method='REML')
#k.check(wq.gamm2)
#appraise(wq.gamm2)
#wq.resid <- simulateResiduals(wq.gamm2,  plot=TRUE)


## ----fitModel4, results='markdown', eval=TRUE,hidden=TRUE---------------------
## wq.gamm2 <- gamm(NOx ~ s(Dt.num, k=20)+
##                     s(Mnth,bs='cc',k=5),
##                 knots=list(Mnth=seq(1,12,length=5)),
##                 random=list(reef.alias=~1),
##                 family=Gamma(link='log'),
##                 data=wq, method='REML')
## k.check(wq.gamm2$gam)
## gam.check(wq.gamm2$gam)
## draw(wq.gamm2$gam)

wq.gamm2 <- gam(NOx ~ s(Dt.num, k=20)+
                   s(Mnth,bs='cc',k=5) +
                   s(reef.alias, bs='re'),
                 knots=list(Mnth=seq(1,12,length=5)),
                 family=Gamma(link='log'),
                data=wq, method='REML')
k.check(wq.gamm2)
gam.check(wq.gamm2)
wq.sub.resids <- simulateResiduals(wq.gamm2,  plot=TRUE)
draw(wq.gamm2)

wq.gamm2b <- gam(NOx ~ s(Dt.num, k=20)+
                   s(Mnth,bs='cc',k=5) +
                   s(Region,  bs='re') +
                   s(reef.alias, bs='re'),
                 knots=list(Mnth=seq(1,12,length=5)),
                 family=Gamma(link='log'),
                data=wq, method='REML')
k.check(wq.gamm2b)
gam.check(wq.gamm2b)
wq.sub.resids <- simulateResiduals(wq.gamm2b,  plot=TRUE)
draw(wq.gamm2b)



## ----summarizeModel2, results='markdown', eval=TRUE, hidden=TRUE--------------
## summary(wq.gamm2$lme)
## summary(wq.gamm2$gam)
## tidy(wq.gamm2$gam)
summary(wq.gamm2)
tidy(wq.gamm2)
tidy(wq.gamm2,  conf.int=TRUE,  exponentiate=TRUE)
summary(wq.gamm2b)


## ----summaryFigure2, results='markdown', eval=TRUE, hidden=TRUE---------------
##gamm
##plot(wq.gamm2$gam,cex=4,shift=fixef(wq.gamm2$lme)[1],
##     trans=exp,scale=0, resid=TRUE, ylim=c(0,10),pages=1)
##plot(wq.gamm2$gam,cex=4,shift=fixef(wq.gamm2$lme)[1],
##     trans=I,resid=TRUE,pages=1)
## gam
plot(wq.gamm2,cex=4,shift=coef(wq.gamm2)[1],
     trans=exp,scale=0, resid=TRUE, ylim=c(0,10),pages=1)
plot(wq.gamm2,cex=4,shift=coef(wq.gamm2)[1],
     trans=I,resid=TRUE,pages=1)

wq.list = with(wq, list(Dt.num = seq(min(Dt.num), max(Dt.num), len=100)))
## gamm
##newdata = emmeans(wq.gamm2, ~Dt.num, at=wq.list, type='response',
##                  data=wq.gamm2$gam$model) %>% as.data.frame
## gam
newdata = emmeans(wq.gamm2, ~Dt.num, at=wq.list, type='response',
                  data=wq.gamm2$model) %>% as.data.frame
#newdata = emmeans(wq.gamm, ~Dt.num, at=wq.list, type='response', call=wq.gamm2$gam$call) %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw()
## Note, it does appear that these values are slightly higher than predict would give...

## Partial residuals
#wq.presid=with(wq.gamm2$gam$model, data.frame(Dt.num=Dt.num, Mnth=mean(Mnth))) %>%
#    mutate(Resid=exp(fixef(wq.gamm2$lme)[1] + as.vector(predict(wq.gamm2$gam, newdata=., type='terms')[,1]) + wq.gamm2$gam$residuals))
## gamm
##wq.presid=with(wq.gamm2$gam$model, data.frame(Dt.num=Dt.num, Mnth=mean(Mnth))) %>%
##    mutate(Resid=exp(as.vector(predict(wq.gamm2$gam, newdata=., type='link')) + wq.gamm2$gam$residuals))
## gam
##wq.presid=with(wq.gamm2$model, data.frame(Dt.num=Dt.num, Mnth=mean(Mnth),  reef.alias=NA))
wq.presid = data.frame(
  wq.gamm2$model %>% dplyr::select(Dt.num, Mnth), 
  Resid=exp(
    as.vector(predict(wq.gamm2,  exclude='s(reef.alias)',  type='link')) +
    wq.gamm2$residuals
  )
)

head(wq.presid)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
  geom_line() +
  geom_point(data=wq.presid, aes(y=Resid)) +
  #geom_point(data=wq,  aes(y=NOx, x=date_decimal(Dt.num)),  color='red') +
  scale_x_datetime('') +
  scale_y_continuous(breaks=seq(0,10,by=2), limits=c(0,10))+
  theme_bw() 

#ggpredict(wq.gamm2$gam, ~Dt.num, back.transform = TRUE) %>% plot()
#draw(wq.gamm2$gam, parametric=FALSE)
                                        #
## What was the decline in NOx betwen 2014 and 2016
emmeans(wq.gamm2,  pairwise~Dt.num,  at=list(Dt.num=c(2016, 2014)),
        type='response') %>%
  confint


