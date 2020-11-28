## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)


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


## ----EDA1a, results='markdown', eval=TRUE, hidden=TRUE------------------------
ggplot(wq, aes(y=NOx, x=Date)) + geom_point()


## ----EDA1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=15, fig.height=12----
ggplot(wq, aes(y=NOx, x=Date)) +
    geom_point() +
    facet_wrap(~reef.alias)


## ----EDA1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=15, fig.height=12----
ggplot(wq, aes(y=NOx, x=Date)) +
  geom_point() +
  geom_smooth() +
  scale_y_continuous(trans=scales::pseudo_log_trans()) +
  facet_wrap(~reef.alias,  scales='free_y')



## ----prepareData2, results='markdown', eval=TRUE, hidden=TRUE-----------------
wq=wq %>% mutate(Dt.num=decimal_date(Date))
#wq=wq %>% mutate(sDt=scale(Date))


## ----model1a, results='markdown', eval=TRUE, hidden=TRUE----------------------
wq.pandora=wq %>% filter(reef.alias=='Pandora', !is.na(NOx))

ggplot(wq.pandora, aes(y=NOx, x=Date)) + geom_point() +
    geom_smooth() +
  scale_y_log10()


## ----model1b, results='markdown', eval=TRUE, hidden=TRUE----------------------
wq.pandora=wq %>% filter(reef.alias=='Pandora', !is.na(NOx))


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
wq.gam1 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family='gaussian', method='REML')
wq.gam2 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=gaussian(link='log'), method='REML')
wq.gam3 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=Gamma(link='log'), method='REML')
wq.gam4 <- gam(NOx ~ s(Dt.num), data=wq.pandora, family=tw(link='log'), method='REML')
library(MuMIn)
AICc(wq.gam1, wq.gam2, wq.gam3, wq.gam4)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam1)
appraise(wq.gam1)
performance::check_model(wq.gam1)


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.resid <- simulateResiduals(wq.gam1,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam1$model$Dt.num)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam2)
appraise(wq.gam2)
performance::check_model(wq.gam2)


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.resid <- simulateResiduals(wq.gam2,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam2$model$Dt.num)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam3)
appraise(wq.gam3)
performance::check_model(wq.gam3)


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.resid <- simulateResiduals(wq.gam3,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam3$model$Dt.num)


## ----modelValidation4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam4)
appraise(wq.gam4)
performance::check_model(wq.gam4)


## ----modelValidation4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
#wq.resid <- simulateResiduals(wq.gam4,  plot=TRUE)
#testTemporalAutocorrelation(wq.resid,  time=wq.gam4$model$Dt.num)


## ----partialPlots1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gam1)


## ----partialPlots1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gam1, pages=1,  shift=coef(wq.gam1)[1], resid=TRUE, cex=4,  scale=0)


## ----partialPlots2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gam2)


## ----partialPlots2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gam2, pages=1,  shift=coef(wq.gam2)[1], resid=TRUE, trans=exp,  cex=4,  scale=0)


## ----partialPlots3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gam3)


## ----partialPlots3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gam3, pages=1,  shift=coef(wq.gam3)[1], resid=TRUE, trans=exp,  cex=4,  scale=0)


## ----partialPlots4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gam4)


## ----partialPlots4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gam4, pages=1,  shift=coef(wq.gam4)[1], resid=TRUE, trans=exp,  cex=4,  scale=0)


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(wq.gam1)


## ----summary1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(wq.gam1)


## ----summary2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(wq.gam2)


## ----summary2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(wq.gam2)


## ----summary3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(wq.gam3)


## ----summary3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(wq.gam3)


## ----summary4a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
summary(wq.gam4)


## ----summary4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
tidy(wq.gam4)


## ----fitModel5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
wq.gam5 <- gam(NOx ~ s(Dt.num, by=Season),
               data=wq.pandora,
               family=Gamma(link='log'), method='REML')


## ----modelValidation5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam5)
appraise(wq.gam5)
performance::check_model(wq.gam5)


## ----modelValidation5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.resid <- simulateResiduals(wq.gam5,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam5$model$Dt.num)


## ----partialPlots5a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gam5)


## ----partialPlots5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gam5, pages=1,  shift=coef(wq.gam5)[1], resid=TRUE, trans=exp,  cex=4,  scale=0)


## ----fitModel6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
wq.gam6 <- gam(NOx ~ s(Dt.num)+
                 s(Mnth,bs='cc',k=5,fx=F),
               knots=list(Mnth=seq(1,12,length=5)),
               data=wq.pandora,
               family=Gamma(link='log'), method='REML')


## ----modelValidation6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gam6)
appraise(wq.gam6)
#performance::check_model(wq.gam6)


## ----modelValidation6b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.resid <- simulateResiduals(wq.gam6,  plot=TRUE)
testTemporalAutocorrelation(wq.resid,  time=wq.gam5$model$Dt.num)


## ----partialPlots6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gam6)


## ----partialPlots6b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gam6, pages=1,  shift=coef(wq.gam5)[1], resid=TRUE, trans=exp,  cex=4,  scale=0)


## ----AIC1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
AIC(wq.gam5, wq.gam6)


## ----modelSummary, results='markdown', eval=TRUE, hidden=TRUE-----------------
summary(wq.gam6)


## ----summaryFigure6a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
wq.list = with(wq.pandora, list(Dt.num = seq(min(Dt.num), max(Dt.num), len=100)))
newdata = emmeans(wq.gam6, ~Dt.num, at=wq.list, type='response') %>% as.data.frame
head(newdata)
g1=ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw()

wq.list = with(wq.pandora, list(Mnth = seq(1, 12, len=100)))
newdata1 = emmeans(wq.gam6, ~Mnth, at=wq.list, type='response') %>% as.data.frame
head(newdata1)
g2=ggplot(newdata1, aes(y=response, x=Mnth)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    theme_bw()
#library(gridExtra)
#grid.arrange(g1, g2, nrow=1)
g1 + g2

## Partial residuals
wq.presid = data.frame(Dt.num=wq.gam6$model$Dt.num, Mnth=mean(wq.gam6$model$Mnth)) %>%
    mutate(Pred = predict(wq.gam6, newdata=., type='link'),
           Resid = wq.gam6$residuals,
           Presid = exp(Pred + Resid))
head(wq.presid)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw() +
    geom_point(data=wq.presid, aes(y=Presid)) +
    geom_point(data=wq.pandora, aes(y=NOx), color='red', alpha=0.5)

wq.presid=with(wq.gam6$model, data.frame(Dt.num=Dt.num, Mnth=mean(Mnth))) %>%
    mutate(Resid=exp(as.vector(predict(wq.gam6, newdata=., type='link')) + wq.gam6$residuals))

ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=wq.presid, aes(y=Resid)) +
    scale_x_datetime('') +
    theme_bw() 



## ----process3, results='markdown', eval=FALSE,hidden=TRUE---------------------
## wq.sub <- wq %>%
##   group_by(reef.alias) %>%
##   mutate(Min=min(Dt.num)) %>%
##   ungroup() %>%
##   filter(Min<2012) %>%
##   droplevels
## 
## ## reef=wq %>%
## ##   group_by(reef.alias) %>%
## ##   dplyr:::summarise(Min=min(Dt.num)) %>%
## ##   filter(Min<2012) %>%
## ##   pull(reef.alias)
## ## reef
## ## wq2=wq %>% filter(reef.alias %in% reef) %>% droplevels
## ggplot(wq.sub, aes(y=NOx,x=Dt.num)) +
##     geom_point() +
##     facet_wrap(~reef.alias)
## 


## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=15, fig.height=12----
ggplot(wq, aes(y=NOx,x=Dt.num)) +
    geom_point() +
    facet_wrap(~reef.alias, scales='free_y')
## Some reefs dont have the full time series


## ----fitModel7a, results='markdown', eval=TRUE,hidden=TRUE--------------------
wq.gamm1 <- gamm(NOx ~ s(Dt.num), random=list(reef.alias=~1),
                 data=wq, family=Gamma(link='log'), method='REML')


## ----validateModel7a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm1$gam)
appraise(wq.gamm1$gam)


## ----validateModel7b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
#wq.resids <- simulateResiduals(wq.gamm1$gam,  plot=TRUE)


## ----partialPlots7a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gamm1$gam)


## ----partialPlots7b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gamm1$gam, pages=1,  shift=fixef(wq.gamm1$lme)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----fitModel8a, results='markdown', eval=TRUE,hidden=TRUE--------------------
wq.gamm2 <- gamm4::gamm4(NOx ~ s(Dt.num),  random=~(1|reef.alias),
                 data=wq, family=Gamma(link='log'), REML=TRUE)


## ----validateModel8a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm2$gam)
appraise(wq.gamm2$gam)


## ----validateModel8b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
#wq.resids <- simulateResiduals(wq.gamm2$gam,  plot=TRUE)


## ----partialPlots8a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
draw(wq.gamm2$gam)


## ----partialPlots8b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(wq.gamm2$gam, pages=1,  shift=fixef(wq.gamm2$mer)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----fitModel9a, results='markdown', eval=TRUE,hidden=TRUE--------------------
wq.gamm3 <- gam(NOx ~ s(Dt.num) + s(reef.alias,  bs='re'),
                 data=wq, family=Gamma(link='log'), method='REML')


## ----validateModel9a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm3)
appraise(wq.gamm3)


## ----validateModel9b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
wq.resids <- simulateResiduals(wq.gamm3,  plot=TRUE)


## ----partialPlots9a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gamm3)


## ----partialPlots9b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gamm3, pages=1,  shift=coef(wq.gamm3)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----fitModel10a, results='markdown', eval=TRUE,hidden=TRUE-------------------
wq.gamm3a <- gam(NOx ~ s(Dt.num, k=20) + s(reef.alias,  bs='re'),
                 data=wq, family=Gamma(link='log'), method='REML')


## ----validateModel10a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm3a)
appraise(wq.gamm3a)


## ----validateModel10b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
wq.resids <- simulateResiduals(wq.gamm3a,  plot=TRUE)


## ----partialPlots10a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gamm3a)


## ----partialPlots10b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gamm3a, pages=1,  shift=coef(wq.gamm3)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----fitModel11a, results='markdown', eval=TRUE,hidden=TRUE-------------------
wq.gamm3b <- gam(NOx ~ s(Dt.num,  k=20)+
                   s(Mnth,bs='cc',k=5) +
                   s(reef.alias, bs='re'),
                 knots=list(Mnth=seq(1,12,length=5)),
                 family=Gamma(link='log'),
                 data=wq, method='REML')


## ----validateModel11a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm3b)
appraise(wq.gamm3b)


## ----validateModel11b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
wq.resids <- simulateResiduals(wq.gamm3b,  plot=TRUE)


## ----partialPlots11a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gamm3b)


## ----partialPlots11b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gamm3b, pages=1,  shift=coef(wq.gamm3)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----fitModel12a, results='markdown', eval=TRUE,hidden=TRUE-------------------
## wq.gamm3c <- gam(NOx ~ s(Dt.num, k=20)+
##                    s(Mnth,bs='cc',k=5) +
##                    s(Region,  bs='re') +
##                    s(reef.alias, bs='re'),
##                  knots=list(Mnth=seq(1,12,length=5)),
##                  family=Gamma(link='log'),
##                 data=wq, method='REML')
wq.gamm3c <- gam(NOx ~ s(Dt.num, by=Region, k=20)+
                   s(Mnth,bs='cc', by=Region, k=5) +
                   s(reef.alias, bs='re'),
                 knots=list(Mnth=seq(1,12,length=5)),
                 family=Gamma(link='log'),
                data=wq, method='REML')


## ----validateModel12a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=5, fig.height=5----
k.check(wq.gamm3c)
appraise(wq.gamm3c)


## ----validateModel12b, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
wq.resids <- simulateResiduals(wq.gamm3c,  plot=TRUE)


## ----partialPlots12a, results='markdown', eval=TRUE,hidden=TRUE, fig.width=8, fig.height=4----
draw(wq.gamm3c)


## ----partialPlots12b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
plot(wq.gamm3c, pages=1,  shift=coef(wq.gamm3)[1], resid=FALSE, trans=exp,  cex=4,  scale=0)


## ----summarizeModel12a, results='markdown', eval=TRUE, hidden=TRUE------------
summary(wq.gamm3c)


## ----summarizeModel12b, results='markdown', eval=TRUE, hidden=TRUE------------
tidy(wq.gamm3c)
tidy(wq.gamm3c) %>% knitr::kable()


## ----furtherAnalyses1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
emmeans(wq.gamm3c,  pairwise~Dt.num,  at=list(Dt.num=c(2016, 2014)),
        type='response') %>%
  confint


## ----furtherAnalyses1a1, results='markdown', eval=TRUE, echo=FALSE, hidden=TRUE, fig.width=5, fig.height=5----
wq.emmeans <- emmeans(wq.gamm3c,  pairwise~Dt.num,  at=list(Dt.num=c(2016, 2014)), type='response') %>% confint() %>% as.data.frame


## ----furtherAnalyses1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
emmeans(wq.gamm3c,  pairwise~Dt.num|Region,  at=list(Dt.num=c(2016, 2014)),
        type='response') %>%
  confint


## ----furtherAnalyses1b1, results='markdown', eval=TRUE, echo=FALSE, hidden=TRUE, fig.width=5, fig.height=5----
wq.emmeans <- emmeans(wq.gamm3c,  pairwise~Dt.num|Region,  at=list(Dt.num=c(2016, 2014)), type='response') %>% confint() %>% as.data.frame


## ----summaryFigure2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
wq.list = with(wq, list(Dt.num = seq(min(Dt.num), max(Dt.num), len=100),
                        Region=levels(Region)))
newdata = emmeans(wq.gamm3c, ~Dt.num, at=wq.list, type='response',
                  data=wq.gamm3c$model) %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
    geom_line() +
    scale_x_datetime('') +
    theme_bw()

## Partial residuals
wq.presid = data.frame(
  wq.gamm3c$model %>% dplyr::select(Dt.num, Mnth), 
  Resid=exp(
    as.vector(predict(wq.gamm3c,  exclude='s(reef.alias)',  type='link')) +
    wq.gamm3c$residuals
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


## ----summaryFigure2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=9, fig.height=3----
wq.list = with(wq, list(Dt.num = seq(min(Dt.num), max(Dt.num), len=100),
                        Region=levels(Region)))
newdata = emmeans(wq.gamm3c, ~Dt.num|Region, at=wq.list, type='response',
                  data=wq.gamm3c$model) %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=date_decimal(Dt.num))) +
  geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), fill='blue', alpha=0.3) +
  geom_line() +
  scale_x_datetime('') +
  facet_wrap(~Region,  nrow=1) +
  theme_bw()

