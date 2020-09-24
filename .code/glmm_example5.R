## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for effects plots in ggplotjk
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(DHARMa)    #for assessing dispersion etc
library(glmmTMB)    #for glmmTMB
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## ----readData, results='markdown', eval=TRUE----------------------------------
owls = read_csv('../data/owls.csv', trim_ws=TRUE)
glimpse(owls)


## ----eda, results='markdown', eval=FALSE, hidden=TRUE-------------------------
## ## Amount of Sibling negotiation (vocalizations when parents are absent)
## ## Foot treatment (deprived or satiated
## ## Sex of parent
## ## Arrival time of parent
## ## Nest as random
## ## Brood size offset
## owls = owls %>% mutate(Nest =factor(Nest),
##                        FoodTreatment = factor(FoodTreatment),
##                        SexParent = factor(SexParent),
##                        NCalls = SiblingNegotiation)
## ggplot(data = owls) +
##     geom_boxplot(aes(y = NCalls, x = Nest)) +
##     facet_grid(SexParent ~ FoodTreatment)
## 
## ggplot(data=owls) +
##   geom_blank(aes(x=FoodTreatment)) +
##   geom_line(aes(y=NCalls,  x=as.numeric(FoodTreatment),  color=SexParent)) +
##   facet_wrap(~Nest)
## 
## ggplot(data = owls,aes(y = NCalls, x = BroodSize, color=SexParent)) +
##     geom_point() +
##     geom_smooth(method='lm') +
##     facet_grid(~FoodTreatment)
## 
## ggplot(data = owls,aes(y = NCalls, x = ArrivalTime, color=SexParent)) +
##     geom_point() +
##     geom_smooth(method='lm') +
##     facet_grid(~FoodTreatment)
## 


## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## owls.glmmTMB = glmmTMB(NCalls ~ FoodTreatment + (1|Nest), data=owls,
##                      family=poisson(link='log'),  REML=FALSE)
## owls.glmmTMB1 = glmmTMB(NCalls ~ FoodTreatment*scale(ArrivalTime, scale=FALSE) + (1|Nest), data=owls,
##                      family=poisson(link='log'),  REML=FALSE)
## owls.glmmTMB2 = glmmTMB(NCalls ~ FoodTreatment+scale(ArrivalTime, scale=FALSE) + (1|Nest), data=owls,
##                      family=poisson(link='log'),  REML=FALSE)
## owls.glmmTMB3 = glmmTMB(NCalls ~ SexParent*FoodTreatment*scale(ArrivalTime, scale=FALSE) + (1|Nest), data=owls,
##                         family=poisson(link='log'),  REML=FALSE)
## 
## owls.glmmTMB4 = glmmTMB(NCalls ~ FoodTreatment+scale(ArrivalTime, scale=FALSE) + offset(log(BroodSize)) + (1|Nest), data=owls,
##                      family=poisson(link='log'),  REML=FALSE)
## AICc(owls.glmmTMB,  owls.glmmTMB1,  owls.glmmTMB2,  owls.glmmTMB3,  owls.glmmTMB4)
## 
## owls.glmmTMB4a = update(owls.glmmTMB4,  REML=TRUE)
## owls.glmmTMB4b = update(owls.glmmTMB4a,  .~.-(1|Nest) + (scale(ArrivalTime, scale=FALSE)|Nest))
## owls.glmmTMB4c = update(owls.glmmTMB4a,  .~.-(1|Nest) + (FoodTreatment+scale(ArrivalTime, scale=FALSE)|Nest))
## 
## AICc(owls.glmmTMB4a,  owls.glmmTMB4b,  owls.glmmTMB4c)


## ----validation, results='markdown', eval=FALSE, hidden=TRUE------------------
## performance::check_model(owls.glmmTMB4c)
## #plot_model(owls.glmmTMB4c,  type='diag')
## owls.resid = simulateResiduals(owls.glmmTMB4c,  plot=TRUE)
## testZeroInflation(owls.resid)
## testDispersion(owls.resid)
## testTemporalAutocorrelation(owls.resid,  time=owls$ArrivalTime)
## owls.resid1 <- recalculateResiduals(owls.resid,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
## testTemporalAutocorrelation(owls.resid1,  time=unique(owls$ArrivalTime))
## #owls$Residuals = owls.resid1$scaledResiduals
## #owls.sub = owls[owls$Nest == 'AutavauxTV', ]
## #plot(Residuals ~ time,  data=owls.sub)
## 
## owls.glmmTMB5 = update(owls.glmmTMB4c,  ziformula=~1)
## owls.resid = simulateResiduals(owls.glmmTMB5,  plot=TRUE)
## testDispersion(owls.resid)
## testZeroInflation(owls.resid)


## ----summary, results='markdown', eval=FALSE, hidden=TRUE---------------------
## summary(owls.glmmTMB5)
## options(width=100)
## tidy(owls.glmmTMB5, conf.int=TRUE)
## plogis(-1.3705)
## exp(-1.3705)
## tidy(owls.glmmTMB5, effects='fixed', conf.int=TRUE,  exponentiate=TRUE)
## performance::r2_nakagawa(owls.glmmTMB5)
## 
## owls.glmmTMB6 = update(owls.glmmTMB4c,  ziformula=~FoodTreatment+scale(ArrivalTime, scale=FALSE))
## summary(owls.glmmTMB6)
## plogis(-1.89)
## exp(-1.89)
## exp(1.13)
## exp(0.18)
## tidy(owls.glmmTMB6, effects='fixed', conf.int=TRUE,  exponentiate=TRUE)
## performance::r2_nakagawa(owls.glmmTMB6)
## 


## ----summaryFig, results='markdown', eval=FALSE, hidden=TRUE------------------
## owls.grid = with(owls, list(FoodTreatment=levels(FoodTreatment),
##                             ArrivalTime=seq(min(ArrivalTime), max(ArrivalTime),  len=100),
##                             SexParent=levels(SexParent)))
## newdata = emmeans(owls.glmmTMB6, ~ArrivalTime|FoodTreatment, at=owls.grid,
##                   type='response') %>%
##     as.data.frame
## head(newdata)
## ggplot(newdata, aes(y=rate, x=ArrivalTime)) +
##     geom_line(aes(color=FoodTreatment)) +
##     geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL, fill=FoodTreatment), alpha=0.2)

