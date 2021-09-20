## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(broom.mixed) #for tidy output
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
library(lme4)       #for glmer
library(glmmTMB)    #for glmmTMB


## ----readData, results='markdown', eval=TRUE----------------------------------
owls = read_csv('../data/owls.csv', trim_ws=TRUE)
glimpse(owls)


## ----dataProcessing, results='markdown', eval=TRUE, hidden=TRUE---------------
## Amount of Sibling negotiation (vocalizations when parents are absent)
## Foot treatment (deprived or satiated
## Sex of parent
## Arrival time of parent
## Nest as random
## Brood size offset
owls = owls %>% mutate(Nest =factor(Nest),
                       FoodTreatment = factor(FoodTreatment),
                       SexParent = factor(SexParent),
                       NCalls = SiblingNegotiation)


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=5----
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point()
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point(position=position_jitterdodge(jitter.height=0,  dodge.width=1))
ggplot(data = owls, aes(y = NCalls, x = FoodTreatment,  color=SexParent)) +
  geom_violin() +
  geom_point(position=position_jitterdodge(jitter.height=0,  dodge.width=1))+
  scale_y_continuous(trans=scales::pseudo_log_trans())


## ----eda2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10----
ggplot(data=owls) +
  geom_point(aes(y=NCalls,  x=FoodTreatment,  color=SexParent),  position=position_dodge(0.5)) +
  facet_wrap(~Nest)


## ----eda3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=5----
ggplot(data = owls,aes(y = NCalls, x = BroodSize, color=SexParent)) +
  geom_point() + 
  geom_smooth(method='lm') +
  facet_grid(~FoodTreatment) +
  scale_y_continuous(trans=scales::pseudo_log_trans()) +
  scale_x_log10()


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmer1 <- glmer(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize)) + (1|Nest),  data=owls,
                    family=poisson(link='log'))
owls.glmer2 <- update(owls.glmer1, ~ . - FoodTreatment:SexParent)
AICc(owls.glmer1, owls.glmer2)
anova(owls.glmer1, owls.glmer2)


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE-------
owls.glmer1a <- owls.glmer1
owls.glmer1b <- update(owls.glmer1a, ~ . - (1|Nest) + (FoodTreatment|Nest))
owls.glmer1c <- update(owls.glmer1a, ~ . - (1|Nest) + (SexParent|Nest))
owls.glmer1d <- update(owls.glmer1a, ~ . - (1|Nest) + (FoodTreatment*SexParent|Nest))
owls.allFit <- allFit(owls.glmer1d)
owls.allFit
## Check which of the models are considered valid (OK)
is.OK <- sapply(owls.allFit, is, "merMod")
is.OK
diff_optims.OK <- owls.allFit[is.OK]
lapply(diff_optims.OK,function(x) x@optinfo$conv$lme4$messages)
owls.glmer1d <- update(owls.glmer1d, control=glmerControl(optimizer='bobyqa'))
owls.glmer1c <- update(owls.glmer1c, control=glmerControl(optimizer='bobyqa'))
owls.glmer1a <- update(owls.glmer1a, control=glmerControl(optimizer='bobyqa'))
owls.glmer1b <- update(owls.glmer1b, control=glmerControl(optimizer='bobyqa'))

AICc(owls.glmer1a, owls.glmer1b, owls.glmer1c, owls.glmer1d)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmmTMB1 <- glmmTMB(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize))
                         + (1|Nest),  data=owls,
                         family=poisson(link='log'), REML=FALSE)
owls.glmmTMB2 <- update(owls.glmmTMB1, ~ . - FoodTreatment:SexParent)
AICc(owls.glmmTMB1, owls.glmmTMB2)
anova(owls.glmmTMB1, owls.glmmTMB2)


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmmTMB1a <- update(owls.glmmTMB1,  REML=TRUE)
owls.glmmTMB1b <- update(owls.glmmTMB1a, ~ . - (1|Nest) + (FoodTreatment|Nest))
owls.glmmTMB1c <- update(owls.glmmTMB1a, ~ . - (1|Nest) + (SexParent|Nest))
owls.glmmTMB1d <- update(owls.glmmTMB1a, ~ . - (1|Nest) + (FoodTreatment*SexParent|Nest))

AICc(owls.glmmTMB1a, owls.glmmTMB1b, owls.glmmTMB1c, owls.glmmTMB1d)


## ----validation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(owls.glmer1d,  type='diag')


## ----validation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(owls.glmer1d)


## ----validation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(owls.glmer1d)


## ----validation1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_overdispersion(owls.glmer1d)


## ----validation1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_zeroinflation(owls.glmer1d)


## ----validation1f, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(owls.glmer1d,  plot=TRUE, integerResponse = TRUE)  


## ----validation1g, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
testZeroInflation(owls.resid)  


## ----validation1h, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
testTemporalAutocorrelation(owls.resid,  time=owls$ArrivalTime)
owls.resid1 <- recalculateResiduals(owls.resid,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
testTemporalAutocorrelation(owls.resid1,  time=unique(owls$ArrivalTime))


## ----validation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7, message=FALSE, warning=FALSE----
plot_model(owls.glmmTMB1d,  type='diag')


## ----validation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_model(owls.glmmTMB1d)


## ----validation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_distribution(owls.glmmTMB1d)


## ----validation2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_overdispersion(owls.glmmTMB1d)


## ----validation2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10, message=FALSE, warning=FALSE----
performance::check_zeroinflation(owls.glmmTMB1d)


## ----validation2f, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, message=FALSE, warning=FALSE----
owls.resid = simulateResiduals(owls.glmmTMB1d,  plot=TRUE, integerResponse = TRUE)


## ----validation2g, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
testZeroInflation(owls.resid)


## ----validation2h, results='markdown', eval=TRUE, error=TRUE,hidden=TRUE, fig.width=7, fig.height=5, cache=FALSE, message=FALSE, warning=FALSE----
testTemporalAutocorrelation(owls.resid,  time=owls$ArrivalTime)
owls.resid1 <- recalculateResiduals(owls.resid,  group=interaction(owls$ArrivalTime,  owls$Nest),  aggregateBy = mean)
testTemporalAutocorrelation(owls.resid1,  time=unique(owls$ArrivalTime))


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmmTMB3 <- glmmTMB(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize)) +
                           (FoodTreatment*SexParent|Nest), 
                         ziformula=~1,  data=owls,
                         family=poisson(link='log'),
                         REML=TRUE)
#OR
owls.glmmTMB3 <- update(owls.glmmTMB1d, ziformula=~1)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
owls.resid = simulateResiduals(owls.glmmTMB3,  plot=TRUE, integerResponse = TRUE)
testZeroInflation(owls.resid)
testDispersion(owls.resid)
performance::check_overdispersion(owls.glmmTMB3)
testUniformity(owls.resid)
testQuantiles(owls.resid)
testResiduals(owls.resid)


## ----fitModel4a, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmmTMB4 <- glmmTMB(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize)) +
                           (FoodTreatment*SexParent|Nest), 
                         ziformula=~FoodTreatment*SexParent,  data=owls,
                         family=poisson(link='log'), REML=TRUE)


## ----fitModel4b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
owls.resid = simulateResiduals(owls.glmmTMB4,  plot=TRUE, integerResponse = TRUE)
testZeroInflation(owls.resid)
testDispersion(owls.resid)
testUniformity(owls.resid)
testQuantiles(owls.resid)
testResiduals(owls.resid)


## ----fitModel5, results='markdown', eval=TRUE, hidden=TRUE--------------------
owls.glmmTMB5 <- glmmTMB(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize)) +
                           (FoodTreatment*SexParent|Nest), 
                         ziformula = ~1,  data=owls,
                         family=nbinom2(link='log'), REML=TRUE)


## ----fitModel5b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
owls.resid = simulateResiduals(owls.glmmTMB5,  plot=TRUE, integerResponse = TRUE)
testZeroInflation(owls.resid)
testDispersion(owls.resid)
testUniformity(owls.resid)
testQuantiles(owls.resid)
testResiduals(owls.resid)


## ----fitModel6a, results='markdown', eval=TRUE, hidden=TRUE-------------------
owls.glmmTMB6 <- glmmTMB(NCalls ~ FoodTreatment*SexParent + offset(log(BroodSize)) +
                           (FoodTreatment+SexParent|Nest), 
                         ziformula = ~FoodTreatment*SexParent,  data=owls,
                         family=nbinom2(link='log'), REML=TRUE)


## ----fitModel6b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
owls.resid = simulateResiduals(owls.glmmTMB6,  plot=TRUE, integerResponse = TRUE)
testZeroInflation(owls.resid)
testDispersion(owls.resid)
testUniformity(owls.resid)
testQuantiles(owls.resid)
testResiduals(owls.resid)


## ----compareModels, results='markdown', eval=TRUE-----------------------------
AICc(owls.glmmTMB3, owls.glmmTMB4,  owls.glmmTMB5, owls.glmmTMB6)


## ----partialPlots1a, results='markdown', eval=TRUE----------------------------
plot_model(owls.glmmTMB4,  type='eff',  terms=c('FoodTreatment', 'SexParent'))
plot_model(owls.glmmTMB6,  type='eff',  terms=c('FoodTreatment', 'SexParent'))


## ----partialPlots1b, results='markdown', eval=TRUE----------------------------
plot(allEffects(owls.glmmTMB4), multiline=TRUE, ci.style='bars')


## ----partialPlots1c, results='markdown', eval=TRUE----------------------------
ggpredict(owls.glmmTMB4,  terms=c('FoodTreatment', 'SexParent')) %>% plot


## ----partialPlots1d1, results='markdown', eval=TRUE---------------------------
#off<-owls %>% group_by(SexParent, FoodTreatment) %>% summarize(Mean=mean(BroodSize))
off<-owls %>% summarize(Mean=mean(BroodSize))
as.numeric(off)
ggemmeans(owls.glmmTMB4,  ~FoodTreatment+SexParent, offset=log(off$Mean)) %>% plot


## ----partialPlots1d2, results='markdown', eval=TRUE---------------------------
ggemmeans(owls.glmmTMB4,  ~FoodTreatment+SexParent, offset=0) %>% plot
ggemmeans(owls.glmmTMB6,  ~FoodTreatment+SexParent, offset=0) %>% plot


## ----summary1a, results='markdown', eval=TRUE, hidden=TRUE--------------------
summary(owls.glmmTMB4)
summary(owls.glmmTMB6)


## ----summary1a1, results='markdown', eval=TRUE, echo=FALSE, hidden=TRUE-------
owls.tidy <- tidy(owls.glmmTMB4)


## ----summary1b, results='markdown', eval=TRUE, hidden=TRUE--------------------
tidy(owls.glmmTMB4,  conf.int=TRUE)
## or on the response scale
tidy(owls.glmmTMB4,  conf.int=TRUE, exponentiate = TRUE)
tidy(owls.glmmTMB4,  conf.int=TRUE, exponentiate = TRUE) %>% kable


## ----summary1c, results='markdown', eval=TRUE, hidden=TRUE--------------------
# warning this is only appropriate for html output
sjPlot::tab_model(owls.glmmTMB4, show.se=TRUE, show.aic=TRUE)


## ---- eval=FALSE--------------------------------------------------------------
## options(width=100)
## tidy(owls.glmmTMB4, conf.int=TRUE)
## plogis(-1.3705)
## exp(-1.3705)
## tidy(owls.glmmTMB4, effects='fixed', conf.int=TRUE,  exponentiate=TRUE)


## ----r2, results='markdown', eval=TRUE, hidden=TRUE---------------------------
#r.squaredGLMM(owls.glmmTMB4)
performance::r2_nakagawa(owls.glmmTMB4)


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
## owls.grid = with(owls, list(FoodTreatment=levels(FoodTreatment),
##                             SexParent=levels(SexParent)))
## newdata = emmeans(owls.glmmTMB4, ~FoodTreatment+SexParent, at=owls.grid,
##                   offset=0, type='response') %>%
##     as.data.frame
newdata = emmeans(owls.glmmTMB4, ~FoodTreatment+SexParent, 
                  offset=0, type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=rate, x=FoodTreatment)) +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL, color=SexParent),
                  position=position_dodge(width=0.2)) +
  scale_y_continuous('Number of sibling negotiations per chick') +
  theme_bw()

##OR if we want to express this for the average brood size
## owls.grid = with(owls, list(FoodTreatment=levels(FoodTreatment),
##                             SexParent=levels(SexParent)))
## newdata = emmeans(owls.glmmTMB4, ~FoodTreatment+SexParent, at=owls.grid,
##                   offset=log(mean(owls$BroodSize)), type='response') %>%
##     as.data.frame
newdata = emmeans(owls.glmmTMB4, ~FoodTreatment+SexParent,
                  offset=log(mean(owls$BroodSize)), type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=rate, x=FoodTreatment)) +
  geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL, color=SexParent),
                  position=position_dodge(width=0.2)) +
  scale_y_continuous('Number of sibling negotiations per nest') +
  theme_bw()


## ----comparemodels, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=6----

newdata=tidy(owls.glmmTMB3, effects='fixed', conf.int=TRUE,  exponentiate=TRUE) %>%
  mutate(Model='zip (simple zi)') %>%
  bind_rows(
    tidy(owls.glmmTMB4, effects='fixed', conf.int=TRUE,  exponentiate=TRUE) %>%
    mutate(Model='zip (complex zi)')
  ) %>%
  bind_rows(
    tidy(owls.glmmTMB5, effects='fixed', conf.int=TRUE,  exponentiate=TRUE) %>%
    mutate(Model='zinb (simple zi)')
  ) %>%
  bind_rows(
    tidy(owls.glmmTMB6, effects='fixed', conf.int=TRUE,  exponentiate=TRUE) %>%
    mutate(Model='zinb (complex zi)')
  ) %>%
  mutate(Model=factor(Model,  levels=c('zip (simple zi)', 'zip (complex zi)',
                                       'zinb (simple zi)', 'zinb (complex zi)')),
         Cond=interaction(component, term)) %>%
  arrange(component, term) %>%
  mutate(Cond=factor(Cond,  levels=rev(unique(Cond))))

ggplot(newdata,  aes(y=estimate,  x=Cond,  color=Model)) +
  geom_pointrange(aes(ymin=conf.low,  ymax=conf.high),  position=position_dodge(width=0.2)) +
  coord_flip()


newdata = emmeans(owls.glmmTMB3, ~FoodTreatment+SexParent, offset=0, type='response') %>%
  as.data.frame %>% mutate(Model='zip (simple zi)',  response=rate) %>%
  bind_rows(
    emmeans(owls.glmmTMB4, ~FoodTreatment+SexParent, offset=0, type='response') %>%
    as.data.frame %>% mutate(Model='zip (complex zi)',  response=rate)
  ) %>%
  bind_rows(
    emmeans(owls.glmmTMB5, ~FoodTreatment+SexParent, offset=0, type='response') %>%
    as.data.frame %>% mutate(Model='zinb (simple zi)',  response=response)
  ) %>%
  bind_rows(
    emmeans(owls.glmmTMB6, ~FoodTreatment+SexParent, offset=0, type='response') %>%
    as.data.frame %>% mutate(Model='zinb (complex zi)',  response=response)
  ) %>%
  mutate(Model=factor(Model,  levels=c('zip (simple zi)', 'zip (complex zi)',
                                       'zinb (simple zi)', 'zinb (complex zi)')))

head(newdata)
ggplot(newdata, aes(y=response, x=FoodTreatment)) +
  geom_pointrange(aes(color=SexParent, ymin=lower.CL, ymax=upper.CL), 
                  position=position_dodge(width=0.2)) +
  facet_wrap(~Model,  nrow=1)

ggplot(newdata,  aes(y=response,  x=interaction(FoodTreatment,SexParent),  color=Model)) +
  geom_pointrange(aes(ymin=lower.CL,  ymax=upper.CL),  position=position_dodge(width=0.2)) +
  coord_flip()

