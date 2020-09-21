## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects) #for partial effects plots
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots


## ----readData, results='markdown', eval=TRUE----------------------------------
polis = read_csv('../data/polis.csv', trim_ws=TRUE)
glimpse(polis)
head(polis)
str(polis)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(polis, aes(y=PA, x=RATIO))+geom_point()
ggplot(polis, aes(y=PA, x=RATIO))+geom_point()+
    geom_smooth(method='glm', formula=y~x,
                method.args=list(family='binomial'))


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
polis.glm <- glm(PA ~ RATIO, family=binomial(link='logit'), data=polis)

## ----fitModel1, results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE-------
## polis$Total <- 1
## polis.glm <- glm(PA ~ RATIO, family=binomial(link='logit'), data=polis, weights=Total)
## polis.glm <- glm(cbind(PA,Total-PA) ~ RATIO, family=binomial(link='logit'), data=polis)


## ----validateModel, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(polis.glm, which=1:6, label.repel=TRUE)
## there does seem to be an outlier that is influential - obs #3
## Perhaps we should redo the scatterplot but with text so that we can see which obs is #3

## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE-------------------------
polis %>% mutate(n=1:nrow(.)) %>%
  ggplot(aes(y=PA, x=RATIO))+geom_text(aes(label=n))
## it seems that Uta lizards were present on this island dispite it having
## a relatively large surface area to volume ratio.
## Perhaps this island:
## - was a long way from other islands (isolated)
## - not inhabited
## - was very large
## - some other reason why it was not typical of the population.
## If no,  then we cannot simply exclude the point.
## In anycase,  if anything,  this obs is going to result in greater variability and
## a more concervative test - so it is not really harming to leave it in.


## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
polis.ss <- sum(resid(polis.glm, type="pearson")^2)
1-pchisq(polis.ss, polis.glm$df.resid)
#No evidence of a lack of fit

#Deviance
1-pchisq(polis.glm$deviance, polis.glm$df.resid)
#No evidence of a lack of fit

## ----validateModel2a, results='markdown', eval=FALSE, echo=FALSE, hidden=TRUE, fig.width=6, fig.height=6----
## # only work for LM and LMM
## #plot_grid(plot_model(polis.glm, type='diag'))

## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
augment(polis.glm) %>%
    ggplot() +
    geom_point(aes(y=.resid, x=.fitted))

## ----validateModelb, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
polis.resid <- simulateResiduals(polis.glm)
plot(polis.resid)
testResiduals(polis.resid)
## no evidence of overdispersion or lack of uniformity

## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
plot(allEffects(polis.glm, residuals=TRUE), type='response')

## ----validateModel5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
plot_model(polis.glm, type='eff', show.data=TRUE)


## ----validateModel6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
ggpredict(polis.glm) %>% plot(add.data=TRUE)
ggemmeans(polis.glm,  ~RATIO) %>% plot(add.data=TRUE)


## ----summaryModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
summary(polis.glm)
confint(polis.glm)
tidy(polis.glm, conf.int=TRUE)
glance(polis.glm)

## ----summaryModel2a, results='markdown', eval=TRUE, hidden=TRUE---------------
exp(coef(polis.glm))
polis.glm %>% coef %>% exp
##odd ratio - ratio of change in probability of present to absent for
##-when PA ratio is 0 (not possible), 36.8 times more likely to be
##present than absent
##a 1 unit change in X.  In this case 0.80.  So every 1 unit increase
##in PA ratio results in a 0.80 increase in odds ratio (prob)/(1-prob)
## That is the odds are multiplied by 0.8 - the odds decline by 20%
##present - thus a decline. So the chances of having UTA (likelihood
##of present/absent) decreases by
##approx 20% for each unit increase in PA ratio


## ----summaryModel2b, results='asis', eval=TRUE, hidden=TRUE-------------------
polis.glm %>% tidy(conf.int=TRUE) %>% kable

## ----summaryModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
# warning this is only appropriate for html output
sjPlot::tab_model(polis.glm,show.se=TRUE,show.aic=TRUE)


## ----predictModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
#R2
1-(polis.glm$deviance/polis.glm$null)


## ----predictModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
#LD50
(ld50 <- -polis.glm$coef[1]/polis.glm$coef[2])
## What about other points (not just 50%)
ld=MASS::dose.p(polis.glm, p=c(0.5,0.9))
ld.SE = attr(ld, "SE")
ld = data.frame(LD = attr(ld,'p'),
                Dose = as.vector(ld),
                SE = ld.SE) %>%
    mutate(lower=Dose-SE*qnorm(0.975),
           upper=Dose+SE*qnorm(0.975))
ld


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
polis.grid = with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))
polis.grid = polis %>% data_grid(RATIO=seq_range(RATIO,  n=100))

newdata=emmeans(polis.glm, ~RATIO,at=polis.grid, type='response') %>%
    as.data.frame

ggplot(newdata, aes(y=prob, x=RATIO))+
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), fill='blue',alpha=0.2)+
    geom_line() +
    theme_classic()

ggplot(newdata, aes(y=prob, x=RATIO))+
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), fill='blue',alpha=0.2)+
    geom_line() +
    geom_point(data=polis, aes(y=PA, x=RATIO))+
    theme_classic()

ggplot(newdata, aes(y=prob, x=RATIO))+
    geom_line(aes(y=asymp.LCL), linetype='dashed') +
    geom_line(aes(y=asymp.UCL), linetype='dashed') +
    geom_line() +
    geom_point(data=polis, aes(y=PA, x=RATIO))+
    theme_classic()


partial_resid=emmeans(polis.glm, ~RATIO, at=polis, type='link') %>%
    as.data.frame %>%
    dplyr::select(RATIO, emmean)
partial_resid

# Partial will represent the fitted values plus the residuals back transformed onto the probability scale
# Partial1 then backtransforms these onto the response scale [0,1]
partial_resid = polis %>% left_join(partial_resid) %>%
    mutate(Partial = emmean + resid(polis.glm, type='pearson'),
           #Partial = binomial()$linkinv(Partial),
           Partial = plogis(Partial), 
           Partial1 = qbinom(Partial,1,0.5))
partial_resid

ggplot(newdata, aes(y=prob, x=RATIO))+geom_line() +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), fill='blue',alpha=0.2)+
    geom_point(data=polis, aes(y=PA, x=RATIO))+
    geom_point(data=partial_resid, aes(y=Partial1), color='green') + 
    geom_vline(xintercept=ld50, linetype='dashed') +
    theme_classic()


## ----modelr, hidden=TRUE------------------------------------------------------
polis.mod = polis %>% fit_with(glm,  list(PA~RATIO),  family=binomial())
polis.mod[[1]] %>% modelr::rsquare(polis)
map(polis.mod,  ~rsquare(., polis))

modelr::add_predictions(polis,  polis.mod[[1]],  type='response')

