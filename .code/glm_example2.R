## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects) #for partial effects plots
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
polis = read_csv('../data/polis.csv', trim_ws=TRUE)


## ----examinData---------------------------------------------------------------
glimpse(polis)
head(polis)
str(polis)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(polis, aes(y=PA, x=RATIO))+
  geom_point()
ggplot(polis, aes(y=PA, x=RATIO))+
  geom_point()+
  geom_smooth(method='glm', formula=y~x,
              method.args=list(family='binomial'))


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
polis.glm <- glm(PA ~ RATIO, family=binomial(link='logit'), data=polis)


## ----fitModel1, results='markdown', eval=FALSE, echo=TRUE, hidden=TRUE--------
## polis$Total <- 1
## polis.glm1 <- glm(PA ~ RATIO, family=binomial(link='logit'), data=polis, weights=Total)


## ----fitModel2, results='markdown', eval=FALSE, echo=TRUE, hidden=TRUE--------
## polis$Total <- 1
## polis.glm1 <- glm(cbind(PA,Total-PA) ~ RATIO, family=binomial(link='logit'), data=polis)


## ----validateModel, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
polis.glm %>% autoplot(which=1:6, label.repel=TRUE)
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


## ----validateModela1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
polis.glm %>% influence.measures()


## ----validateModela1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
polis.glm %>% performance::check_model()


## ----validateModela1a1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=3, message=FALSE, warning=FALSE----
polis.glm %>% performance::check_outliers()
polis.glm %>% performance::check_outliers() %>% plot
## These are probabilities of exceedance rather than actual Cook's D values
#https://easystats.github.io/performance/reference/check_outliers.html
polis.glm %>% performance::check_heteroscedasticity()


## ----validateModela1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, message=FALSE, warning=FALSE----
polis.resid <- polis.glm %>% simulateResiduals(plot=TRUE)


## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
polis.ss <- sum(resid(polis.glm, type="pearson")^2)
1-pchisq(polis.ss, polis.glm$df.resid)
#No evidence of a lack of fit

#Deviance
1-pchisq(polis.glm$deviance, polis.glm$df.resid)
#No evidence of a lack of fit


## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=3, fig.height=3----
polis.glm %>%
    augment() %>%
    ggplot() +
    geom_point(aes(y=.resid, x=.fitted))


## ----validateModel5, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
polis.glm %>% plot_model(type='eff', show.data=TRUE)


## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
polis.glm %>% allEffects(residuals=TRUE) %>% plot(type='response')


## ----validateModel6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
polis.glm %>% ggpredict() %>% plot(add.data=TRUE, jitter=FALSE)


## ----validateModel7, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
polis.glm %>% ggemmeans(~RATIO) %>% plot(add.data=TRUE, jitter=FALSE)


## ----summaryModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
polis.glm %>% summary()


## ----summaryModel2d, results='markdown', echo=FALSE, eval=FALSE, hidden=TRUE----
## exp(coef(polis.glm))
## polis.glm %>% coef %>% exp
## ##odd ratio - ratio of change in probability of present to absent for
## ##-when PA ratio is 0 (not possible), 36.8 times more likely to be
## ##present than absent
## ##a 1 unit change in X.  In this case 0.80.  So every 1 unit increase
## ##in PA ratio results in a 0.80 increase in odds ratio (prob)/(1-prob)
## ## That is the odds are multiplied by 0.8 - the odds decline by 20%
## ##present - thus a decline. So the chances of having UTA (likelihood
## ##of present/absent) decreases by
## ##approx 20% for each unit increase in PA ratio


## ----summaryModel2a, results='markdown', eval=TRUE, hidden=TRUE---------------
## on link scale (log odds)
polis.glm %>% confint()
## or on odds (ratio) scale
polis.glm %>% confint() %>% exp


## ----summaryModel2c, results='markdown', eval=TRUE, hidden=TRUE---------------
polis.glm %>% tidy(conf.int=TRUE)
polis.glm %>% glance()


## ----summaryModel2b, results='asis', eval=TRUE, hidden=TRUE-------------------
polis.glm %>% tidy(conf.int=TRUE) %>% kable


## ----summaryModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
# warning this is only appropriate for html output
polis.glm %>% sjPlot::tab_model(show.se=TRUE,show.aic=TRUE)


## ----predictModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
#R2
1-(polis.glm$deviance/polis.glm$null)


## ----predictModel1a, results='markdown', eval=TRUE, hidden=TRUE---------------
#R2 for binomial outcomes
polis.glm %>% performance::r2_tjur()
#R2 for binomial outcomes
polis.glm %>% r2()

## Likelihood ratio based
polis.glm %>% MuMIn::r.squaredLR()


## ----predictModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
#LD50
(ld50 <- -polis.glm$coef[1]/polis.glm$coef[2])
## What about other points (not just 50%) along with confidence intervals..
ld <- polis.glm %>% MASS::dose.p(p=c(0.5,0.9))
ld.SE <- attr(ld, "SE")
ld <- data.frame(LD = attr(ld,'p'),
                 Dose = as.vector(ld),
                 SE = ld.SE) %>%
    mutate(lower = Dose-SE*qnorm(0.975),
           upper = Dose+SE*qnorm(0.975))
ld


## ----figureModel1a, results='markdown', eval=TRUE, hidden=TRUE----------------
## Using emmeans
polis.grid <- with(polis, list(RATIO = seq(min(RATIO), max(RATIO), len=100)))
#OR
polis.grid <- polis %>% data_grid(RATIO=seq_range(RATIO,  n=100))

newdata <- polis.glm %>% emmeans(~RATIO, at = polis.grid, type = 'response') %>%
    as.data.frame

ggplot(newdata, aes(y = prob, x = RATIO))+
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = 'blue', alpha = 0.2)+
    geom_line() +
    theme_classic()


## ----figureModel1b, results='markdown', eval=TRUE, hidden=TRUE----------------
ggplot(newdata, aes(y = prob, x = RATIO))+
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = 'blue', alpha = 0.2)+
    geom_line() +
    geom_point(data = polis, aes(y = PA, x = RATIO))+
    theme_classic()


## ----figureModel1c, results='markdown', eval=TRUE, hidden=TRUE----------------
ggplot(newdata, aes(y = prob, x = RATIO))+
    geom_line(aes(y = asymp.LCL), linetype = 'dashed') +
    geom_line(aes(y = asymp.UCL), linetype = 'dashed') +
    geom_line() +
    geom_point(data = polis, aes(y = PA, x = RATIO))+
    theme_classic()


## ----figureModel1d, results='markdown', eval=TRUE, hidden=TRUE----------------
polis.partial <- polis.glm %>%
    emmeans(~RATIO, at=polis, type="response") %>% as.data.frame() %>%
    mutate(
        Resid = stats::residuals(polis.glm, type="response"),
        Partial.obs = prob + Resid
    )
ggplot(newdata, aes(x=RATIO)) +
    geom_point(data = polis.partial, aes(y=Partial.obs), color='black') +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = 'blue', alpha = 0.2)+
    geom_line(aes(y=prob)) +
    geom_vline(xintercept = ld50, linetype='dashed') +
    theme_classic()


## ----figureModel1d2, results='markdown', eval=TRUE, hidden=TRUE---------------
# Partial will represent the fitted values plus the residuals back transformed onto the probability scale
# Partial.obs then backtransforms these onto the response scale [0,1]
polis.partial <- polis.glm %>%
    emmeans(~RATIO, at=polis, type="link") %>% as.data.frame() %>%
    mutate(
        Resid = stats::residuals(polis.glm, type="working"),
        Partial = plogis(emmean + Resid),
        Partial.obs = qbinom(Partial, 1, 0.5)
    )
ggplot(newdata, aes(x=RATIO)) +
    geom_point(data = polis.partial, aes(y=Partial), color='gray') +
    geom_point(data = polis.partial, aes(y=Partial.obs), color='black') +
    geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), fill = 'blue', alpha = 0.2)+
    geom_line(aes(y=prob)) +
    geom_vline(xintercept = ld50, linetype='dashed') +
    theme_classic()


## ----figureModel2a, results='markdown', eval=TRUE, hidden=TRUE----------------
newdata = polis %>%
  modelr::add_predictions(polis.glm) %>%
  modelr::add_residuals(polis.glm) %>%
  mutate(Partial=pred + resid,
         pred = plogis(pred))

ggplot(newdata, aes(y=pred, x=RATIO))+geom_line() +
    #geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), fill='blue',alpha=0.2)+
    geom_point(data=polis, aes(y=PA, x=RATIO))+
    geom_point(aes(y=Partial), color='green') + 
    geom_vline(xintercept=ld50, linetype='dashed') +
    theme_classic()

## ----modelr, hidden=TRUE------------------------------------------------------
polis.mod = polis %>% fit_with(glm,  list(PA~RATIO),  family=binomial())
polis.mod[[1]] %>% modelr::rsquare(polis)
map(polis.mod,  ~rsquare(., polis))

modelr::add_predictions(polis,  polis.mod[[1]],  type='response')

