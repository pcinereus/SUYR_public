## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
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
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(DHARMa)    #for residual diagnostics plots
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(modelr)    #for auxillary modelling functions
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
peake = read_csv('../data/peakquinn.csv', trim_ws=TRUE)
glimpse(peake)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(peake, aes(y=INDIV, x=AREA)) +
  geom_point()+
  geom_smooth()


## ----EDA1, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(peake, aes(y=INDIV)) + geom_boxplot()

ggplot(peake, aes(y=AREA)) + geom_boxplot()


## ----EDA2, results='markdown', eval=TRUE, hidden=TRUE, warning=FALSE, message=FALSE----
ggplot(peake, aes(y=INDIV, x=AREA)) +
  geom_point()+
  geom_smooth() +
  scale_y_log10() +
  scale_x_log10()


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
peake.lm <- lm(INDIV~AREA, data=peake) 


## ----validateModel1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, messsage=FALSE, warning=FALSE----
autoplot(peake.lm, which=1:6)


## ----validateModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
influence.measures(peake.lm)


## ----validateModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
performance::check_model(peake.lm)


## ----validateModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
peake.resid <- simulateResiduals(peake.lm,  plot=TRUE)


## ----validateModel1e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4, warning=FALSE, message=FALSE----
testResiduals(peake.resid)


## ----fitMode2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.glm <- glm(INDIV ~ log(AREA), data=peake, family=poisson(link='log'))


## ----validateModel2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, messsage=FALSE, warning=FALSE----
autoplot(peake.glm, which=1:6)


## ----validateModel2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
influence.measures(peake.glm)


## ----validateModel2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
performance::check_model(peake.glm)


## ----validateModel2cc, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=4, warning=FALSE, message=FALSE----
performance::check_overdispersion(peake.glm)
performance::check_zeroinflation(peake.glm) 
performance::check_normality(peake.glm) %>% plot
performance::check_outliers(peake.glm)


## ----validateModel2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
peake.resid <- simulateResiduals(peake.glm,  plot=TRUE)


## ----validateModel2e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4, warning=FALSE, message=FALSE----
testResiduals(peake.resid)


## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
peake.ss <- sum(resid(peake.glm, type = "pearson")^2)
1 - pchisq(peake.ss, peake.glm$df.resid)
##Evidence of a lack of fit

#Deviance
1-pchisq(peake.glm$deviance, peake.glm$df.resid)
#Evidence of a lack of fit


## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.ss/peake.glm$df.resid
peake.glm$deviance/peake.glm$df.resid


## ----fitMode3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.glm1 <- glm.nb(INDIV ~ log(AREA), data=peake)
## lets also fit a model in which we have centered the predictor to see the impact on estimated coefficients.
peake.glm2 <- glm.nb(INDIV ~ scale(log(AREA), scale=FALSE), data=peake)


## ----validateModel3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, messsage=FALSE, warning=FALSE----
autoplot(peake.glm1, which=1:6)


## ----validateModel3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
influence.measures(peake.glm1)


## ----validateModel3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
performance::check_model(peake.glm1)


## ----validateModel3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6, warning=FALSE, message=FALSE----
peake.resid <- simulateResiduals(peake.glm1,  plot=TRUE)


## ----validateModel3e, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4, warning=FALSE, message=FALSE----
testResiduals(peake.resid)


## ----validateModel3f, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
##Check the model for lack of fit via:
##Pearson chisq
peake.ss <- sum(resid(peake.glm1, type = "pearson")^2)
1 - pchisq(peake.ss, peake.glm1$df.resid)
##Evidence of a lack of fit

#Deviance
1-pchisq(peake.glm1$deviance, peake.glm1$df.resid)
#Evidence of a lack of fit


## ----validateModel3g, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
peake.ss/peake.glm1$df.resid


## ----aicModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
AIC(peake.glm, peake.glm1)
## For small sample sizes,  it is better to use AICc - this is
## corrected for small sample sizes.
AICc(peake.glm, peake.glm1)


## ----plotModel1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
## The following is equivalent to ggeffect
plot_model(peake.glm1, type='eff', show.data=FALSE,  terms='AREA') 
#plot_model(peake.glm1, type='eff', show.data=FALSE,  terms='AREA [exp]') 
## The following is equivalent to ggpredict
#plot_model(peake.glm1, type='pred',  show_data=TRUE, terms='AREA [exp]')
## The following is equivalent to ggemmeans
#plot_model(peake.glm1, type='emm',  terms='AREA [exp]')



## ----plotModela1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
plot(allEffects(peake.glm1, residuals=TRUE), type='response')


## ----plotModel1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
ggpredict(peake.glm1) %>% plot(add.data=TRUE)


## ----plotModel1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=5, fig.height=5----
ggemmeans(peake.glm1,  ~AREA) %>% plot(add.data=TRUE)


## ----summaryModel1a, results='markdown', eval=TRUE, hidden=TRUE---------------
summary(peake.glm1)


## ----summaryModel1b, results='markdown', eval=TRUE, hidden=TRUE---------------
confint(peake.glm1)
## or on the response scale
exp(confint(peake.glm1))


## ----summaryModel1c, results='markdown', eval=TRUE, hidden=TRUE---------------
tidy(peake.glm1, conf.int=TRUE)
tidy(peake.glm1, conf.int=TRUE, exponentiate=TRUE)
glance(peake.glm1)


## ----summaryModel1d, results='markdown', eval=TRUE, hidden=TRUE---------------
# warning this is only appropriate for html output
sjPlot::tab_model(peake.glm1,show.se=TRUE,show.aic=TRUE)


## ----predictModel1a, results='markdown', eval=TRUE, hidden=TRUE---------------
#R2
1-(peake.ss/peake.glm1$null)
## Or based on deviance (preferred)
1-(peake.glm1$deviance/peake.glm1$null)


## ----predictModel1b, results='markdown', eval=TRUE, hidden=TRUE---------------
r.squaredLR(peake.glm1)


## ----predictModel1c, results='markdown', eval=TRUE, hidden=TRUE---------------
performance::r2_nagelkerke(peake.glm1)


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
peake.grid = with(peake, list(AREA=seq(min(AREA), max(AREA), len=100)))
#OR
peake.grid = peake %>% data_grid(AREA=seq_range(AREA,  n=100))
newdata = emmeans(peake.glm1, ~AREA, at=peake.grid, type='response') %>%
    as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    theme_classic()

ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=peake, aes(y=INDIV)) +
    scale_x_log10(breaks = as.vector(c(1,2,5,10) %o% 10^(-1:4))) +
    scale_y_log10() +
    theme_classic()
## If we want to plot the partial observations
partial.obs = emmeans(peake.glm1, ~AREA, at=peake, type='response') %>%
    as.data.frame %>%
    mutate(response=response+resid(peake.glm1,type='response'))

## response residuals are just resid * mu.eta(predict)
ggplot(newdata, aes(y=response, x=AREA)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL),fill='blue', alpha=0.3) +
    geom_line() +
    geom_point(data=peake, aes(y=INDIV)) +
    geom_point(data=partial.obs, aes(y=response), color='green') + 
    scale_x_log10(breaks = as.vector(c(1,2,5,10) %o% 10^(-1:4))) +
    scale_y_log10() +
    theme_classic()	


