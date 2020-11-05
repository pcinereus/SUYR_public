## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE,  warning=FALSE, message=FALSE)


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(emmeans)   #for estimating marginal means
library(ggeffects) #for plotting marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(DHARMa)    #for residual diagnostics plots
library(patchwork) #grid of plots
library(scales)    #for more scales


## ----readData, results='markdown', eval=TRUE----------------------------------
day = read_csv('../data/day.csv', trim_ws=TRUE)
glimpse(day)
day <- day %>%
  mutate(TREAT = factor(TREAT))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, hidden=TRUE----
ggplot(day, aes(y=BARNACLE, x=TREAT)) +
    geom_boxplot()+
    geom_point(color='red')
ggplot(day, aes(y=BARNACLE, x=TREAT)) +
    geom_violin()+
    geom_point(color='red')


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
#Effects model
## start by dummy coding the categorical predictor and expressing
## the treatment effects as a model matrix.
Xmat <- model.matrix(~TREAT, data=day)
Xmat %>% head
##latex-math-preview-expression
# solve(X'X)X'Y
solve(t(Xmat) %*% Xmat) %*% t(Xmat) %*% day$BARNACLE



## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
day.glm <- glm(BARNACLE~TREAT, data=day, family='gaussian')
day.glm1 <- glm(BARNACLE~TREAT, data=day, family='poisson')


## ----validateModel1a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(day.glm)


## ----validateModel1b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(day.glm1)


## ----validateModel2a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
influence.measures(day.glm)


## ----validateModel2b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
influence.measures(day.glm1)


## ----validateModel3a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
performance::check_model(day.glm)


## ----validateModel3b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
performance::check_model(day.glm1)


## ----validateModel4a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.resids <- simulateResiduals(day.glm, plot=TRUE)


## ----validateModel4b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.resids <- simulateResiduals(day.glm1, plot=TRUE)


## ----validateModel5a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
## overdispersion
1-pchisq(day.glm$deviance,day.glm$df.residual)
day.glm$deviance/day.glm$df.residual


## ----validateModel5b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
1-pchisq(day.glm1$deviance,day.glm1$df.residual)
day.glm1$deviance/day.glm1$df.residual


## ----partialPlots1a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
plot_model(day.glm,  type='eff')


## ----partialPlots1b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
plot_model(day.glm1,  type='eff')


## ----partialPlots2a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
allEffects(day.glm) %>% plot


## ----partialPlots2b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
allEffects(day.glm1) %>% plot
allEffects(day.glm1, transformation=NULL) %>% plot


## ----partialPlots3a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
ggpredict(day.glm) %>% plot


## ----partialPlots3b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
ggpredict(day.glm1) %>% plot
## The following should not back transform,  yet it seems to
ggpredict(day.glm1, back.transform = FALSE) %>% plot


## ----partialPlots4a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
ggemmeans(day.glm,  ~TREAT) %>% plot


## ----partialPlots4b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
ggemmeans(day.glm1, ~TREAT) %>% plot
## The following should not back transform,  yet it seems to
ggemmeans(day.glm1, ~TREAT, back.transform = FALSE) %>% plot


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(day.glm)


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------
confint(day.glm)


## ----summariseModelc, results='markdown', eval=TRUE, hidden=TRUE--------------
tidy(day.glm,conf.int=TRUE)


## ----summaryModel1d, results='markdown', eval=TRUE, hidden=TRUE---------------
# warning this is only appropriate for html output
sjPlot::tab_model(day.glm,show.se=TRUE,show.aic=TRUE)


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------
summary(day.glm1)


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------
confint(day.glm1)
exp(confint(day.glm1))


## ----summariseMode2c, results='markdown', eval=TRUE, hidden=TRUE--------------
tidy(day.glm1,conf.int=TRUE)
tidy(day.glm1,conf.int=TRUE, exponentiate=TRUE) 


## ----summariseMode2d, results='markdown', eval=TRUE, hidden=TRUE--------------
tidy(day.glm1,conf.int=TRUE, exponentiate=TRUE) %>%
  kable()


## ----summaryModel3a, results='markdown', eval=TRUE, hidden=TRUE---------------
# warning this is only appropriate for html output
sjPlot::tab_model(day.glm1,show.se=TRUE,show.aic=TRUE)


## ----predictions, results='markdown', eval=TRUE, hidden=TRUE------------------
day.glm1 %>% emmeans(pairwise ~TREAT, type='response')
day.glm1 %>% emmeans(pairwise ~TREAT, type='response') %>% confint()
## Lets store this for late
day.pairwise <- (day.glm1 %>%
  emmeans(pairwise ~TREAT, type='response') %>%
  confint())$contrasts %>%
  as.data.frame


## ----planned, results='markdown', eval=TRUE, hidden=TRUE----------------------
##      Alg1_Alg2 NB_S Alg_Bare
## ALG1         1    0      0.5
## ALG2        -1    0      0.5
## NB           0    1     -0.5
## S            0   -1     -0.5
cmat<-(cbind('Alg1_Alg2'=c(1,-1,0,0),
              'NB_S'=c(0,0,1,-1),
             'Alg_Bare'=c(0.5,0.5,-0.5,-0.5)))
cmat


## ----planned1, results='markdown', eval=TRUE, hidden=TRUE---------------------
crossprod(cmat)


## ----planned2, results='markdown', eval=TRUE, hidden=TRUE---------------------
day.glm1 %>% emmeans(~TREAT, contr=list(TREAT=cmat), type='response')
## or with confidence intervals
day.glm1 %>% emmeans(~TREAT, contr=list(TREAT=cmat), type='response') %>% confint

## ----planned3, results='markdown', eval=TRUE,echo=FALSE-----------------------
day.planned <- day.glm1 %>%
  emmeans(~TREAT, contr=list(TREAT=cmat), type='response') %>%
  "["('contrasts') %>%
  as.data.frame


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
newdata = emmeans(day.glm1, ~TREAT, type='response') %>%
    as.data.frame
newdata
## A quick version
ggplot(newdata, aes(y=rate, x=TREAT)) +
    geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL)) +
    theme_classic()


## ----summaryFig1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
g1 <- ggplot(newdata, aes(y=rate, x=TREAT)) +
    geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL))+
    geom_point()+
    scale_x_discrete('Treatment', breaks=c('ALG1','ALG2','NB','S'),
       labels=c('Algae spp 1', 'Algae spp 2', 'Naturally bare', 'Scraped bare'))+
    scale_y_continuous(expression(Number~of~newly~recruited~barnacles~(cm^2)))+
    theme_classic()

g2 <- day.pairwise %>%
  ggplot(aes(y=ratio,  x=contrast)) +
  geom_hline(yintercept=1,  linetype='dashed') +
  geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL)) +
  scale_y_continuous(trans=scales::log2_trans(), breaks=scales::breaks_log(base=2)) +
  coord_flip(ylim=c(0.25, 4)) +
  theme_classic()

g1 + g2


## ----summaryFig2a, results='markdown', eval=TRUE, hidden=TRUE-----------------
newdata = with(day,  data.frame(TREAT=levels(TREAT)))
Xmat <- model.matrix(~TREAT,  newdata)
coefs <- coef(day.glm1)

fit <- as.vector(coefs %*% t(Xmat))
se <- sqrt(diag(Xmat %*% vcov(day.glm1) %*% t(Xmat)))
q <- qt(0.975, df=df.residual(day.glm1))
newdata = newdata %>%
  mutate(Fit = exp(fit),
         Lower=exp(fit - q*se),
         Upper=exp(fit+q*se))
## A quick version
ggplot(newdata, aes(y=Fit, x=TREAT)) +
    geom_pointrange(aes(ymin=Lower, ymax=Upper)) +
    theme_classic()


## ----summaryFig2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
g1 <- ggplot(newdata, aes(y=Fit, x=TREAT)) +
    geom_pointrange(aes(ymin=Lower, ymax=Upper))+
    geom_point()+
    scale_x_discrete('Treatment', breaks=c('ALG1','ALG2','NB','S'),
       labels=c('Algae spp 1', 'Algae spp 2', 'Naturally bare', 'Scraped bare'))+
    scale_y_continuous(expression(Number~of~newly~recruited~barnacles~(cm^2)))+
    theme_classic()

g2 <- day.pairwise %>%
  ggplot(aes(y=ratio,  x=contrast)) +
  geom_hline(yintercept=1,  linetype='dashed') +
  geom_pointrange(aes(ymin=asymp.LCL,  ymax=asymp.UCL)) +
  scale_y_continuous(trans=scales::log2_trans(), breaks=scales::breaks_log(base=2)) +
  coord_flip(ylim=c(0.25, 4)) +
  theme_classic()

g1 + g2

