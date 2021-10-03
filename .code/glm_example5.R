## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')


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
    geom_boxplot() + 
    geom_point(color='red')
ggplot(day, aes(y=BARNACLE, x=TREAT)) +
    geom_violin() +
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
day.glm %>% autoplot()


## ----validateModel1b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.glm1 %>% autoplot()


## ----validateModel2a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.glm %>% influence.measures()


## ----validateModel2b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.glm1 %>% influence.measures()


## ----validateModel3a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.glm %>% performance::check_model()


## ----validateModel3b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.glm1 %>% performance::check_model()


## ----validateModel4a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.resids <- day.glm %>% simulateResiduals(plot=TRUE)
day.resids %>% testDispersion()


## ----validateModel4b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
day.resids <- day.glm1 %>% simulateResiduals(plot=TRUE)
day.resids %>% testDispersion()


## ----validateModel5a, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
## overdispersion
1-pchisq(day.glm$deviance, day.glm$df.residual)
day.glm$deviance/day.glm$df.residual


## ----validateModel5b, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
1-pchisq(day.glm1$deviance, day.glm1$df.residual)
day.glm1$deviance/day.glm1$df.residual


## ----partialPlots1a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm %>% plot_model(type='eff', show.data=TRUE)


## ----partialPlots1b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm1 %>% plot_model(type='eff', show.data=TRUE)


## ----partialPlots2a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm %>% allEffects() %>% plot


## ----partialPlots2b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm1 %>% allEffects() %>% plot
allEffects(day.glm1, transformation=NULL) %>% plot


## ----partialPlots3a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm %>% ggpredict() %>% plot(add.data=TRUE, jitter=FALSE)


## ----partialPlots3b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm1 %>% ggpredict() %>% plot(add.data=TRUE, jitter=FALSE)
## back.transform only applies to response transformations, not link functions
## day.glm1 %>% ggpredict(back.transform = FALSE) %>% plot()


## ----partialPlots4a, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE, jitter=FALSE)


## ----partialPlots4b, results='markdown', eval=TRUE, fig.width=5, fig.height=5----
day.glm1 %>% ggemmeans(~TREAT) %>% plot(add.data=TRUE, jitter=FALSE)
## back.transform only applies to response transformations, not link functions
## day.glm1 %>% ggemmeans(back.transform = FALSE) %>% plot()


## ----summariseModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------
day.glm %>% summary()


## ----summariseModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------
day.glm %>% confint()


## ----summariseModelc, results='markdown', eval=TRUE, hidden=TRUE--------------
day.glm %>% tidy(conf.int=TRUE)


## ----summaryModel1d, results='markdown', eval=TRUE, hidden=TRUE---------------
# warning this is only appropriate for html output
day.glm %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summariseModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------
day.glm1 %>% summary()


## ----summariseModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------
day.glm1 %>% confint()
day.glm1 %>% confint() %>% exp()


## ----summariseMode2c, results='markdown', eval=TRUE, hidden=TRUE--------------
day.glm1 %>% tidy(conf.int=TRUE)
day.glm1 %>% tidy(conf.int=TRUE, exponentiate=TRUE) 


## ----summariseMode2d, results='markdown', eval=TRUE, hidden=TRUE--------------
day.glm1 %>% tidy(conf.int=TRUE, exponentiate=TRUE) %>%
  kable()


## ----summaryModel3a, results='markdown', eval=TRUE, hidden=TRUE---------------
# warning this is only appropriate for html output
day.glm1 %>% sjPlot::tab_model(show.se=TRUE,show.aic=TRUE)


## ----predictions, results='markdown', eval=TRUE, hidden=TRUE------------------

day.glm1 %>% emmeans(pairwise ~TREAT, type='response')
day.glm1 %>% emmeans(~TREAT, type='response') %>% pairs()
day.glm1 %>% emmeans(~TREAT, type='response') %>% pairs() %>% confint()
day.glm1 %>% emmeans(~TREAT, type='response') %>% pairs() %>% summary(infer=TRUE)
## Lets store this for late

day.pairwise <- day.glm1 %>%
    emmeans(~TREAT, type='response') %>%
    pairs() %>%
    summary(infer=TRUE) %>%
    as.data.frame()


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
day.glm1 %>% emmeans(~TREAT, type='response') %>%
    contrast(method=list(TREAT=cmat)) %>%
    summary(infer=TRUE)

## what about absolute differences
day.glm1 %>% emmeans(~TREAT, type='link') %>%
    regrid() %>%
    contrast(method=list(TREAT=cmat)) %>%
    summary(infer=TRUE)


## ----planned3, results='markdown', eval=TRUE,echo=FALSE-----------------------
day.planned <- day.glm1 %>% emmeans(~TREAT, type='response') %>%
    contrast(method=list(TREAT=cmat)) %>%
    summary(infer=TRUE)
## day.planned <- day.glm1 %>%
##   emmeans(~TREAT, contr=list(TREAT=cmat), type='response') %>%
##   "["('contrasts') %>%
##   as.data.frame


## ----summaryFig1a, results='markdown', eval=TRUE, hidden=TRUE-----------------
newdata <- day.glm1 %>% emmeans(~TREAT, type='response') %>%
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

