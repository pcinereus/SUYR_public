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
library(ggeffects) #for plotting marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots
library(patchwork) #grid of plots
library(scales)    #for more scales


## ----readData, results='markdown', eval=TRUE----------------------------------
day = read_csv('../data/day.csv', trim_ws=TRUE)
glimpse(day)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8, hidden=TRUE----
ggplot(day, aes(y=BARNACLE, x=TREAT)) +
    geom_boxplot()+
    geom_point(color='red')
ggplot(day, aes(y=BARNACLE, x=TREAT)) +
    geom_violin()+
    geom_point(color='red')


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
#Effects model
#contrasts(day$TREAT)
model.matrix(~TREAT, data=day)

mm <- model.matrix(~TREAT, data=day)

##latex-math-preview-expression
##$Barnacle_{i} = \mu + \alpha_{Treatment_j} +\varepsilon_i \hspace{1cm} \varepsilon \sim{} \mathcal{N}(0, \sigma^2)$
#we could solve the whole thing as
# solve(X'X)X'Y
solve(t(mm) %*% mm) %*% t(mm) %*% day$BARNACLE



## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
day.glm <- glm(BARNACLE~TREAT, data=day, family='gaussian')
day.glm1 <- glm(BARNACLE~TREAT, data=day, family='poisson')


## ----validateModel, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(day.glm)


## ----validateModel1, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(day.glm1, which=1:6)
day.resid <- simulateResiduals(day.glm1)
plot(day.resid)


## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE---------------
## overdispersion
1-pchisq(day.glm1$deviance,day.glm1$df.residual)
day.glm1$deviance/day.glm1$df.residual


## ----AIC, results='markdown', eval=TRUE, hidden=TRUE--------------------------
## AIC(day,glm, day.glm1)


## ----effectsPlot, results='markdown', eval=TRUE, hidden=TRUE------------------
plot(allEffects(day.glm))
plot_model(day.glm, type='eff')
plot_model(day.glm,  type='est')


## ----summariseModel, results='markdown', eval=TRUE, hidden=TRUE---------------
summary(day.glm)
tidy(day.glm,conf.int=TRUE) #Gaussian
anova(day.glm, test='Chisq')


## ----effectsPlot1, results='markdown', eval=TRUE, hidden=TRUE-----------------
plot(allEffects(day.glm1))
plot(allEffects(day.glm1), type='link')
plot_model(day.glm1, type='eff')
plot_model(day.glm1, type='est')
ggpredict(day.glm1,  terms='TREAT') %>% plot
ggemmeans(day.glm1,  ~TREAT) %>% plot


## ----summariseModel1, results='markdown', eval=TRUE, hidden=TRUE--------------
summary(day.glm1)
tidy(day.glm1,conf.int=TRUE) #Poisson
tidy(day.glm1,conf.int=TRUE, exponentiate=TRUE) #Poisson
exp(coef(day.glm1))
##log(A) + log(B) = log(A*B)
##exp(A)*exp(B) = exp(A+B)
22.4+6
22.4*1.268
summary(day.glm1)
confint(day.glm1)
anova(day.glm1, test='Chisq')



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
crossprod(cmat)
#emmeans(day.lm, ~TREAT, contr=list(TREAT=cmat))
                                        #confint(emmeans(day.lm, ~TREAT, contr=list(TREAT=cmat)))
#emmeans(day.glm1, ~TREAT, contr=list(TREAT=cmat), type='link')
day.glm1 %>% emmeans(~TREAT, contr=list(TREAT=cmat), type='response')
day.glm1 %>% emmeans(~TREAT, contr=list(TREAT=cmat), type='response') %>% confint


## ----summaryFig, results='markdown', eval=TRUE, hidden=TRUE-------------------
newdata = emmeans(day.glm1, ~TREAT, type='response') %>%
    as.data.frame
newdata
ggplot(newdata, aes(y=rate, x=TREAT)) +
    geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL)) +
    theme_classic()

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

