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
library(tidyverse) #for data wrangling
library(modelr)    #for auxillary modelling functions
library(DHARMa)    #for residual diagnostics plots


## ----readData, results='markdown', eval=TRUE----------------------------------
loyn = read_csv('../data/loyn.csv', trim_ws=TRUE)
glimpse(loyn)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
scatterplotMatrix(~ABUND+DIST+LDIST+AREA+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))
				  
scatterplotMatrix(~ABUND+log(DIST)+log(LDIST)+log(AREA)+GRAZE+ALT+YR.ISOL, data=loyn,
                  diagonal = list(method='boxplot'))


## ----prepareData, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
loyn = loyn %>% mutate(fGRAZE=factor(GRAZE))


## ----EDA1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
ggplot(loyn, aes(y=ABUND, x=AREA, color=fGRAZE)) +
    geom_smooth(method='lm') +
    scale_x_log10()

ggplot(loyn, aes(y=ABUND, x=DIST, color=fGRAZE)) +
    geom_smooth(method='lm') +
    scale_x_log10()


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
loyn.lm<-lm(ABUND~scale(log(DIST))+scale(log(LDIST))+scale(log(AREA))+
                fGRAZE + scale(ALT) + scale(YR.ISOL), data=loyn)


## ----validateModel1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
vif(loyn.lm)
## If any terms have df>1, then Generalized VIF calculated
## This is the inflation in size compared to what would be expected if orthogonal



## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
autoplot(loyn.lm, which=1:6)
loyn.resid <- simulateResiduals(loyn.lm)
plot(loyn.resid)
testResiduals(loyn.resid)


## ----validateModel3, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8----
plot(allEffects(loyn.lm, residuals=TRUE), type='response')
plot_grid(plot_model(loyn.lm,  type='eff'))
## We can sort of do part of the backtransforms - but only partly..
plot_grid(list(
  plot_model(loyn.lm,  type='eff',  terms='DIST [exp]'),
  plot_model(loyn.lm,  type='eff',  terms='LDIST [exp]'),
  plot_model(loyn.lm,  type='eff',  terms='AREA [exp]'),
  plot_model(loyn.lm,  type='eff',  terms='fGRAZE'),
  plot_model(loyn.lm,  type='eff',  terms='ALT'),
  plot_model(loyn.lm,  type='eff',  terms='YR.ISOL')  
))
plot_model(loyn.lm,  type='est', transform='c')

## ----summaryModel1, results='markdown', eval=TRUE, hidden=TRUE----------------
summary(loyn.lm)
confint(loyn.lm)
tidy(loyn.lm, conf.int=TRUE)
#emtrends(loyn.lm,  ~DIST+LDIST+AREA+fGRAZE+ALT+YR.ISOL,  var='AREA')
glance(loyn.lm)


## ----modelSelection1, results='markdown', eval=TRUE, hidden=TRUE--------------
summary(loyn.lm)$'adj.r.squared'
AIC(loyn.lm)
AICc(loyn.lm)


## ----modelSelection2, results='markdown', eval=TRUE, hidden=TRUE--------------
# Option 1 - dredge
loyn.lm = update(loyn.lm, na.action=na.fail)
#options(width=1000)
dredge(loyn.lm, rank = "AICc")


## ----modelSelection3, results='markdown', eval=TRUE, hidden=TRUE--------------
## 2. model averaging
loyn.av<-model.avg(dredge(loyn.lm, rank = "AICc"),
                     subset=delta<=4)
summary(loyn.av)
## - parameter is assumed to be present in all models (it is set to zero when not present)
## - conditional averages only average over the models in which the parameter appears
##   these estimates have a tendency to be biased away from zero 
confint(loyn.av)


## ----modelSelection4, results='markdown', eval=TRUE, hidden=TRUE--------------
loyn.lm1<-update(loyn.lm, .~scale(log(DIST))+scale(log(LDIST)))
loyn.lm2<-update(loyn.lm, .~scale(log(AREA)) + fGRAZE + scale(YR.ISOL))
loyn.lm3<-update(loyn.lm, .~scale(log(AREA)) + fGRAZE)
loyn.lm4<-update(loyn.lm, .~scale(ALT))
loyn.null<-update(loyn.lm, .~1)
AICc(loyn.lm, loyn.lm1, loyn.lm2, loyn.lm3,loyn.lm4, loyn.null) %>%
    mutate(Model=row.names(.), delta=AICc-AICc[6]) %>%
    dplyr::select(Model, everything())
## Support for model 2, 3 and 4, no support for model 1


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
newdata = list(fGRAZE=levels(loyn$fGRAZE),
                             AREA=seq_range(loyn$AREA,  n=100))
newdata = emmeans(loyn.lm3,  ~AREA|fGRAZE,  at=newdata) %>%
  as.data.frame
# OR
newdata = emmeans(loyn.lm3, ~AREA|fGRAZE,
        at=list(fGRAZE=levels(loyn$fGRAZE),
                AREA=seq(min(loyn$AREA), max(loyn$AREA), len=100))) %>%
    as.data.frame
head(newdata)

ggplot(newdata, aes(y=emmean, x=AREA, color=fGRAZE, fill=fGRAZE)) +
    geom_ribbon(aes(ymin=lower.CL, ymax=upper.CL), color=NA, alpha=0.3) +
    geom_line() +
    scale_x_log10(labels=scales::comma)+
    scale_y_continuous('Abundance') +
    theme_classic()



## ----figureModel2, results='markdown', eval=TRUE, hidden=TRUE-----------------
## Predicted Abundance less than 0
loyn.glm<-glm(ABUND~scale(log(AREA)) * fGRAZE, data=loyn, family=gaussian(link='log'))
loyn.glm1<-glm(ABUND~scale(log(AREA)) + fGRAZE, data=loyn, family=gaussian(link='log'))
#loyn.glm<-glm(ABUND~scale(log(AREA)) * fGRAZE, data=loyn, family=Gamma(link='log'))
#loyn.glm1<-glm(ABUND~scale(log(AREA)) + fGRAZE, data=loyn, family=Gamma(link='log'))
AICc(loyn.glm, loyn.glm1) %>%
  mutate(Model=row.names(.),
         delta=AICc-min(AICc)) %>%
  dplyr::select(Model, everything())
loyn.resid <- simulateResiduals(loyn.glm)
plot(loyn.resid)
summary(loyn.glm)
tidy(loyn.glm)

loyn.grid = with(loyn,  list(fGRAZE=levels(fGRAZE),
                             AREA=seq_range(AREA, n=100)))

newdata = emmeans(loyn.glm, ~AREA|fGRAZE,
                  at=loyn.grid,
                  type='response') %>%
    as.data.frame
head(newdata) 

ggplot(newdata, aes(y=response, x=AREA, color=fGRAZE, fill=fGRAZE)) +
    geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), color=NA, alpha=0.3) +
    geom_line() +
    scale_x_log10(labels=scales::comma)+
    scale_y_log10('Abundance') +
    theme_classic()

