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
quinn = read_csv('../data/quinn.csv', trim_ws=TRUE)
glimpse(quinn)
summary(quinn)


## ----dataprep, results='markdown', eval=TRUE, hidden=TRUE---------------------
quinn = quinn %>% mutate(SEASON = factor(SEASON),
                         DENSITY = factor(DENSITY))


## ----eda, hidden=TRUE---------------------------------------------------------
ggplot(quinn, aes(y=RECRUITS, x=SEASON, fill=DENSITY)) +
     geom_boxplot()

ggplot(quinn, aes(y=RECRUITS, x=SEASON, fill=DENSITY)) +
  geom_boxplot() +
  scale_y_log10()


## ----fitModel, hidden=TRUE----------------------------------------------------
quinn.glm <- glm(log(RECRUITS+1) ~ DENSITY*SEASON, data=quinn, family=gaussian)
quinn.glm <- glm(RECRUITS ~ DENSITY*SEASON, data=quinn,
                  family=poisson(link='log'))


## ----ValidateModel, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(quinn.glm,which=1:6)


## ----modelValidation2, results='markdown', eval=TRUE, hidden=TRUE-------------
## goodness of fit
1-pchisq(quinn.glm$deviance, df=quinn.glm$df.residual)
## any evidence of overdispersion
quinn.glm$deviance/quinn.glm$df.residual

quinn.resid = simulateResiduals(quinn.glm,  plot=TRUE)
plot(quinn.resid)
testResiduals(quinn.resid)
testDispersion(quinn.resid)
testZeroInflation(quinn.resid)
#testTemporalAutocorrelation(quinn.glm1)


## ----modelValidation3, results='markdown', eval=TRUE, hidden=TRUE-------------
quinn %>% group_by(SEASON, DENSITY) %>%
  summarise(Zeros=sum(RECRUITS==0), 
            Prop=Zeros/n(),
            Mean=mean(RECRUITS))
x=rpois(100000,lambda=2.67)
tab.1 = table(x==0)
tab.1/sum(tab.1)

## OR,  over the entire data
## is this due to excessive zeros (zero-inflation)
tab=table(quinn$RECRUITS==0)
tab/sum(tab)
## 5% is not many.. so it cant be zero-inflated
## how many 0's would we expect from a poisson distribution with a mean similar to our mean
mean(quinn$RECRUITS)
x=rpois(100000,lambda=mean(quinn$RECRUITS))
tab.1 = table(x==0)
tab.1/sum(tab.1)


## ----fitModel2, results='markdown', eval=TRUE, hidden=TRUE--------------------
library(MASS)
quinn.nb = glm.nb(RECRUITS ~ DENSITY*SEASON, data=quinn)


## ----modelValidation4, results='markdown', eval=TRUE, fig.width=7, fig.height=7, hidden=TRUE----
autoplot(quinn.nb,which=1:6)
quinn.resid = simulateResiduals(quinn.nb,  plot=TRUE)
plot(quinn.resid)
testResiduals(quinn.resid)
testDispersion(quinn.resid)
testZeroInflation(quinn.resid)


## ----modelValidation5, results='markdown', eval=TRUE, hidden=TRUE-------------
## goodness of fit
1-pchisq(quinn.nb$deviance, df=quinn.nb$df.residual)
## any evidence of overdispersion
quinn.nb$deviance/quinn.nb$df.residual


## ----modelValidation6, results='markdown', eval=TRUE, hidden=TRUE-------------
AICc(quinn.glm, quinn.nb)


## ----partialplots, results='markdown', eval=TRUE, hidden=TRUE-----------------
plot(allEffects(quinn.nb),multiline=TRUE, ci.style='bar')
plot(allEffects(quinn.nb),multiline=TRUE, ci.style='bar', type='link')
plot_model(quinn.nb, type='eff',  terms=c('SEASON', 'DENSITY'))

ggpredict(quinn.nb, c('SEASON', 'DENSITY')) %>% plot()
ggemmeans(quinn.nb,  ~SEASON*DENSITY) %>% plot()
plot(allEffects(quinn.nb),multiline=TRUE, ci.style='bar', type='link')
summary(quinn.nb)
tidy(quinn.nb, conf.int=TRUE)
tidy(quinn.nb, conf.int=TRUE, exponentiate=TRUE)


## ----mainEffects, results='markdown', eval=TRUE, hidden=TRUE------------------
quinn.nb %>% emmeans(pairwise~DENSITY|SEASON)

quinn.nb %>% emmeans(pairwise~DENSITY|SEASON, type='response')

quinn.nb %>% emmeans(pairwise~DENSITY|SEASON,  type='response') %>%
  confint

eff <- (quinn.nb %>%
        emmeans(pairwise~DENSITY|SEASON,  type='response') %>%
        confint)$contrasts %>%
               as.data.frame

eff %>%
  ggplot(aes(y=ratio, x=SEASON)) +
  geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL)) +
  geom_hline(yintercept=1, linetype='dashed') +
  scale_x_discrete(name='') +
  scale_y_continuous(name='Density effect (High vs Low)', trans=scales::log2_trans(),
                     breaks=scales::breaks_log(base=2)) +
  coord_flip(ylim=c(0.25, 4)) +
  theme_classic()


## ----summaryFig, results='markdown', eval=TRUE, hidden=TRUE-------------------

newdata = emmeans(quinn.nb, ~DENSITY|SEASON, type='response') %>% as.data.frame
head(newdata)
ggplot(newdata, aes(y=response, x=SEASON, fill=DENSITY)) +
  geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL), shape=21,
                  position=position_dodge(width=0.2)) +
  theme_classic() +
  annotate(geom='text', x='Summer', y=70, label='*', size=7)


## ----junk, results='markdown', eval=TRUE, hidden=TRUE-------------------------
newdata = emmeans(quinn.nb, ~DENSITY|SEASON, type='response') %>% as.data.frame
head(newdata)
newdata = newdata %>% mutate(SEASON=factor(SEASON, levels=c('Autumn','Winter','Spring','Summer')))
ggplot(newdata, aes(y=response, x=SEASON, fill=DENSITY)) +
    geom_pointrange(aes(ymin=asymp.LCL, ymax=asymp.UCL), shape=21,
                    position=position_dodge(width=0.2)) +
    theme_classic() +
    annotate(geom='text', x='Summer', y=70, label='*', size=7) +
    scale_y_continuous('Number of Mussel recruits') +
    theme(axis.title.x=element_blank(),
          legend.position=c(0.01,1), legend.justification = c(0,1)) +
    scale_fill_manual('Density',values=c('white','black'))
    

