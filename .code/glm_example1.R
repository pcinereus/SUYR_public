## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')
options(tinytex.engine = 'xelatex')


## ----libraries, results='markdown', eval=TRUE, warning=TRUE, message=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(DHARMa)    #for residual diagnostics
library(performance) #for residuals diagnostics
library(see)         #for plotting residuals
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for partial effects plots
library(emmeans)   #for estimating marginal means
library(modelr)    #for auxillary modelling functions
library(tidyverse) #for data wrangling


## ----readData, results='markdown', eval=TRUE----------------------------------
fert = read_csv('../data/fertilizer.csv', trim_ws=TRUE)


## ----examinData---------------------------------------------------------------
glimpse(fert)
## Explore the first 6 rows of the data
head(fert)
str(fert)


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, message=FALSE-----------
ggplot(fert, aes(y = YIELD, x = FERTILIZER)) +
  geom_point() +
  geom_smooth()
ggplot(fert, aes(y = YIELD, x = FERTILIZER)) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(fert, aes(y = YIELD)) +
  geom_boxplot(aes(x = 1))


## ----name, results='markdown', eval=TRUE--------------------------------------
## Generate the model matrix
X <- model.matrix(~FERTILIZER, data = fert)
## Solve for beta
beta <- solve(t(X) %*% X) %*% t(X) %*% fert$YIELD
beta


## ----fitModel, results='markdown', eval=TRUE, hidden=TRUE---------------------
fert.lm <- lm(YIELD~1+FERTILIZER, data = fert)
fert.lm <- lm(YIELD~FERTILIZER, data = fert)


## ----summaryModel, results='markdown', eval=TRUE, echo=TRUE-------------------
## Get the name of the attributes within the lm model
attributes(fert.lm)
## Explore all the conttents of the fitted model
str(fert.lm)
## Return the data used to fit the model 
fert.lm$model
## Return the estimated coefficients
fert.lm$coefficients


## ----summaryModel1, results='markdown', eval=TRUE, echo=TRUE------------------
## Extract the estimated coefficients
coef(fert.lm)
## Extract the fitted (predicted) values
fitted(fert.lm)
## Extract the residules
resid(fert.lm)
args(residuals.lm)


## ----validateModel, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, message=FALSE, warning=FALSE----
fert.lm %>% autoplot(which = 1:6, ncol = 2, label.size = 3)


## ----validateModela, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
fert.lm %>% influence.measures()


## ----validateModela1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6, warning=FALSE, message=FALSE----
fert.lm %>% performance::check_model()
fert.lm %>% performance::check_outliers()
fert.lm %>% performance::check_outliers() %>% plot
## These are probabilities of exceedance rather than actual Cook's D values
#https://easystats.github.io/performance/reference/check_outliers.html


## ----validateModelb, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=6----
fert.resid <- fert.lm %>% simulateResiduals(plot = TRUE)


## ----validateModelc, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=4----
## To run tests of KS (uniformity),  dispersion and outliers
fert.resid %>% testResiduals()


## ----validateModeld, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
## OR individually
fert.resid %>% testUniformity()
fert.resid %>% testDispersion()
fert.resid %>% testOutliers()
## Other useful tests
fert.resid %>% testZeroInflation()
fert.resid %>% testQuantiles()
## The above fits quantile gams at 0.25,  0.5 and 0.75
## testSpatialAutocorrelation(fert.resid,  x=,  y=) # needs x and y coordinates
## testTemporalAutocorrelation(fert.resid,  time=) # needs time


## ----validateModele, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
fert.lm %>% augment()
## we could then pipe these to ggplot in order to look at residuals etc
fert.lm %>% augment() %>%
    ggplot() +
    geom_point(aes(y = .resid, x = .fitted))


## ----validateModelf, results='markdown', eval=TRUE, hidden=TRUE, fig.width=8, fig.height=8,message=FALSE,warning=FALSE----
fert.lm %>% plot_model(type='diag') %>% plot_grid()


## ----validateModel2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
fert.lm %>% plot_model(type = "eff", show.data = TRUE)


## ----validateModel4, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
plot(allEffects(fert.lm, residuals = TRUE))


## ----validateModel6, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
fert.lm %>% ggpredict() %>% plot(add.data = TRUE, jitter=FALSE)


## ----validateModel7, results='markdown', eval=TRUE, hidden=TRUE, fig.width=4, fig.height=4----
fert.lm %>% ggemmeans(~FERTILIZER) %>% plot(add.data = TRUE, jitter=FALSE)


## ----summaryModel2a, results='markdown', eval=TRUE, hidden=TRUE---------------
fert.lm %>% summary()


## ----summaryModel2b, results='markdown', eval=TRUE, hidden=TRUE---------------
fert.lm %>% confint()


## ----summaryModel2c, results='markdown', eval=TRUE, hidden=TRUE---------------
fert.lm %>% tidy(conf.int=TRUE)


## ----summaryModel2d, results='asis', eval=TRUE, hidden=TRUE-------------------
fert.lm %>% tidy(conf.int = TRUE) %>% kable


## ----summaryModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
# warning this is only appropriate for html output
fert.lm %>% sjPlot::tab_model(show.se = TRUE, show.aic = TRUE)


## ----predictModel2, results='markdown', eval=TRUE, hidden=TRUE----------------
## establish a data set that defines the new data to predict against
newdata = data.frame(FERTILIZER = 110)
## using the predict function
fert.lm %>% predict(newdata = newdata)
## include confidence intervals
fert.lm %>% predict(newdata = newdata,  interval = "confidence")


## ----predictModel, results='markdown', eval=TRUE, echo=TRUE, hidden=TRUE------
## establish a data set that defines the new data to predict against
newdata = data.frame(FERTILIZER = 110)
## Establish an appropriate model matrix
Xmat = model.matrix(~FERTILIZER, data = newdata)
## Perform matrix multiplication
(pred <- coef(fert.lm) %*% t(Xmat))
## Calculate the standard error
se <- sqrt(diag(Xmat %*% vcov(fert.lm) %*% t(Xmat)))
## Calculate the confidence intervals
as.numeric(pred) + outer(se, qt(df = df.residual(fert.lm),  c(0.025, 0.975)))


## ----predictModel3, results='markdown', eval=TRUE, hidden=TRUE----------------
## using emmeans
newdata = list(FERTILIZER = 110)
fert.lm %>% emmeans(~FERTILIZER, at = newdata)


## ----predictModel4, results='markdown', eval=TRUE, echo=TRUE, hidden=TRUE-----
## testing a specific hypothesis
## Probabiliy of getting our estimate if slope was 1
fert.lm %>% multcomp::glht(linfct = c("FERTILIZER == 1")) %>% summary
## Cant ask probability that the slope is equal to something in frequentist
## If we wanted to know the probability that the slope was greater than
## 1, the closest we could get is
fert.lm %>% multcomp::glht(linfct = c("FERTILIZER >= 0.9")) %>% summary


## ----predictModel4a, results='markdown', eval=TRUE, echo=TRUE, hidden=TRUE----
## testing a specific hypothesis
## Probabiliy of getting our estimate if slope was 1
fert.lm %>% brms::hypothesis("FERTILIZER = 1")
## Cant ask probability that the slope is equal to something in frequentist
## If we wanted to know the probability that the slope was greater than
## 1, the closest we could get is
fert.lm %>% brms::hypothesis("FERTILIZER > 0.9")


## ----predictModel5a, results='markdown', eval=TRUE, echo=TRUE, hidden=TRUE----
newdata <- list(FERTILIZER = c(200, 100))
fert.lm %>% emmeans(pairwise~FERTILIZER, at = newdata) 
## or with confidence intervals
fert.lm %>% emmeans(pairwise~FERTILIZER, at = newdata) %>% confint


## ----figureModel, results='markdown', eval=TRUE, hidden=TRUE------------------
## Using emmeans
fert_grid <- with(fert, list(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len = 100)))
## OR
fert_grid <- fert %>% data_grid(FERTILIZER = seq_range(FERTILIZER, n = 100))
newdata <- fert.lm %>%
  emmeans(~FERTILIZER,  at = fert_grid) %>%
  as.data.frame
newdata <- fert.lm %>% emmeans(~FERTILIZER, at = fert_grid) %>% as.data.frame
newdata %>% head
ggplot(newdata, aes(y = emmean, x = FERTILIZER))+
    geom_point(data = fert, aes(y = YIELD)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "blue", alpha = 0.3) + 
    geom_line() +
    scale_y_continuous(expression(Grass~yield~(g.m^-3)))+
    scale_x_continuous(expression(Fertilizer~concentration~(g.ml^-1)))+
    theme_classic()


## ----figureModelA, results='markdown', eval=TRUE, hidden=TRUE-----------------
## Using emmeans
fert_grid <- fert %>% data_grid(FERTILIZER = seq_range(FERTILIZER, n = 100))
newdata <- fert.lm %>%
  emmeans(~FERTILIZER,  at = fert_grid) %>%
  as.data.frame
newdata <- fert.lm %>% emmeans(~FERTILIZER, at = fert_grid) %>% as.data.frame
newdata %>% head

## Now generate partial residuals
fitted_grid <- fert
fit <- fert.lm %>% emmeans(~FERTILIZER, at = fitted_grid) %>% as.data.frame() %>%
    pull(emmean)
resid.newdata = fert %>%
    mutate(Fit = fit,
           Resid = resid(fert.lm),
           Partial.obs = Fit + Resid)
resid.newdata %>%  head
ggplot(newdata, aes(y = emmean, x = FERTILIZER))+
    geom_point(data = resid.newdata, aes(y = Partial.obs)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), fill = "blue", alpha = 0.3) + 
    geom_line() +
    scale_y_continuous(expression(Grass~yield~(g.m^-3)))+
    scale_x_continuous(expression(Fertilizer~concentration~(g.ml^-1)))+
    theme_classic()


## ----figureModel1, results='markdown', eval=TRUE, hidden=TRUE-----------------
## Using emmeans
newdata <- with(fert, data.frame(FERTILIZER = seq(min(FERTILIZER), max(FERTILIZER), len=100)))
Xmat <- model.matrix(~FERTILIZER,  data = newdata)
coefs <- coef(fert.lm)
preds <- coefs %*% t(Xmat)
se <- sqrt(diag(Xmat %*% vcov(fert.lm) %*% t(Xmat)))
fert.df <- df.residual(fert.lm)
newdata <- newdata %>%
  mutate(fit = as.vector(preds),
         lower = fit - qt(0.975, df = fert.df)*se,
         upper = fit + qt(0.975,  df = fert.df)*se)
ggplot(newdata, aes(y = fit, x = FERTILIZER))+
    geom_point(data = fert, aes(y = YIELD)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha =0.3) + 
    geom_line() +
    scale_y_continuous(expression(Grass~yield~(g.m^-3)))+
    scale_x_continuous(expression(Fertilizer~concentration~(g.ml^-1)))+
    theme_classic()

