## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE)


## ----libraries, results='markdown', eval=TRUE---------------------------------
library(gbm)         #for gradient boosted models
library(car)
library(dismo)
library(pdp)
library(ggfortify)
library(randomForest)
library(tidyverse)
library(gridExtra)
library(patchwork)


## ----readData, results='markdown', eval=TRUE----------------------------------
abalone = read_csv('../data/abalone.csv', trim_ws=TRUE)
glimpse(abalone)


## ----preparation, results='markdown', eval=TRUE, hidden=TRUE------------------
abalone = abalone %>% mutate(SEX=factor(SEX))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE, fig.width=15, fig.height=15----
ggplot(abalone) +
    geom_point(aes(y=AGE, x=RINGS))
## Very tight relationship

scatterplotMatrix(~RINGS + SEX +LENGTH+DIAMETER+HEIGHT+WHOLE_WEIGHT+MEAT_WEIGHT+GUT_WEIGHT+
                  SHELL_WEIGHT, data=abalone)



## ----fitModel1, results='markdown', eval=TRUE, hidden=TRUE--------------------
set.seed(123)
nrow(abalone)
i = sample(1:nrow(abalone), 100,replace=FALSE)
abalone.train = abalone[-i,]
abalone.test = abalone[i,]


## ----fitModel2, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
abalone.gbm = gbm(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
                      WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT +
                      SHELL_WEIGHT,
                  data=abalone.train,
                  distribution='poisson',
                  var.monotone=c(0,1,1,1,1,1,1,1),
                  n.trees=10000,
                  interaction.depth=5,
                  bag.fraction=0.5,
                  shrinkage=0.01,
                  train.fraction=1,
                  cv.folds=3)


## ----fitModel3, results='markdown', eval=TRUE, hidden=TRUE--------------------
(best.iter = gbm.perf(abalone.gbm,method='OOB'))
(best.iter = gbm.perf(abalone.gbm,method='cv'))


## ----fitModel4, results='markdown', eval=TRUE, hidden=TRUE, cache=TRUE--------
abalone.gbm = gbm(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
                    WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT +
                    SHELL_WEIGHT,
                  data=abalone.train,
                  distribution='poisson',
                  var.monotone=c(0,1,1,1,1,1,1,1),
                  n.trees=5000,
                  interaction.depth=5,
                  bag.fraction=0.5,
                  shrinkage=0.005,
                  train.fraction=1,
                  cv.folds=3)


## ----fitModel5, results='markdown', eval=TRUE, hidden=TRUE--------------------
(best.iter = gbm.perf(abalone.gbm,method='OOB'))
(best.iter = gbm.perf(abalone.gbm,method='cv'))


## ----relativeInfluence1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=10----
summary(abalone.gbm, n.trees=best.iter)


## ----partialEffects1, results='markdown', eval=TRUE, hidden=TRUE, fig.width=6, fig.height=6----
attr(abalone.gbm$Terms,"term.labels")
plot(abalone.gbm, 8, n.tree=best.iter)
abalone.gbm %>% pdp::partial(pred.var='SHELL_WEIGHT',
                        n.trees=best.iter,
                        recursive=FALSE,
                        inv.link=exp) %>%
    autoplot()


## ----partialEffects2, results='markdown', eval=TRUE, hidden=TRUE, fig.width=10, fig.height=10----
nms <- attr(abalone.gbm$Terms,"term.labels")
p <- vector('list', length(nms))
names(p) <- nms
for (nm in nms) {
  print(nm)
  p[[nm]] <- abalone.gbm %>% pdp::partial(pred.var=nm,
                                 n.trees=best.iter,
                                 inv.link=exp,
                                 recursive=FALSE,
                                 type='regression') %>%
    autoplot() + ylim(0, 20)
}
 
do.call('grid.arrange', p)


## ----partialEffects3, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE----
abalone.gbm %>% pdp::partial(pred.var=c('SHELL_WEIGHT','MEAT_WEIGHT'),
                             n.trees=best.iter, recursive=TRUE) %>%
    autoplot()

g1 = abalone.gbm %>% pdp::partial(pred.var='SHELL_WEIGHT', n.trees=best.iter,
                            recursive=FALSE,inv.link=exp) %>%
    autoplot
g2 = abalone.gbm %>% pdp::partial(pred.var='HEIGHT', n.trees=best.iter,
                            recursive=FALSE,inv.link=exp) %>%
    autoplot


g1 + g2


## ----Accuracy1, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=7, fig.height=7----
abalone.acc <- abalone.test %>%
  bind_cols(Pred = predict(abalone.gbm,newdata=abalone.test, n.tree=best.iter, type='response'))

with(abalone.acc,  cor(RINGS, Pred))


abalone.acc %>%
  ggplot() +
  geom_point(aes(y=Pred,  x=RINGS))



## ----Interactions1, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=7, fig.height=7----
attr(abalone.gbm$Terms,"term.labels")
 
interact.gbm(abalone.gbm, abalone,c(1,4), n.tree=best.iter)
interact.gbm(abalone.gbm, abalone,c(4,8), n.tree=best.iter)
interact.gbm(abalone.gbm, abalone,c(1,4,8), n.tree=best.iter)

abalone.gbm %>% pdp::partial(pred.var=c(1),  n.trees=best.iter, recursive=FALSE) %>% autoplot
abalone.gbm %>% pdp::partial(pred.var=c(1, 4),  n.trees=best.iter, recursive=FALSE) %>% autoplot
abalone.gbm %>% pdp::partial(pred.var=c(1, 8),  n.trees=best.iter, recursive=FALSE) %>% autoplot

#plot(abalone.gbm, c(1,4), n.tree=best.iter)
#plot(abalone.gbm, c(5,6), n.tree=best.iter)


## ----Interactions2, results='markdown', eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=7, fig.height=7----

abalone.grid = plot(abalone.gbm, c(1,4), n.tree=best.iter, return.grid=TRUE)
head(abalone.grid)

ggplot(abalone.grid, aes(y=HEIGHT, x=SEX)) +
    geom_tile(aes(fill=y)) +
    geom_contour(aes(z=y)) +
    scale_fill_gradientn(colors=heat.colors(10))



## ----Interactions3, eval=TRUE, echo=FALSE-------------------------------------
terms <- attr(abalone.gbm$Terms,"term.labels")
abalone.int <- NULL
for (i in 1:(length(terms)-1)) {
    for (j in (i+1):length(terms)) {
        print(paste('i=',i, ' Name = ', terms[i]))
        print(paste('j=',j, ' Name = ', terms[j]))
        abalone.int <- rbind(abalone.int,
                             data.frame(Var1=terms[i], Var2=terms[j],
                                        "H.stat"=interact.gbm(abalone.gbm, abalone,c(i,j),
                                                              n.tree=best.iter)
                                        ))
    }
}
abalone.int %>% arrange(-H.stat)


## ----Interactions4, eval=TRUE, echo=FALSE-------------------------------------
plot(abalone.gbm, c(1,4), n.tree=best.iter)
plot(abalone.gbm, c(5,6), n.tree=best.iter)

abalone.grid = plot(abalone.gbm, c(5,6), n.tree=best.iter, return.grid=TRUE)
head(abalone.grid)

ggplot(abalone.grid, aes(y=MEAT_WEIGHT, x=WHOLE_WEIGHT)) +
    geom_tile(aes(fill=y)) +
    geom_contour(aes(z=y)) +
   scale_fill_gradientn(colors=heat.colors(10))



## ----gbmstep1, eval=TRUE, hidden=TRUE, cache=FALSE----------------------------
abalone.gbm1 <- gbm.step(data=abalone %>% as.data.frame, gbm.x=1:8, gbm.y=9,
                        tree.complexity=5,
                        learning.rate=0.001,
                        bag.fraction=0.5,
                        n.trees=10000,
                        family='poisson')


## ----gbmstep2, eval=TRUE, hidden=TRUE, cache=FALSE----------------------------
summary(abalone.gbm1)


## ----gbmstep3, eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=10,  fig.height=10----
gbm.plot(abalone.gbm1, n.plots=8, write.title = FALSE)


## ----gbmstep4, eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=10,  fig.height=10----
gbm.plot.fits(abalone.gbm1)


## ----gbmstep5, eval=TRUE, hidden=TRUE, cache=FALSE, fig.width=10,  fig.height=10----
find.int <- gbm.interactions(abalone.gbm1)
summary(find.int)
find.int$rank.list
gbm.perspec(abalone.gbm1,6,5)


## ----randomForest, results='markdown', eval=TRUE, hidden=TRUE-----------------
library(randomForest)
abalone.rf = randomForest(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
                      WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT + SHELL_WEIGHT,
                      data=abalone, importance=TRUE,
                      ntree=1000)
abalone.imp = importance(abalone.rf)
## Rank by either:
## *MSE (mean decrease in accuracy)
## For each tree, calculate OOB prediction error.
## This also done after permuting predictors.
## Then average diff of prediction errors for each tree
## *NodePurity (mean decrease in node impurity)
## Measure of the total decline of impurity due to each
## predictor averaged over trees
100*abalone.imp/sum(abalone.imp)
varImpPlot(abalone.rf)
## use brute force
abalone.rf %>% pdp::partial('SHELL_WEIGHT') %>% autoplot

