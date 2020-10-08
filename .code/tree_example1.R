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
library(patchwork)


## ----readData, results='markdown', eval=TRUE----------------------------------
abalone = read_csv('../data/abalone.csv', trim_ws=TRUE)
glimpse(abalone)


## ----preparation, results='markdown', eval=TRUE, hidden=TRUE------------------
abalone = abalone %>% mutate(SEX=factor(SEX))


## ----EDA, results='markdown', eval=TRUE, hidden=TRUE--------------------------
ggplot(abalone) +
    geom_point(aes(y=AGE, x=RINGS))
## Very tight relationship

scatterplotMatrix(~RINGS + SEX +LENGTH+DIAMETER+HEIGHT+WHOLE_WEIGHT+MEAT_WEIGHT+GUT_WEIGHT+
                  SHELL_WEIGHT, data=abalone)



## ----fitModel, results='markdown', eval=FALSE, hidden=TRUE--------------------
## set.seed(123)
## nrow(abalone)
## i = sample(1:nrow(abalone), 100,replace=FALSE)
## abalone.train = abalone[-i,]
## abalone.test = abalone[i,]
## 
## abalone.gbm = gbm(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
##                       WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT +
##                       SHELL_WEIGHT,
##                   data=abalone.train,
##                   distribution='poisson',
##                   var.monotone=c(0,1,1,1,1,1,1,1),
##                   n.trees=10000,
##                   interaction.depth=5,
##                   bag.fraction=0.5,
##                   shrinkage=0.01,
##                   train.fraction=1,
##                   cv.folds=3)
## (best.iter = gbm.perf(abalone.gbm,method='OOB'))
## (best.iter = gbm.perf(abalone.gbm,method='cv'))
## summary(abalone.gbm, n.trees=best.iter)
## 
## plot(abalone.gbm, 8, n.tree=best.iter)
## 
## par(mfrow=c(2,4))
## for (i in 1:8) plot(abalone.gbm, i, n.tree=best.iter)
## 
## library(pdp)
## library(ggfortify)
## ## recursive indicates that a weighted tree traversal method described by Friedman 2001 (which is very fast)
## ## should be used (only works for gbm).  Otherwise a slower brute force method is used.
## ## if want to back transform - need to use brute force.
## abalone.gbm %>% pdp::partial(pred.var='SHELL_WEIGHT',
##                         n.trees=best.iter) %>%
##     autoplot()
## abalone.gbm %>% pdp::partial(pred.var='SHELL_WEIGHT',
##                         n.trees=best.iter,
##                         recursive=FALSE,
##                         inv.link=exp) %>%
##     autoplot()
## 
## 
## abalone.gbm %>% pdp::partial(pred.var=c('SHELL_WEIGHT','MEAT_WEIGHT'),
##                         n.trees=best.iter) %>%
##     autoplot()
## 
## g1 = abalone.gbm %>% pdp::partial(pred.var='SHELL_WEIGHT', n.trees=best.iter,
##                             recursive=FALSE,inv.link=exp) %>%
##     autoplot
## g2 = abalone.gbm %>% pdp::partial(pred.var='HEIGHT', n.trees=best.iter,
##                             recursive=FALSE,inv.link=exp) %>%
##     autoplot
## 
## 
## g1 + g2
## 
## abalone.test %>%
##   bind_cols(Pred = predict(abalone.gbm,newdata=abalone.test, n.tree=best.iter, type='response')) %>%
##   ggplot() +
##   geom_point(aes(y=Pred,  x=RINGS))
## 
## 
## 
## ##Computes Friedman's H-statistic to assess the strength of variable interactions.
## ## This measures the relative strength of interactions in models
## ## It is on a scale of 0-1, where 1 is very strong interaction
## ## In y=β_0+β_1x_1+β_2x_2+β_3x_3..
## ## H=\frac{β_3}{√{β_1^2+β_2^2+β_3^2}}
## ## If both main effects are weak, then the H- stat will be unstable.. and could indicate
## ## a strong interaction.
## ## What were the strong main effects...
## ##SHELL_WEIGHT
## ##HEIGHT
## ##SEX
## attr(abalone.gbm$Terms,"term.labels")
## 
## interact.gbm(abalone.gbm, abalone,c(1,4), n.tree=best.iter)
## interact.gbm(abalone.gbm, abalone,c(1,8), n.tree=best.iter)
## interact.gbm(abalone.gbm, abalone,c(4,8), n.tree=best.iter)
## interact.gbm(abalone.gbm, abalone,c(1,4,8), n.tree=best.iter)
## 
## abalone.gbm %>% pdp::partial(pred.var=c(1),  n.trees=best.iter, recursive=FALSE) %>% autoplot
## abalone.gbm %>% pdp::partial(pred.var=c(1, 4),  n.trees=best.iter, recursive=FALSE) %>% autoplot
## abalone.gbm %>% pdp::partial(pred.var=c(1, 8),  n.trees=best.iter, recursive=FALSE) %>% autoplot
## 
## #plot(abalone.gbm, c(1,4), n.tree=best.iter)
## #plot(abalone.gbm, c(5,6), n.tree=best.iter)
## 
## 
## abalone.grid = plot(abalone.gbm, c(1,4), n.tree=best.iter, return.grid=TRUE)
## head(abalone.grid)
## 
## ggplot(abalone.grid, aes(y=HEIGHT, x=SEX)) +
##     geom_tile(aes(fill=y)) +
##     geom_contour(aes(z=y)) +
##     scale_fill_gradientn(colors=heat.colors(10))
## 


## -----------------------------------------------------------------------------
interact.gbm(abalone.gbm, abalone,1:5, n.tree=best.iter)

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
terms
plot(abalone.gbm, c(1,4), n.tree=best.iter)
plot(abalone.gbm, c(5,6), n.tree=best.iter)

abalone.grid = plot(abalone.gbm, c(5,6), n.tree=best.iter, return.grid=TRUE)
head(abalone.grid)

ggplot(abalone.grid, aes(y=MEAT_WEIGHT, x=WHOLE_WEIGHT)) +
    geom_tile(aes(fill=y)) +
    geom_contour(aes(z=y)) +
   scale_fill_gradientn(colors=heat.colors(10))



## ----gbmstep, eval=FALSE, hidden=TRUE-----------------------------------------
## ### gbm.step - this takes a long time - do over a break
## library(dismo)
## abalone.gbm1 <- gbm.step(data=abalone %>% as.data.frame, gbm.x=1:8, gbm.y=9,
##                         tree.complexity=5,
##                         learning.rate=0.01,
##                         bag.fraction=0.5,
##                         n.trees=10000,
##                         family='poisson')
## summary(abalone.gbm1)
## gbm.plot(abalone.gbm1, n.plots=8, write.title = FALSE)
## gbm.plot.fits(abalone.gbm1)
## find.int <- gbm.interactions(abalone.gbm1)
## summary(find.int)
## find.int$rank.list
## gbm.perspec(abalone.gbm1,6,5)
## 
## library(caret)
## ## The following is not working
## tune.grid = expand.grid(interaction.depth=c(3,5,7),
##                         n.trees = c(500,1000),
##                         shrinkage=c(0.1,0.01),
##                         n.minobsinnode=20)
## fitControl <- trainControl(## 10-fold CV
##                            method = "repeatedcv",
##                            number = 10,
##                            ## repeated ten times
##                            repeats = 10)
## abalone.training = train(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
##                       WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT +
##                       SHELL_WEIGHT,
##                       data=abalone.train,
##                       method='gbm',
##                       tuneGrid=tune.grid,
##                       trControl = fitControl,
##                       distribution='poisson',
##                       var.monotone=c(0,1,1,1,1,1,1,1),
##                       train.fraction=1,
##                       )
## summary(abalone.training)


## ----randomForest, results='markdown', eval=FALSE, hidden=TRUE----------------
## library(randomForest)
## abalone.rf = randomForest(RINGS ~ SEX + LENGTH + DIAMETER + HEIGHT +
##                       WHOLE_WEIGHT + MEAT_WEIGHT + GUT_WEIGHT + SHELL_WEIGHT,
##                       data=abalone, importance=TRUE,
##                       ntree=1000)
## abalone.imp = importance(abalone.rf)
## ## Rank by either:
## ## *MSE (mean decrease in accuracy)
## ## For each tree, calculate OOB prediction error.
## ## This also done after permuting predictors.
## ## Then average diff of prediction errors for each tree
## ## *NodePurity (mean decrease in node impurity)
## ## Measure of the total decline of impurity due to each
## ## predictor averaged over trees
## 100*abalone.imp/sum(abalone.imp)
## varImpPlot(abalone.rf)
## ## use brute force
## abalone.rf %>% pdp::partial('SHELL_WEIGHT') %>% autoplot


## ----Tree1, cache=TRUE, eval=FALSE, hidden=TRUE-------------------------------
## abalone.gbm = ...
## save(abalone.gbm, file='data/abalone.gbm.RData')


## ---- eval=FALSE, hidden=TRUE-------------------------------------------------
## load(file='data/abalone.gbm.RData')

