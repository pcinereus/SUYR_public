#############################################################
##             RDA (Correlations, Euclidean distance)                
##            /  \                              
##Unconstrained   Constrained ---> ordination                    
##   (PCA)            (RDA)   ---> anova
##
##              CA (Chisq distance)
##             /  \
##Unconstrained   Constrained ---> ordination
## (CA)             (CCA)     ---> anova
##
##             PCoA (any distance)
##             /  \
##Unconstrained   Constrained ---> ordination
##                            ---> anova
##
##Unconstrained  ---> ordination
##               ---> envfit (overlay enviromental data) (permutation test)
##               ---> lm/glm etc (response or predictor)
#############################################################
##     Dissimilarity
##            --> MDS      ---> ordination
##            --> bioenv   ---> variable importance       (perm test)
##            --> adonis   ---> anova                     (perm test)
##            --> simper   ---> similarity percentages
##            --- betadisp ---> homogeneity of dispersion (perm test)
#############################################################
##     Model based
##            ---> manyglm ---> anova
#############################################################

## ---- PCA
library(tidyverse)
library(vegan)
library(GGally)
library(corrplot)
library(car)
library(mvabund)
library(scales)
library(ggvegan)
library(ggrepel)

data <- read_csv('../public/data/data.csv', trim_ws=TRUE)
head(data) 
enviro <- read_csv('../public/data/enviro.csv', trim_ws=TRUE)
head(enviro)

enviro <- enviro %>% mutate(Substrate=factor(Substrate))
## EDA
data[,-1] %>%
    cor %>%
    corrplot(type='upper', diag=FALSE)
data[,-1] %>%
    cor %>%
    corrplot(type='upper', order='FPC', diag=FALSE)

ggpairs(data[,-1], lower = list(continuous = "smooth"),
        diag = list(continuous = "density"),
        axisLabels = "show")

ggpairs(data[,-1]^0.25, lower = list(continuous = "smooth"),
        diag = list(continuous = "density"),
        axisLabels = "show")

data.std <- data %>%
    dplyr::select(-Sites) %>%
    mutate(across(everything(), function(x) x^0.25)) %>%
    wisconsin()
data.stnd

## Run PCA - unconstrained axis rotations
## normally we would not scale, but for illustrative purposes...
data.rda <- rda(data.std, scale=TRUE)
## data.rda <- rda(data.std, scale=FALSE)
summary(data.rda, display=NULL)

screeplot(data.rda)
abline(a=1,b=0)

## Quick and nasty ordination plots
biplot(data.rda, scaling='species')
biplot(data.rda, scaling='sites')

## Quick and nasty ordination plots
pl<-ordiplot(data.rda)
points(pl, "sites", pch=21, col="red", bg="yellow")
text(pl, "sites", col="red", cex=0.9)
text(pl, "species", col="blue", cex=0.9)
## line(pl, "sites")


## ggvegan provides ways to use ggplot
## library(devtools)
## devtools::install_github("gavinsimpson/ggvegan")
## library(ggvegan)
autoplot(data.rda)
autoplot(data.rda) + theme_bw()
autoplot(data.rda,geom='text') + theme_bw()

data.rda.scores <- data.rda %>%
    fortify()
data.rda.scores


#######################################################################
g <-
    ggplot(data = NULL, aes(y=PC2, x=PC1)) +
    geom_hline(yintercept=0, linetype='dotted') +
    geom_vline(xintercept=0, linetype='dotted') +
    geom_point(data=data.rda.scores %>% filter(Score=='sites')) +
    geom_text(data=data.rda.scores %>% filter(Score=='sites'),
              aes(label=Label), hjust=-0.2) +
    geom_segment(data=data.rda.scores %>% filter(Score=='species'),
                 aes(y=0, x=0, yend=PC2, xend=PC1),
                 arrow=arrow(length=unit(0.3,'lines')), color='red') +
    ## geom_text(data=data.rda.scores %>% filter(Score=='species'),
    ##           aes(y=PC2*1.1, x=PC1*1.1, label=Label), color='red') +
    geom_text_repel(data=data.rda.scores %>% filter(Score=='species'),
              aes(y=PC2*1.1, x=PC1*1.1, label=Label), color='red') +
    theme_bw()
g

# Nice axes titles
eig <- eigenvals(data.rda)

paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig)))
g <- g +
    scale_y_continuous(paste(names(eig[2]), sprintf('(%0.1f%% explained var.)',
                                                    100 * eig[2]/sum(eig))))+
    scale_x_continuous(paste(names(eig[1]), sprintf('(%0.1f%% explained var.)',
                                                    100 * eig[1]/sum(eig))))

#put a circle
circle.prob <- 0.68
circle.prob <- 0.95
r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(data.rda$CA$u[,1:2]^2))^(1/4)
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- data.frame(PC1 = r * cos(theta), PC2 = r * sin(theta))
g <- g + geom_path(data = circle, aes(y=PC2,x=PC1), color = muted('white'), size = 1/2, alpha = 1/3)
g
######################################################################

## Envfit
library(car)
vif(lm(1:nrow(enviro)~pH+Slope+Pressure+Altitude+Substrate, data=enviro))
lm(1:nrow(enviro)~pH+Slope+Pressure+Altitude+Substrate, data=enviro) %>% vif
#take out either Altitude or Pressure
vif(lm(1:nrow(enviro)~pH+Slope+Altitude+Substrate, data=enviro))

Xmat <- model.matrix(~-1+pH+Slope+Altitude+Substrate, enviro)
data.env <- envfit(data.rda, env=Xmat)
data.env

########
data.env.scores <- data.env %>% fortify()
g <- g + 
    geom_segment(data=data.env.scores,
                 aes(y=0, x=0, yend=PC2, xend=PC1),
                 arrow=arrow(length=unit(0.3,'lines')), color='blue') +
    geom_text(data=data.env.scores,
              aes(y=PC2*1.1, x=PC1*1.1, label=Label), color='blue')
g
##############

##extract PC's to use as responses
PC1 <- data.rda.scores %>% filter(Score=='sites') %>% pull(PC1)
PC2 <- data.rda.scores %>% filter(Score=='sites') %>% pull(PC2)

lm(PC1 ~ pH+Slope+Altitude+Substrate, data=enviro) %>%
    summary()
lm(PC2 ~ pH+Slope+Altitude+Substrate, data=enviro) %>%
    summary()
## ----end

## ---- RDA
data.rda <- rda(data.std ~ scale(pH)+scale(Slope)+scale(Altitude)+Substrate,
                data=enviro, scale=FALSE)
summary(data.rda, display=NULL)

vif.cca(data.rda)
#overall test
anova(data.rda)
anova(data.rda, by='axis')
anova(data.rda, by='margin')
#anova(data.rda, by='margin', scope="Altitude")

## see the regression coefficients
coef(data.rda)

RsquareAdj(data.rda)

screeplot(data.rda)

autoplot(data.rda, geom='text')


#######################################################################
data.rda.scores <- data.rda %>% fortify()
data.rda.scores
ggplot(data = NULL, aes(y=PC2, x=PC1)) +
    geom_hline(yintercept=0, linetype='dotted') +
    geom_vline(xintercept=0, linetype='dotted') +
    geom_point(data=data.rda.scores %>% filter(Score=='sites')) +
    geom_text(data=data.rda.scores %>% filter(Score=='sites'),
              aes(label=Label), hjust=-0.2) +
    geom_segment(data=data.rda.scores %>% filter(Score=='species'),
                 aes(y=0, x=0, yend=PC2, xend=PC1),
                 arrow=arrow(length=unit(0.3,'lines')), color='red') +
    geom_text(data=data.rda.scores %>% filter(Score=='species'),
              aes(y=PC2*1.1, x=PC1*1.1, label=Label), color='red') ->
    g
g

# Nice axes titles
eig <- eigenvals(data.rda)
paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig)))
g <- g + scale_y_continuous(paste(names(eig[2]), sprintf('(%0.1f%% explained var.)',
                                                         100 * eig[2]/sum(eig))))+
    scale_x_continuous(paste(names(eig[1]), sprintf('(%0.1f%% explained var.)',
                                                    100 * eig[1]/sum(eig))))

#put a circle
## circle.prob <- 0.68
circle.prob <- 0.95
r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(data.rda$CA$u[,1:2]^2))^(1/4)
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- data.frame(PC1 = r * cos(theta), PC2 = r * sin(theta))
g <- g + geom_path(data = circle, aes(y=PC2,x=PC1),color = muted('white'), size = 1/2, alpha = 1/3)
g


g <- g + 
    geom_segment(data=data.rda.scores %>% filter(Score=='biplot'),
                 aes(y=0, x=0, yend=RDA2, xend=RDA1),
                 arrow=arrow(length=unit(0.3,'lines')), color='blue') +
    geom_text(data=data.rda.scores %>% filter(Score=='biplot'),
              aes(y=RDA2*1.1, x=RDA1*1.1, label=Label), color='blue')
g


## ----end

## ---- CA
#convert to frequencies (of row and columns)
# MARGIN=2 indicates columns
data.std <- data %>%
    dplyr::select(-Sites) %>% 
    decostand(method="total",MARGIN=2)
data.std

data.std %>% cor %>%
    corrplot(diag=FALSE)

data.std %>% cor %>%
    corrplot(diag=FALSE, order='FPC')

data.ca <- cca(data.std, scale=FALSE)
summary(data.ca, display=NULL)
#anova(data.ca)

screeplot(data.ca)
sum(eigenvals(data.ca))/length(eigenvals(data.ca))
eigenvals(data.ca)/sum(eigenvals(data.ca))
plot(data.ca, scaling='species')
Xmat <- model.matrix(~-1+pH+Slope+Altitude+Substrate, enviro)
data.env <- envfit(data.ca, env=Xmat)
data.env
plot(data.env, add=TRUE)

##                                         #extract CA's to use as responses
## data.sites.scores <- as.data.frame(scores(data.ca, display='sites'))
## head(data.sites.scores)
## data.sites.scores <- data.frame(data.sites.scores, data)
## data.species.scores <- as.data.frame(scores(data.ca, display = 'species'))
## head(data.species.scores)
## data.species.scores$Species <- colnames(data[,-1])
## head(data.species.scores)

autoplot(data.ca, geom='text')

data.ca.scores <- data.ca %>% fortify()
CA1 <- data.ca.scores %>% filter(Score =='sites') %>% pull(CA1)
CA2 <- data.ca.scores %>% filter(Score =='sites') %>% pull(CA2)
summary(lm(CA1 ~ pH+Slope+Altitude+Substrate, data=enviro))
summary(lm(CA2 ~ pH+Slope+Altitude+Substrate, data=enviro))

## ----end

## ---- CCA
data.cca <- cca(data.std~pH + Altitude + Substrate + Slope, data=enviro, scale=FALSE)

summary(data.cca, display=NULL)
anova(data.cca)

plot(data.cca)
plot(data.cca, scaling='species')
plot(data.cca, scaling='sites')
autoplot(data.cca)

vif.cca(data.cca)
#overall test
anova(data.cca)
anova(data.cca, by='axis')
anova(data.cca, by='margin')
#anova(data.cca, by='margin', scope="pH")

coef(data.cca)

RsquareAdj(data.cca)

screeplot(data.cca)
int <- data.cca$tot.chi/length(data.cca$CA$eig)
abline(h=int)

## ----end


## ---- MDS macnally
## MUST READ IN THIS WAY..
macnally <- read.csv('../public/data/macnally_full.csv',strip.white=TRUE)
head(macnally)
macnally[1:5,1:5]


apply(macnally[,c(-1)],2,mean, na.rm=TRUE)
apply(macnally[,c(-1)],2,max)
apply(macnally[,c(-1)],2,sum)
apply(macnally[,c(-1)],2,var, na.rm=TRUE)


macnally.mds <- metaMDS(macnally[,-1], k=2,  plot=TRUE)
macnally.mds

library(vegan)
macnally.std <- wisconsin(macnally[,c(-1)]^0.25)
macnally.dist <- vegdist(macnally.std,"bray")

macnally.mds <- metaMDS(macnally.std, k=2, plot=TRUE)
macnally.mds <- metaMDS(macnally.dist, k=2, plot=TRUE)
macnally.mds <- metaMDS(macnally[,-1], k=2)


macnally.mds$stress

stressplot(macnally.mds)

macnally.mds.scores <- macnally.mds %>%
        fortify() %>%
    full_join(macnally %>% add_rownames(var='Label'))

g <-
    ggplot(data = NULL, aes(y=NMDS2, x=NMDS1)) +
    geom_hline(yintercept=0, linetype='dotted') +
    geom_vline(xintercept=0, linetype='dotted') +
    geom_point(data=macnally.mds.scores %>% filter(Score=='sites'),
               aes(color=HABITAT)) +
    geom_text(data=macnally.mds.scores %>% filter(Score=='sites'),
              aes(label=Label, color=HABITAT), hjust=-0.2) 
g


g + ggforce::geom_mark_ellipse(data=macnally.mds.scores %>% filter(Score=='sites'),
                      aes(y=NMDS2, x=NMDS1, fill=HABITAT), expand=0) 
g + ggforce::geom_mark_hull(data=macnally.mds.scores %>% filter(Score=='sites'),
                      aes(y=NMDS2, x=NMDS1, fill=HABITAT), expand=0) 
g + ggforce::geom_mark_hull(data=macnally.mds.scores %>% filter(Score=='sites'),
                      aes(y=NMDS2, x=NMDS1, fill=HABITAT), expand=0, concavity = 10) 


Xmat <- model.matrix(~-1+HABITAT, data=macnally)
colnames(Xmat) <-gsub("HABITAT","",colnames(Xmat))
envfit <- envfit(macnally.mds, env=Xmat)
envfit


data.env.scores <- envfit %>% fortify()
g <- g + 
    geom_segment(data=data.env.scores,
                 aes(y=0, x=0, yend=NMDS2, xend=NMDS1),
                 arrow=arrow(length=unit(0.3,'lines')), color='blue') +
    geom_text(data=data.env.scores,
              aes(y=NMDS2*1.1, x=NMDS1*1.1, label=Label), color='blue')
g


plot(envfit, col="gray")


bioenv(macnally.dist,
                      decostand(habitat,"standardize"))

adonis(macnally.dist ~ HABITAT, data=macnally)

simper(macnally.std, macnally$HABITAT)

macnally.disp <- betadisper(macnally.dist, macnally$HABITAT)
boxplot(macnally.disp)
plot(macnally.disp)
anova(macnally.disp)
permutest(macnally.disp, pairwise = TRUE)
TukeyHSD(macnally.disp)

macnally.disp <- betadisper(macnally.dist, macnally$HABITAT, type="median",bias.adjust = TRUE)
boxplot(macnally.disp)
plot(macnally.disp)
anova(macnally.disp)
permutest(macnally.disp, pairwise = TRUE)
TukeyHSD(macnally.disp)
## ----end


## ---- dune
dune <- read_csv('../public/data/dune.csv', trim_ws=TRUE)
dune <- dune %>% mutate(MANAGEMENT=factor(MANAGEMENT,  levels=c("NM","BF","HF","SF"))) %>%
  as.data.frame()
dune

#species means
apply(dune[,-1],2, mean, na.rm=TRUE)
#species maximums
apply(dune[,-1],2, max)
#species sums
apply(dune[,-1],2, sum, na.rm=TRUE)
#species variance
apply(dune[,-1],2, var, na.rm=TRUE)

dune.dist <- vegdist(wisconsin(dune[,-1]^0.25), "bray")

dune.mds = metaMDS(dune.dist, k=2)
plot(dune.mds, type="text", display="sites" )


dune.adonis<-adonis(dune.dist~MANAGEMENT,  data=dune)
dune.adonis

management <-factor(dune$MANAGEMENT, levels=c("NM","BF","HF","SF"))
mm <- model.matrix(~management)
head(mm)
colnames(mm) <-gsub("management","",colnames(mm))
mm <- data.frame(mm)
dune.adonis<-adonis(dune.dist~BF+HF+SF, data=mm,
                    perm=9999)
dune.adonis

dune.simper=simper(dune[,-1], dune[,1], permutations = 999)
summary(dune.simper)

#PERMDISP2 - multivariate homogeneity of group dispersions (variances)
dune.disp <- betadisper(dune.dist,  group=dune$MANAGEMENT)
boxplot(dune.disp)
plot(dune.disp)
anova(dune.disp)
TukeyHSD(dune.disp)
## ----end

## ---- MVABUND

combined.data <- cbind(data, enviro)
names(combined.data)
mva = mvabund(data[,-1])

meanvar.plot(mva)
plot(mva)
X = enviro$Substrate
## enviro = enviro %>% mutate(ph=cut(pH, breaks=c(0,2,4,6,8,10)))

data.mod <- manyglm(mva~scale(pH) + scale(Altitude) + Substrate + scale(Slope),
                    family=poisson(link='log'), data=enviro)

plot(data.mod)

data.mod <- manyglm(mva~scale(pH) + scale(Altitude) + Substrate + scale(Slope),
                    family='negative.binomial', data=enviro)
plot(data.mod)
data.mod
anova(data.mod, test='LR')
anova(data.mod, cor.type = 'R')
anova(data.mod, cor.type = 'shrink')
## We can also explore the individal univariate tests.
anova(data.mod, p.uni='adjusted')
summary(data.mod, test="LR")

inverts.mva <- mvabund(inverts)
inverts.mglmP <- manyglm(inverts.mva ~ TREATMENT * WEEK, data = brink, family = 'poisson')
plot(inverts.mglmP) 
inverts.mglmNB <- manyglm(inverts.mva ~ TREATMENT * WEEK, data = brink, family = 'negative.binomial')
plot(inverts.mglmNB) 
control <- how(within = Within(type = 'none'),
               Plots(strata = brink$DITCH, type = 'free'),
               nperm = 50)
permutations <- shuffleSet(nrow(inverts.mva), control = control)
inverts.mglmNB2 <- manyglm(inverts.mva ~ TREATMENT + WEEK, 
                                data = brink, family = 'negative.binomial')
inverts_aov <- anova(inverts.mglmNB, inverts.mglmNB2, 
                     bootID = permutations,  
                     p.uni = 'adjusted', test = 'LR') 
inverts_aov 

## Compare to model without any treatment - so test for effect of treatment
inverts.mglmNB3 <- manyglm(inverts.mva ~ WEEK, data = brink, 
                       family = 'negative.binomial')
inverts_aov2 <- anova(inverts.mglmNB, inverts.mglmNB3 , bootID = permutations,  
      p.uni = 'adjusted', test = 'LR') 
inverts_aov2 



mod_pt <- NULL
for (i in levels(brink$WEEK)) {
    brink.sub <- brink %>% filter(WEEK == i)
    inverts.sub <- brink.sub %>% dplyr::select(-TREATMENT, -WEEK, -DITCH) %>%
        mvabund()
    ## model
    ##mod_pt[[i]]$mod <- manyglm(inverts.sub ~ TREATMENT, data = brink.sub)
    mod <- manyglm(inverts.sub ~ TREATMENT, data = brink.sub)
    aov <- anova(mod, nBoot = 100, 
                 p.uni = 'adjusted', test = 'LR', show.time = "none")
    sum <- summary(mod, nBoot = 100, 
                   p.uni = 'adjusted', test = 'LR')
    
    P <- c(community = aov$table[2,4],
           aov$uni.p[2,])
    mod_pt[[i]] <- list(mod = mod, aov=aov, P=P)
}
dd <- do.call('rbind', lapply(mod_pt, function(x) x$P)) %>%
    as.data.frame() %>% 
    rownames_to_column(var = 'WEEK')
dd

## purrr alternative
library(purrr)
d = bind_cols(inverts = inverts.mva, brink %>% dplyr::select(TREATMENT, WEEK, DITCH))
dd <- d %>% group_by(WEEK) %>%
    nest() %>%
    mutate(mod = purrr::map(data, function(x) {
        manyglm(inverts ~ TREATMENT, data=x)
    })) %>% 
    mutate(aov = purrr::map(mod, function(x) {
        anova(x, nBoot=100, p.uni = 'adjusted', test = 'LR', show.time = 'none')
    })) %>%
    mutate(sum = purrr::map(mod, function(x) {
        summary(x, nBoot=100, p.uni = 'adjusted', test = 'LR')
    })) %>%
    mutate(P = purrr::map(aov, function(x) {
        c(Community = x$table[2,4], x$uni.p[2,])
        }))
dd %>% dplyr::select(WEEK, P) %>% unnest_wider(P)

g <- 
    dd %>% mutate(Deviance = purrr::map(aov, function(x) {
        x$uni.test[2,]
    })) %>%
    dplyr::select(WEEK, Deviance) %>% 
    unnest_wider(Deviance) %>%
    pivot_longer(cols=-WEEK) %>%
    ungroup %>%
    mutate(name = forcats::fct_reorder(name, value, 'sum', .desc = TRUE)) %>%
    ggplot(aes(y=value, x=as.numeric(as.character(WEEK)), fill=name)) +
    geom_area() +
    geom_vline(aes(xintercept = 0)) 
g




## ----end
