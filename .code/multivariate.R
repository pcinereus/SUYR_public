## ---- PCA
library(tidyverse)
library(vegan)
library(GGally)
library(corrplot)
library(car)
library(mvabund)
library(scales)

data <- read_csv('../public/data/data.csv', trim_ws=TRUE)
head(data) 
enviro <- read_csv('../public/data/enviro.csv', trim_ws=TRUE)
head(enviro)

enviro = enviro %>% mutate(Substrate=factor(Substrate))
## EDA
data[,-1] %>%
    cor %>%
    corrplot(type='upper', diag=FALSE)
data[,-1] %>%
    cor %>%
    corrplot(type='upper', order='FPC', diag=FALSE)

ggpairs(data[,-1]^0.25, lower = list(continuous = "smooth"),
        diag = list(continuous = "density"),
        axisLabels = "show")

#there is evidence of non-normality and non-linearity
#although we could attempt to normalize via sqrt transform, this is
#unlikely to fix all.vars'
#linearity also not good.
#It is likely that these issues are the result of the sampling
#occuring over a larger scale than the natural range of the taxa.
#This will result in some species being left skewed and others being
#right skewed. It will also result in non-linearity and the horseshoe
#effect.

data.stng <- data[,-1] %>%
    mutate(across(where(is.numeric), function(x) x^0.25)) %>%
    wisconsin()
data.stnd <- wisconsin(data[,-1]^0.25)
data.stnd

## Run PCA - unconstrained axis rotations
## normally we would not scale, but for illustrative purposes...
data.rda <- rda(data.stnd, scale=TRUE)
summary(data.rda, display=NULL)

screeplot(data.rda)
abline(a=1,b=0)

scores(data.rda, choices=1:3, display='sites')
scores(data.rda, choices=1:3, display='species')

biplot(data.rda, scaling='species')
biplot(data.rda, scaling='sites')

data.sites.scores <- data.rda %>%
    scores(display='sites') %>%
    as.data.frame() %>%
    bind_cols(data)

data.species.scores <- data.rda %>%
    scores(display='species') %>%
    as.data.frame() %>%
    mutate(Species=rownames(.))

g <- ggplot() +
    geom_segment(data=NULL, aes(y=-Inf,x=0,yend=Inf,xend=0),
                 linetype='dotted')+
    geom_segment(data=NULL, aes(y=0,x=-Inf,yend=0,xend=Inf),
                 linetype='dotted')+
    geom_point(data=data.sites.scores, aes(y=PC2,x=PC1))+
    geom_text(data=data.sites.scores, aes(y=PC2,x=PC1, label=Sites,hjust=-0.2),
              show.legend=FALSE) +
    geom_segment(data=data.species.scores, aes(y=0,x=0,yend=PC2,xend=PC1),
                 arrow=arrow(length=unit(0.3,'lines')), color='red')+
    theme_bw()
g




## OR
#data.species.scores <- data.rda %>%
#    fortify(display='species')
#data.site.scores <- data.rda %>%
#    fortify(display='sites')
#g<-ggplot() +
#    geom_segment(data=NULL, aes(y=-Inf,x=0,yend=Inf,xend=0),
#                 linetype='dotted')+
#    geom_segment(data=NULL, aes(y=0,x=-Inf,yend=0,xend=Inf),
#                linetype='dotted')+
#   geom_point(data=data.species.scores, aes(y=PC2,x=PC1)) +
#   geom_text(data=data.site.scores, aes(y=PC2,x=PC1, label=Label,hjust=-0.2),
#             show.legend=FALSE)+
#    geom_segment(data=data.species.scores, aes(y=0,x=0,yend=PC2,xend=PC1),
#                 arrow=arrow(length=unit(0.3,'lines')), color='red')+
#    theme_bw()

#put a circle
circle.prob <- 0.68
r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(data.rda$CA$u[,1:2]^2))^(1/4)
theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
circle <- data.frame(PC1 = r * cos(theta), PC2 = r * sin(theta))
g <- g + geom_path(data = circle, aes(y=PC2,x=PC1),color = muted('white'), size = 1/2, alpha = 1/3)

g

##Put on some nice species labels
## First filter out all those outside the circle
data.species.scores.sub <- data.species.scores %>%
  mutate(Length=sqrt(PC1^2 + PC2^2)) %>%
  filter(Length>r)
hjust <- ifelse(data.species.scores.sub$PC1>0,0,1)
vjust <- ifelse(data.species.scores.sub$PC2>0,0,1)
g <- g + geom_text(data=data.species.scores.sub, aes(y=PC2,x=PC1, label=Species),
                   hjust=hjust, vjust=vjust, color='red')
g

#Put on some nice axes titles
eig <- eigenvals(data.rda)
paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig)))
g <- g + scale_y_continuous(paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig))))+
    scale_x_continuous(paste(names(eig[1]), sprintf('(%0.1f%% explained var.)', 100 * eig[1]/sum(eig))))
g




library(devtools)
devtools::install_github("gavinsimpson/ggvegan")
library(ggvegan)
autoplot(data.rda) + theme_bw()
autoplot(data.rda, arrows=TRUE, display='species') + theme_bw()
autoplot(data.rda,geom='text') + theme_bw()

## Envfit
library(car)
vif(lm(1:nrow(enviro)~pH+Slope+Pressure+Altitude+Substrate, data=enviro))
#take out either Altitude or Pressure
vif(lm(1:nrow(enviro)~pH+Slope+Altitude+Substrate, data=enviro))

Xmat <- model.matrix(~-1+pH+Slope+Altitude+Substrate, enviro)
data.env <- envfit(data.rda, env=Xmat)
data.env

data.env.scores <- data.env %>%
    scores(display='vector') %>%
    as.data.frame %>%
    mutate(Effect=rownames(.))

hjust <- ifelse(data.env.scores$PC1>0,0,1)
vjust <- ifelse(data.env.scores$PC2>0,0,1)
g<- g + geom_segment(data=data.env.scores, aes(y=0,x=0,yend=PC2,xend=PC1),
                     arrow=arrow(length=unit(0.3,'lines')), color='blue')+
    geom_text(data=data.env.scores, aes(y=PC2,x=PC1, label=Effect),
              hjust=hjust, vjust=vjust, color='blue')
g


##extract PC's to use as responses
lm(data.sites.scores$PC1 ~ pH+Slope+Altitude+Substrate, data=enviro) %>%
    summary()
lm(data.sites.scores$PC1 ~ scale(pH)+scale(Slope)+scale(Altitude)+Substrate, data=enviro) %>%
    summary()

## ----end


## ---- RDA
data.rda <- rda(data.stnd ~ scale(pH)+scale(Slope)+scale(Altitude)+Substrate,
                data=enviro, scale=FALSE)
summary(data.rda, display=NULL)
## Mention conditioning

#data.rda <- rda(data.stnd ~ scale(pH)+scale(Slope)+Condition(scale(Altitude))+Substrate,
#                data=enviro, scale=FALSE)
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

plot(data.rda)
autoplot(data.rda)

data.sites.scores <- as.data.frame(scores(data.rda, display='sites'))
head(data.sites.scores)
data.sites.scores <- data.frame(data.sites.scores, data)
data.species.scores <- as.data.frame(scores(data.rda, display = 'species'))
head(data.species.scores)
data.species.scores$Species <- colnames(data[,-1])
head(data.species.scores)

library(grid)
g<-ggplot() +
    geom_segment(data=NULL, aes(y=-Inf,x=0,yend=Inf,xend=0), linetype='dotted')+
    geom_segment(data=NULL, aes(y=0,x=-Inf,yend=0,xend=Inf), linetype='dotted')+
    geom_point(data=data.sites.scores, aes(y=RDA2,x=RDA1))+
    geom_text(data=data.sites.scores, aes(y=RDA2,x=RDA1, label=Sites,hjust=-0.2), show.legend=FALSE)+
    geom_segment(data=data.species.scores, aes(y=0,x=0,yend=RDA2,xend=RDA1), arrow=arrow(length=unit(0.3,'lines')), color='red')+
    theme_classic()

g
#Put on some nice species labels
hjust <- ifelse(data.species.scores$RDA1>0,0,1)
vjust <- ifelse(data.species.scores$RDA2>0,0,1)
g <- g + geom_text(data=data.species.scores, aes(y=RDA2,x=RDA1, label=Species), hjust=hjust, vjust=vjust, color='red')
g
#Put on some nice axes titles
eig <- eigenvals(data.rda)
paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig)))
g <- g + scale_y_continuous(paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig))))+
    scale_x_continuous(paste(names(eig[1]), sprintf('(%0.1f%% explained var.)', 100 * eig[1]/sum(eig))))
g
#put the environmental vectors on
env.scores <- as.data.frame(scores(data.rda, display='bp'))
env.scores$Effect <- row.names(env.scores)

hjust <- ifelse(env.scores$RDA1>0,0,1)
vjust <- ifelse(env.scores$RDA2>0,0,1)
g <- g + geom_segment(data=env.scores, aes(y=0,x=0,yend=RDA2,xend=RDA1), arrow=arrow(length=unit(0.3,'lines')), color='blue')+
    geom_text(data=env.scores, aes(y=RDA2,x=RDA1, label=Effect), hjust=hjust, vjust=vjust, color='blue')
g
#Put on some nice axes titles
eig <- eigenvals(data.rda)
paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig)))
g <- g + scale_y_continuous(paste(names(eig[2]), sprintf('(%0.1f%% explained var.)', 100 * eig[2]/sum(eig))))+
    scale_x_continuous(paste(names(eig[1]), sprintf('(%0.1f%% explained var.)', 100 * eig[1]/sum(eig))))

g

## ----end

## ---- CA
#convert to frequencies (of row and columns)
# MARGIN=2 indicates columns
data.stnd <- data[,-1] %>%
    decostand(method="total",MARGIN=2)
data.stnd

data.stnd %>% cor %>%
    corrplot(diag=FALSE)

data.stnd %>% cor %>%
    corrplot(diag=FALSE, order='FPC')

data.ca <- cca(data.stnd, scale=FALSE)
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

                                        #extract CA's to use as responses
data.sites.scores <- as.data.frame(scores(data.ca, display='sites'))
head(data.sites.scores)
data.sites.scores <- data.frame(data.sites.scores, data)
data.species.scores <- as.data.frame(scores(data.ca, display = 'species'))
head(data.species.scores)
data.species.scores$Species <- colnames(data[,-1])
head(data.species.scores)

summary(lm(data.sites.scores$CA1 ~ pH+Slope+Altitude+Substrate, data=enviro))
summary(lm(data.sites.scores$CA2 ~ pH+Slope+Altitude+Substrate, data=enviro))

## ----end

## ---- CCA
data.cca <- cca(data.stnd~pH + Altitude + Substrate + Slope, data=enviro, scale=FALSE)

summary(data.cca, display=NULL)
anova(data.cca)

plot(data.cca)
plot(data.cca, scaling='species')
plot(data.cca, scaling='sites')
autoplot(data.ca, geom=c('point','text'), layers=c('species','sites','biplot'))

data.cs <- data.ca$CA$u[,1]
data.rs <- data.ca$CA$v[,1]
data3 <- data.stnd[order(data.cs),order(data.rs)]
data3 <- data.frame(Sites=rownames(data3),data3)
data4 = data3 %>%
    gather(key=Species, value=value, -Sites) %>%
    mutate(Species=factor(Species, levels=unique(Species)),
           Sites=factor(Sites, levels=unique(Sites))) 

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

## ---- PCoA
## principal coordinates analysis
data.stnd <- decostand(data[,-1],method="total",MARGIN=2)
data.stnd

data.dist = vegdist(data.stnd, method='bray')
data.capscale = capscale(data.dist~1, data=enviro)

summary(data.capscale, display=NULL)
plot(data.capscale)

#Distance based redundancy analysis
data.capscale = capscale(data.dist~scale(pH) + scale(Altitude) + Substrate + scale(Slope), data=enviro)
summary(data.capscale, display=NULL)
plot(data.capscale)

summary(data.capscale, display=NULL)
anova(data.capscale)

anova(data.capscale, by='margin')
screeplot(data.capscale)
sum(eigenvals(data.capscale))/length(eigenvals(data.capscale))
eigenvals(data.capscale)/sum(eigenvals(data.capscale))

## Conditioning on
data.capscale = capscale(data.dist~pH + Condition(Altitude) + Substrate + Slope, data=enviro)
summary(data.capscale, display=NULL)
plot(data.capscale)

## ----end

## ---- MVABUND
combined.data = cbind(data, enviro)
names(combined.data)

ggpairs(combined.data[,c(2,12:16)], lower = list(continuous = "smooth"),
        diag = list(continuous = "density"),
        axisLabels = "show")
ggpairs(combined.data[,c(5,13:17)], lower = list(continuous = "smooth"),
        diag = list(continuous = "density"),
        axisLabels = "show")
mva = mvabund(data[,-1])

meanvar.plot(mva)
plot(mva)
X = enviro$Substrate
#plot(dd, y=X)
enviro = enviro %>% mutate(ph=cut(pH, breaks=c(0,2,4,6,8,10)))
#plot(dd~interaction(PH,Substrate), data=enviro)

#scatterplotMatrix(~pH+Altitude, data=enviro)
data.mod <- manyglm(mva~scale(pH) + scale(Altitude) + Substrate + scale(Slope),
                    family=poisson(link='log'), data=enviro)

#data.mod <- manyglm(mva~exp(pH) + Altitude + Substrate + Slope,
#                    family=poisson(link='log'), data=enviro)
plot(data.mod)

data.mod <- manyglm(mva~scale(pH) + scale(Altitude) + Substrate + scale(Slope),
                    family='negative.binomial', data=enviro)
plot(data.mod)
data.mod
anova(data.mod)
anova(data.mod, p.uni='adjusted')
summary(data.mod)
summary(data.mod, resamp="residual")
summary(data.mod, resamp="perm.resid")
#summary(data.mod, resamp="monte.carlo", test="wald", nBoot=300) 
#plot(data.mod)
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

#apply(macnally.std[,c(-1,-2)],2,max)
#apply(macnally.std[,c(-1,-2)],2,var, na.rm=TRUE)

#vegdist(macnally[,-1], method='bray')

macnally.dist <- vegdist(macnally.std,"bray")

macnally.mds <- metaMDS(macnally.dist, k=2, plot=TRUE)
#macnally.mds <- metaMDS(macnally[,-1], k=2)

wascores(macnally.mds$points, macnally[, -1])


macnally.mds$stress

stressplot(macnally.mds)

plot(macnally.mds)
species <- wascores(macnally.mds$points,  macnally[, -1]) %>%
  as.data.frame %>%
  mutate(Species = row.names(.))

#g=autoplot(macnally.mds)
#g

macnally.sites.scores <- macnally.mds %>%
    scores(display='sites') %>%
    as.data.frame() %>%
    bind_cols(macnally)
## If used raw data in metaMDS
macnally.species.scores <- macnally.mds %>%
    scores(display='species') %>%
    as.data.frame() %>%
    mutate(Species = rownames(.))
## If used distance matrix in MDS
macnally.species.scores <- wascores(macnally.mds$points,  macnally[, -1]) %>%
    as.data.frame() %>%
  mutate(Species = rownames(.),
         NMDS1=MDS1,
         NMDS2=MDS2)
head(macnally.species.scores)

g<-ggplot() +
    geom_point(data= macnally.sites.scores, aes(y=NMDS2,x=NMDS1, color=HABITAT))+
                                        #geom_text(data=macnally.sites.scores, aes(y=NMDS2,x=NMDS1, label=SITE,hjust=-0.2,color=HABITAT), show_guide=FALSE)+
     geom_point(data=macnally.species.scores, aes(y=NMDS2,x=NMDS1), color='grey') +
    geom_text(data=macnally.species.scores, aes(y=NMDS2,x=NMDS1, label=Species,
                                                hjust=-0.2),show.legend=FALSE, color='grey') +
    geom_segment(data=NULL, aes(y=-Inf,x=0,yend=Inf,xend=0), linetype='dotted')+
    geom_segment(data=NULL, aes(y=0,x=-Inf,yend=0,xend=Inf), linetype='dotted')
#    geom_segment(data=macnally.species.scores, aes(y=0,x=0,yend=NMDS2,xend=NMDS1), arrow=arrow(length=unit(0.3,'lines')))
g


macnally.hull = macnally.sites.scores %>%
  group_by(HABITAT) %>%
  slice(chull(NMDS1, NMDS2))

g <- g + geom_polygon(data=macnally.hull, aes(y=NMDS2,x=NMDS1, fill=HABITAT), alpha=0.2)+
    theme_classic()
g

#ordiplot(macnally.mds, display="sites", type="n")
#text(macnally.mds,lab=rownames(macnally), col=as.numeric(macnally$HABITAT))

habitat <- model.matrix(~-1+macnally$HABITAT)
colnames(habitat) <-gsub("macnally\\$HABITAT","",colnames(habitat))
envfit <- envfit(macnally.mds, env=habitat)
envfit



ordiplot(macnally.mds, display="sites", type="n")
text(macnally.mds,lab=rownames(macnally), col=as.numeric(macnally$HABITAT))
text(macnally.mds,lab=macnally$HABITAT, col=as.numeric(macnally$HABITAT))

Xmat <- model.matrix(~-1+HABITAT, data=macnally)
colnames(Xmat) <-gsub("HABITAT","",colnames(Xmat))
envfit <- envfit(macnally.mds, env=Xmat)
envfit
plot(envfit, col="gray")

plot(envfit, col="gray")


vegdist(data.stnd)


bioenv(macnally.dist,
                      decostand(habitat,"standardize"))

adonis(macnally.dist ~ HABITAT, data=macnally)

simper(macnally.std, macnally$HABITAT)
## ----end

## ---- dune
dune <- read_csv('../public/data/dune.csv', trim_ws=TRUE)
#dune <- read.csv('../downloads/data/dune.csv')
dune

#species means
apply(dune[,-1],2, mean, na.rm=TRUE)
#species maximums
apply(dune[,-1],2, max)
#species sums
apply(dune[,-1],2, sum, na.rm=TRUE)
#species variance
apply(dune[,-1],2, var, na.rm=TRUE)




library(vegan)
dune.dist <- vegdist(wisconsin(dune[,-1]^0.25), "bray")

dune.mds = metaMDS(dune.dist, k=2)
plot(dune.mds, type="text", display="sites" )

dune.mds$species <- wascores(dune.mds$points, dune[,-1], expand = TRUE)
pl <- ordiplot(dune.mds, type = "none")
points(pl, "sites", pch=21, col="red", bg="yellow")
text(pl, "species", col="blue", cex=0.9)
autoplot(dune.mds, geom=c('text'))

ordiplot(dune.mds)
ordihull(dune.mds, dune$MANAGEMENT, col=as.numeric(dune$MANAGEMENT))

dune.adonis<-adonis(dune.dist~as.data.frame(dune[,1]))
dune.adonis

#plot(isomap(dune.dist, k=3), new=TRUE)


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

## Multiple Response Permutation Procedure (MRPP) provides a test of
##      whether there is a significant difference between two or more
##      groups of sampling units. Function ‘meandist’ finds the mean
##      within and between block dissimilarities.
dune.mrpp = mrpp(dune.dist, dune[,1], permutations=999)
dune.mrpp
hist(dune.mrpp$boot.deltas)
# Chance corrected within-group agreement = 1-Obs delta / exp delta
dune.meandist = meandist(dune.dist, dune[,1], permutations=999)
dune.meandist
summary(dune.meandist)
plot(dune.meandist)

## ----end

## ---- bioenv data
data.dist <- vegdist(wisconsin(data[,-1]^0.25),"bray")
Xmat = model.matrix(~ -1 + pH + Slope + Altitude + Substrate, data=enviro[,-1])
data.bioenv <- bioenv(data.dist,
                      decostand(Xmat,"standardize"))
data.bioenv
## ----end

## ---- adonis data
data.adonis <- adonis(data.dist ~ pH + Slope + Altitude + 
                          Substrate, data=enviro)
data.adonis <- adonis(data.dist ~ Altitude, data=enviro)
data.adonis

## ----end

## ---- varveg
vareveg <- read.csv('../public/data/vareveg.csv')
head(vareveg)
vareenv <- read.csv('../public/data/vareenv.csv')
head(vareenv)


vareveg.dist <- vegdist(wisconsin(vareveg[,-1]^0.25),'bray')
#vareveg.dist <- vegdist(vareveg.std, "bray")
#environmental variables 
vareenv.std <- decostand(vareenv[,-1], "standardize")
vareenv.dist <- vegdist(vareenv.std, "euc")

#bioenv(vareveg.std, vareenv.std)

bioenv(vareveg.dist, vareenv.std)
#adonis(vareveg.std~Ca+Fe+Mn+Baresoil, data=vareenv)
adonis(vareveg.dist~Ca+Fe+Mn+Baresoil, data=vareenv.std)

#meandist(vareveg.dist)
#vareveg.simper = simper(vareveg[,-1], vareenv.std$Ca, permutations=100)
#vareveg.simper
#summary(vareveg.simper)

## ----end


library(codyn)
X <- data %>%
    gather(key=Species, value=value,-Sites)
community_diversity(X, abundance.var = "value",
                    replicate.var = 'Sites')
multivariate_change(X,
                    species.var='Species',
                    abundance.var = "value",
                    time.var = 'Sites')
