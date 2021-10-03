## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning=FALSE,cache.lazy = FALSE, tidy='styler')
options(tinytex.engine = 'xelatex')

## ----customEngine, results='markdown', eval=TRUE, echo=FALSE, hidden=TRUE-----
copy_img = function(path,  options) {
  print(path)
  print(options$label)
    print(options$fig.path)
    print(options$fig.ext)
    src_file = paste0(options$fig.path, options$label, '-1.', options$fig.ext)
    out_file = paste0('figures/', options$label, '.', options$fig.ext)
    print(src_file)
    print(out_file)
    print(file.exists(paste0(options$fig.path, options$label, '-1', options$fig.ext)))
    file.rename(paste0(options$fig.path, options$label, '-1', options$fig.ext),  paste0('figures/', options$label, options$fig.ext)) 
}

## knitr::knit_hooks$set(copy_tikz_img=function(before,  options,  envir) {
##   if (!before){
##     if(!dir.exists('figures')) dir.create('figures')
##     print(options$label)
##     print(options$fig.path)
##     print(options$fig.ext)
##     src_file = paste0(options$fig.path, options$label, '-1.', options$fig.ext)
##     out_file = paste0('figures/', options$label, '.', options$fig.ext)
##     print(src_file)
##     print(out_file)
##     print(file.exists(paste0(options$fig.path, options$label, '-1', options$fig.ext)))
##     file.rename(paste0(options$fig.path, options$label, '-1', options$fig.ext),  paste0('figures/', options$label, options$fig.ext)) 
##   }
## })

knitr::knit_engines$set(mytikz = function(options) {
  `%n%` = function(x, y) {
    ifelse(!is.null(x),  x,  y)
  }
  if (!options$eval) 
    return(engine_output(options, options$code, ""))
  lines = xfun::read_utf8(options$engine.opts$template %n% system.file("misc", 
                                                                 "tikz2pdf.tex", package = "knitr"))
  i = grep("%% TIKZ_CODE %%", lines)
  if (length(i) != 1L) 
    stop("Couldn't find replacement string; or the are multiple of them.")
  s = append(lines, options$code, i)
  xfun::write_utf8(s, texf <- knitr:::wd_tempfile("tikz", ".tex"))
  on.exit(unlink(texf), add = TRUE)
  ext = tolower(options$fig.ext %n% knitr:::dev2ext(options$dev))
  to_svg = ext == "svg"
  outf = if (to_svg) 
           tinytex::latexmk(texf, "latex")
         else tinytex::latexmk(texf)
  fig = knitr:::fig_path(if (to_svg) 
                   ".dvi"
                 else ".pdf", options)
  dir.create(dirname(fig), recursive = TRUE, showWarnings = FALSE)
  file.rename(outf, fig)
  fig2 = xfun:::with_ext(fig, ext)
  if (to_svg) {
    if (Sys.which("dvisvgm") == "") 
      tinytex::tlmgr_install("dvisvgm")
    if (system2("dvisvgm", c("-o", shQuote(fig2), fig)) != 
        0) 
      stop("Failed to compile ", fig, " to ", fig2)
  }
  else {
    if (ext != "pdf") 
      magick::image_write(do.call(magick::image_convert, 
                                  c(list(magick::image_read_pdf(fig), ext), options$engine.opts$convert.opts)), 
                          fig2)
  }
  fig = fig2
  options$fig.num = 1L
  options$fig.cur = 1L
  extra = (knit_hooks$get("plot"))(fig, options)
  options$engine = "tex"
  engine_output(options, options$code, "", extra)
})


## ----libraries, results='markdown', eval=TRUE, message=FALSE, warning=FALSE----
library(car)       #for regression diagnostics
library(broom)     #for tidy output
library(ggfortify) #for model diagnostics
library(sjPlot)    #for outputs
library(knitr)     #for kable
library(effects)   #for partial effects plots
library(ggeffects) #for partial effects plots
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(broom.mixed)
library(nlme)      #for lme
library(lme4)      #for lmer
library(lmerTest)  #for Satterthwaite's p-values
library(glmmTMB)   #for glmmTMB
library(DHARMa)   #for residuals and diagnostics
library(performance) #for diagnostic plots
library(see)         #for diagnostic plots


## \tikzstyle{Messy} = [decorate,decoration={random steps,segment length=3pt, amplitude=0.3pt},thick]

## \setmainfont[ExternalLocation=../resources/]{desyrel}

## \begin{tikzpicture}[every node/.style={draw,Messy,fill=blue!20},

## edge from parent/.style={Messy,anchor=south},

## edge from parent 3/.style={draw,-latex},

## parent anchor=south,

## child anchor=north,

## level 1/.style={sibling distance=4cm, level distance=1cm},

## level 2/.style={sibling distance=2cm, level distance=2cm},

## level 3/.style={sibling distance=0.5cm, level distance=2.0cm},

## level 4/.style={sibling distance=0.5cm, level distance=1cm},

## level 5/.style={sibling distance=0.5cm, level distance=1.5cm},

## every tree node/.style={align=north,anchor=north},

## mystyle/.style={-latex}

## ]

## \path

##  node [draw=none, fill=none](top) {}

##   child { node (A) {A}

##     child { node (Aa) {a}

##       child { node (Aa1) {} edge from parent[draw,-latex]}

##       child { node (Aa2) {} edge from parent[draw,-latex]}

##       child { node (Aa3) {} edge from parent[draw,-latex]}

##       child { node (Aa4) {} edge from parent[draw,-latex]}

##     }

##     child { node (Ab) {b}

##  	  child { node (Ab1) {} edge from parent[draw,-latex]}	

##       child { node (Ab2) {} edge from parent[draw,-latex]}

##       child { node (Ab3) {} edge from parent[draw,-latex]}

##       child { node (Ab4) {} edge from parent[draw,-latex]}

##     }

##   }

##   child { node (B) {B}

##     child { node (Bc) {c}

##       child { node (Bc1) {} edge from parent[draw,-latex]}	

##       child { node (Bc2) {} edge from parent[draw,-latex]}

##       child { node (BC3) {} edge from parent[draw,-latex]}

##       child { node (Bc4) {} edge from parent[draw,-latex]}

##     }	

##     child { node (Bd) {d}

##       child { node (Bd1) {} edge from parent[draw,-latex]}	

##       child { node (Bd2) {} edge from parent[draw,-latex]}

##       child { node (Bd3) {} edge from parent[draw,-latex]}

##       child { node (Bd4) {} edge from parent[draw,-latex]}

##     }

##   };

## 
## \draw [Messy,-latex](A.south)--(Aa.north);

## \draw [Messy,-latex](A.south)--(Ab.north);

## \draw [Messy,-latex](B.south)--(Aa.north);

## \draw [Messy,-latex](B.south)--(Ab.north);

## \draw [Messy,-latex](A.south)--(Bc.north);

## \draw [Messy,-latex](A.south)--(Bd.north);

## \draw [Messy,-latex](B.south)--(Bc.north);

## \draw [Messy,-latex](B.south)--(Bd.north);

## 
## \node[Messy,anchor=east,draw=none,fill=none] at ($(A.west) + (-2cm,0)$) (Factor1) {Factor 1};

## \node[Messy,anchor=east,draw=none,fill=none] at ($(Factor1.east|-Aa.west) + (0cm,0)$) (Factor2) {Factor 2};

## \node[Messy,anchor=east,draw=none,fill=none] at ($(Factor2.east|-Aa1.west) + (0cm,0)$) (Observations) {Observations};

## \end{tikzpicture}


## ----readData, results='markdown', eval=TRUE----------------------------------
starling = read_csv('../data/starling_full.csv', trim_ws=TRUE)
glimpse(starling)


## ----dataPrep, results='markdown', eval=TRUE, hidden=TRUE---------------------
starling = starling %>% mutate(MONTH=factor(MONTH,levels=c('Nov','Jan')),
                               SITUATION=factor(SITUATION),
                               BIRD=factor(BIRD)
                               )


## ----eda1, results='markdown', eval=TRUE, hidden=TRUE-------------------------
ggplot(starling, aes(y=MASS, x=MONTH)) +
    geom_boxplot() +
    facet_grid(~SITUATION)
## Better still
ggplot(starling, aes(y=MASS, x=MONTH, group=BIRD)) +
    geom_point() +
    geom_line() +
    facet_grid(~SITUATION) 


## ----fitModel1b, results='markdown', eval=TRUE, hidden=TRUE-------------------
starling.lme1 <- lme(MASS ~ 1, random=~1|BIRD, data=starling, method='REML')
starling.lme2 <- lme(MASS ~ 1, random=~MONTH|BIRD, data=starling, method='REML')
AICc(starling.lme1, starling.lme2)


## ----fitModel1a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Multiplicative and additive models
starling.lme1a <- update(starling.lme1, .~.+MONTH*SITUATION, method='ML')
starling.lme1b <- update(starling.lme1, .~.+MONTH+SITUATION, method='ML')
AICc(starling.lme1a, starling.lme1b)
## Finally, run as REML
starling.lme1a <- update(starling.lme1a, method='ML')


## ----fitModel2b, results='markdown', eval=TRUE, hidden=TRUE-------------------
starling.lmer1 <- lmer(MASS ~ 1 + (1|BIRD), data = starling, REML=TRUE)
starling.lmer2 <- lmer(MASS ~ 1 + (MONTH|BIRD), data = starling, REML=TRUE, control = lmerControl(check.nobs.vs.nRE = 'ignore'))
AICc(starling.lmer1, starling.lmer2)


## ----fitModel2a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Multiplicative and additive models
starling.lmer1a <- update(starling.lmer1, .~.+MONTH*SITUATION, REML=FALSE)
starling.lmer1b <- update(starling.lmer1, .~.+MONTH+SITUATION, REML=FALSE)
AICc(starling.lmer1a, starling.lmer1b)
## Finally, run as REML
starling.lmer1a <- update(starling.lmer1a, REML=TRUE)


## ----fitModel3b, results='markdown', eval=TRUE, hidden=TRUE-------------------
starling.glmmTMB1 = glmmTMB(MASS ~ 1 + (1|BIRD), data=starling, REML=TRUE)
starling.glmmTMB2 = glmmTMB(MASS ~ 1 + (MONTH|BIRD), data=starling, REML=TRUE)
starling.glmmTMB2 = glmmTMB(MASS ~ 1 + (MONTH|BIRD), data=starling, REML=TRUE,
                            control=glmmTMBControl(optimizer=optim,
                                                   optArgs = list(method='BFGS'))
                            )
AICc(starling.glmmTMB1, starling.glmmTMB2)


## ----fitModel3a, results='markdown', eval=TRUE, hidden=TRUE-------------------
##Multiplicative and additive models
starling.glmmTMB1a <- update(starling.glmmTMB1, .~.+MONTH*SITUATION, REML=FALSE)
starling.glmmTMB1b <- update(starling.glmmTMB1, .~.+MONTH+SITUATION, REML=FALSE)
AICc(starling.glmmTMB1a, starling.glmmTMB1b)
## Finally, run as REML
starling.glmmTMB1a <- update(starling.glmmTMB1a, REML=TRUE)


## ----modelValidation1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(starling.lme1a, type='diag')[-2] %>% plot_grid()


## ----modelValidation1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lme1a %>% performance::check_model()


## ----modelValidation1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
#starling.resid = simulateResiduals(starling.lme1a,  plot=TRUE)


## ----modelValidation2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(starling.lmer1a,  type='diag')[-2] %>% plot_grid()


## ----modelValidation2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lmer1a %>% performance::check_model()


## ----modelValidation2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.resid <- starling.lmer1a %>% simulateResiduals(plot=TRUE)


## ----modelValidation3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
plot_model(starling.glmmTMB1a,  type='diag')[-2] %>% plot_grid()


## ----modelValidation3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.glmmTMB1a %>% performance::check_model()


## ----modelValidation3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.resid <- starling.glmmTMB1a %>% simulateResiduals(plot=TRUE)


## ----partialPlot1a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lme1a %>% plot_model(type='eff',  terms=c('SITUATION', 'MONTH'))
starling.lme1a %>% plot_model(type='est')


## ----partialPlot1b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
starling.lme1a %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bars')


## ----partialPlot1c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lme1a %>% ggpredict(terms=c('SITUATION', 'MONTH')) %>% plot(add.data=TRUE)


## ----partialPlot1d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lme1a %>% ggemmeans(~SITUATION*MONTH) %>% plot(add.data=TRUE)


## ----partialPlot2a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lmer1a %>% plot_model(type='eff',  terms=c('SITUATION', 'MONTH'), show.data=TRUE)
starling.lmer1a %>% plot_model(type='est')


## ----partialPlot2b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
starling.lmer1a %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bars')


## ----partialPlot2c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lmer1a %>% ggpredict(terms=c('SITUATION', 'MONTH')) %>% plot(add.data=TRUE)


## ----partialPlot2d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.lmer1a %>% ggemmeans(~SITUATION*MONTH) %>% plot(add.data=TRUE)


## ----partialPlot3a, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.glmmTMB1a %>% plot_model(type='eff',  terms=c('SITUATION', 'MONTH'), show.data=TRUE)
starling.glmmTMB1a %>% plot_model(type='est')


## ----partialPlot3b, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=4----
starling.glmmTMB1a %>% allEffects() %>% plot(multiline=TRUE,  ci.style='bars')


## ----partialPlot3c, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.glmmTMB1a %>% ggpredict(terms=c('SITUATION', 'MONTH')) %>% plot(add.data=TRUE)


## ----partialPlot3d, results='markdown', eval=TRUE, hidden=TRUE, fig.width=7, fig.height=7----
starling.glmmTMB1a %>% ggemmeans(~SITUATION*MONTH) %>% plot(add.data=TRUE)


## ----summarise1a, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.lme1a %>% summary()


## ----summarise1b, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.lme1a %>% fixef()


## ----summarise1c, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.lme1a %>% tidy(conf.int=TRUE)
starling.lme1a %>% tidy(conf.int=TRUE) %>% kable


## ----summarise1d, results='markdown', eval=TRUE, hidden=TRUE------------------
# warning this is only appropriate for html output
starling.lme1a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summarise2a, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.lmer1a %>% summary()


## ----summarise2b, results='markdown', eval=TRUE, hidden=TRUE------------------
confint(starling.lmer1a)


## ----summarise2c, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.lmer1a %>% tidy(conf.int=TRUE)
starling.lmer1a %>% tidy(conf.int=TRUE) %>% kable


## ----summarise2d, results='markdown', eval=TRUE, hidden=TRUE------------------
# warning this is only appropriate for html output
starling.lmer1a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----summarise3a, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.glmmTMB1a %>% summary()


## ----summarise3b, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.glmmTMB1a %>% confint()


## ----summarise3c, results='markdown', eval=TRUE, hidden=TRUE------------------
starling.glmmTMB1a %>% tidy(conf.int=TRUE)
starling.glmmTMB1a %>% tidy(conf.int=TRUE) %>% kable


## ----summarise3d, results='markdown', eval=TRUE, hidden=TRUE------------------
# warning this is only appropriate for html output
starling.glmmTMB1a %>% sjPlot::tab_model(show.se=TRUE, show.aic=TRUE)


## ----postHoc1a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.lme1a %>% emmeans(~SITUATION) %>% pairs() %>% summary(infer=TRUE)
starling.emm <- starling.lme1a %>% emmeans(~SITUATION) %>% pairs() %>% summary(infer=TRUE) %>% data.frame


## ----postHoc1b, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.lme1a %>% r.squaredGLMM()
starling.lme1a %>% performance::r2_nakagawa()


## ----postHoc2a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.lmer1a %>% emmeans(~SITUATION) %>% pairs() %>% summary(infer=TRUE)
starling.emm <- starling.lmer1a %>% emmeans(~SITUATION) %>% pairs() %>% summary(infer=TRUE) %>% data.frame


## ----postHoc2b, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.lmer1a %>% r.squaredGLMM()
starling.lmer1a %>% performance::r2_nakagawa()


## ----postHoc3a, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.glmmTMB1a %>% emmeans(~SITUATION) %>% pairs() %>% summary(infer=TRUE)


## ----postHoc3b, results='markdown', eval=TRUE, echo=1,hidden=TRUE-------------
starling.glmmTMB1a %>% r.squaredGLMM()
starling.glmmTMB1a %>% performance::r2_nakagawa()


## ----summaryPlot1a, results='markdown', eval=TRUE, hidden=TRUE----------------
newdata <- starling.lme1a %>% emmeans(~SITUATION*MONTH) %>%
    as.data.frame
ggplot(newdata, aes(y=emmean, x=SITUATION, fill=MONTH)) +
    geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), shape=21,
                    position=position_dodge(0.2)) +
  theme_classic()


## ----summaryPlot2a, results='markdown', eval=TRUE, hidden=TRUE----------------
newdata <- starling.lmer1a %>% emmeans(~SITUATION*MONTH) %>%
    as.data.frame
ggplot(newdata, aes(y=emmean, x=SITUATION, fill=MONTH)) +
    geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), shape=21,
                    position=position_dodge(0.2)) +
  theme_classic()


## ----summaryPlot3a, results='markdown', eval=TRUE, hidden=TRUE----------------
newdata <- starling.glmmTMB1a %>% emmeans(~SITUATION*MONTH) %>%
    as.data.frame
ggplot(newdata, aes(y=emmean, x=SITUATION, fill=MONTH)) +
    geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), shape=21,
                    position=position_dodge(0.2)) +
  theme_classic()

