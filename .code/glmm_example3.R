## ----setup, include=FALSE, warnings=FALSE, message=FALSE----------------------
knitr::opts_chunk$set(echo = TRUE)
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
library(emmeans)   #for estimating marginal means
library(MASS)      #for glm.nb
library(MuMIn)     #for AICc
library(tidyverse) #for data wrangling
library(broom.mixed)
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


## ----eda, results='markdown', eval=TRUE, hidden=TRUE--------------------------
starling = starling %>% mutate(MONTH=factor(MONTH,levels=c('Nov','Jan')),
                               SITUATION=factor(SITUATION),
                               BIRD=factor(BIRD)
                               )
## Exploratory data analysis
ggplot(starling, aes(y=MASS, x=MONTH)) +
    geom_boxplot() +
    facet_grid(~SITUATION)
## Better still
ggplot(starling, aes(y=MASS, x=MONTH, group=BIRD)) +
    geom_point() +
    geom_line() +
    facet_grid(~SITUATION) 

## It is clear that the Nov mass of each bird is different - so random intercepts
## The degree to which they change between Nov and Dec is also relatively different
##  per bird - perhaps random intercept/random slope



## ----fitModel1, results='markdown', eval=FALSE, hidden=TRUE-------------------
## ##LMER
## starling.lmer = lmer(MASS ~ MONTH*SITUATION +(1|BIRD), data=starling,
##                      REML=FALSE)
## starling.lmer1 = lmer(MASS ~ MONTH+SITUATION +(1|BIRD), data=starling,
##                      REML=FALSE)
## AICc(starling.lmer, starling.lmer1)
## 
## starling.lmer2 <- update(starling.lmer1,  REML=TRUE)
## starling.lmer3 <- update(starling.lmer2,  ~.-(1|BIRD)+(MONTH|BIRD))
## 
## ##glmmTMB
## starling.glmmTMB = glmmTMB(MASS ~ MONTH*SITUATION +(1|BIRD), data=starling,
##                      REML=FALSE)
## starling.glmmTMB1 <- update(starling.glmmTMB,  ~ . - MONTH*SITUATION + MONTH+SITUATION +(1|BIRD))
## AICc(starling.glmmTMB, starling.glmmTMB1)
## 
## starling.glmmTMB2 <- update(starling.glmmTMB1, REML=TRUE)
## starling.glmmTMB3 <- update(starling.glmmTMB2,  ~ . - (1|BIRD) +(MONTH|BIRD))
## AICc(starling.glmmTMB2,  starling.glmmTMB3)


## ----validate, results='markdown', eval=FALSE, hidden=TRUE--------------------
## ##LMER
## plot_grid(plot_model(starling.lmer2,  type='diag')[-2])
## performance::check_model(starling.lmer2)
## starling.resid = simulateResiduals(starling.lmer2,  plot=TRUE)
## 
## ##glmmTMB
## plot_grid(plot_model(starling.glmmTMB2,  type='diag')[-2])
## performance::check_model(starling.glmmTMB2)
## starling.resid = simulateResiduals(starling.glmmTMB2,  plot=TRUE)


## ----summarise, results='markdown', eval=FALSE, hidden=TRUE-------------------
## ##LMER
## plot(allEffects(starling.lmer2),  ci.style='bar')
## #plot_model(starling.lmer2,  type='eff') %>% plot_grid
## plot_model(starling.lmer2,  type='eff',  terms=c('SITUATION', 'MONTH'))
## 
## summary(starling.lmer2)
## confint(starling.lmer2)
## tidy(starling.lmer2, effects='fixed', conf.int=TRUE)
## r.squaredGLMM(starling.lmer2)
## performance::r2_nakagawa(starling.lmer2)
## 
## ##glmmTMB
## plot(allEffects(starling.glmmTMB2),  ci.style='bar')
## #plot_model(starling.glmmTMB2,  type='eff') %>% plot_grid
## plot_model(starling.glmmTMB2,  type='eff',  terms=c('SITUATION', 'MONTH'))
## 
## summary(starling.glmmTMB2)
## confint(starling.glmmTMB2)
## tidy(starling.glmmTMB2, effects='fixed', conf.int=TRUE)
## r.squaredGLMM(starling.glmmTMB2)
## performance::r2_nakagawa(starling.glmmTMB2)


## ----predictions,  results='markup',  eval=FALSE,  hidden=TRUE----------------
## ##LMER
## emmeans(starling.lmer2,  pairwise~SITUATION)
## ##glmmTMB
## 
## emmeans(starling.glmmTMB2,  pairwise~SITUATION)


## ----summary, results='markdown', eval=FALSE, hidden=TRUE---------------------
## ##LMER
## newdata = emmeans(starling.lmer2, ~SITUATION*MONTH) %>%
##     as.data.frame
## ggplot(newdata, aes(y=emmean, x=SITUATION, fill=MONTH)) +
##     geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), shape=21,
##                     position=position_dodge(0.2)) +
##   theme_classic()
## 
## ##glmmTMB
## newdata = emmeans(starling.glmmTMB2, ~SITUATION*MONTH) %>%
##     as.data.frame
## ggplot(newdata, aes(y=emmean, x=SITUATION, fill=MONTH)) +
##     geom_pointrange(aes(ymin=lower.CL, ymax=upper.CL), shape=21,
##                     position=position_dodge(0.2)) +
##   theme_classic()
## 

