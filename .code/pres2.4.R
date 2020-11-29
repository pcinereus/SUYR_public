## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE)
options(tinytex.engine = 'xelatex')


## ----data-transformation, cache=TRUE,echo=FALSE-------------------------------
system("convert -resize 650x ../resources/data-transformation.pdf ../resources/data-transformation.png")
system("mv ../resources/data-transformation-0.png ../resources/data-transformation.png")


## ---- eval=FALSE,prompt=FALSE-------------------------------------------------
## data %>%
##     select(...) %>%
##         group_by(...) %>%
##             summarise(...)


## ---- echo=FALSE--------------------------------------------------------------
options(width=50)

## ---- tidy=TRUE, echo=TRUE----------------------------------------------------
load(file='../data/manipulationDatasets.RData')


## ----data.1, echo=FALSE, results='asis'---------------------------------------
library(pander)
library(tidyverse)
set.seed(1)
data.1 <- expand.grid(Cond=c("H","M","L"),Plot=paste("P",1:4,sep=""))
data.1$Cond <- factor(as.character(data.1$Cond))
data.1$Between <- gl(2,6,12,lab=paste("A",1:2,sep=""))
data.1$Time <- rep(1:4,3)
data.1 <- data.1 %>% group_by(Between) %>% mutate(LAT=rnorm(1, 18.5, 5)) %>% group_by(Between, Plot) %>% mutate(LAT=LAT+rnorm(3, 0, 0.5)) %>% ungroup
data.1 <- data.1 %>% mutate(Temp=22 + rnorm(12, 18.5-LAT, 0.2))
#data.1$Temp <- rnorm(12,22,10)
#data.1$LAT <- rnorm(12,18.5,2)
data.1$LONG <- rnorm(12,145,2)
data.1=dplyr::select(data.1,Between,Plot,Cond,Time,Temp,LAT,LONG)
## xTab<-xtable(data.1)
## align(xTab)<-rep("middle",6)
## print(xTab,type="html",include.rownames=F,only.contents=F,
## 			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
##      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:50%;'")
##       )

pandoc.table(data.1)
#save(data, data.1, data.bio,  data.chem,  data.geo, data.w,  nasa,  tikus, file='../data/manipulationDatasets.RData')
#save(data.1, file='../data/manipulationDatasets.RData')


## ---- results='markup'--------------------------------------------------------
library(dplyr)
library(tidyr)
#OR better still
library(tidyverse)


## ---- echo=FALSE--------------------------------------------------------------
options(width=90)

## ---- results='markup'--------------------------------------------------------
head(data.1)
#OR
data.1 %>% head()
#OR
data.1 %>% head


## ---- results='markup'--------------------------------------------------------
summary(data.1)


## ---- results='markup'--------------------------------------------------------
summary(data.1)
data.1 %>% summary()
data.1 %>% summary


## ---- results='markup'--------------------------------------------------------
str(data.1)


## ---- results='markup'--------------------------------------------------------
glimpse(data.1)


## ---- results='markup'--------------------------------------------------------
data.1 %>% as_tibble


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 1/.style={

##             nodes={text width=2em,font=\bfseries}

##         },

##         column 2/.style={

##             nodes={text width=2em}

##         },

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## &  &   & \\

## &  & |[fill=black!80]| & \\

## &  & |[fill=black!20]|  & \\

## &  & |[fill=black!60]|  & \\

## &  & |[fill=black!40]|  & \\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## &  &   & \\

## &  & |[fill=black!20]| & \\

## &  & |[fill=black!40]|  & \\

## &  & |[fill=black!60]|  & \\

## &  & |[fill=black!80]|  & \\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {arrange()} ++(second);

## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Sorted)  {Sorted data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(LAT)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(-LAT)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(Cond,Temp)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(Temp+LAT)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(Between,Cond)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- echo=FALSE--------------------------------------------------------------
options(width=55)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- echo=FALSE--------------------------------------------------------------
options(width=55)


## ---- results='markup'--------------------------------------------------------
data.1 %>% arrange(Cond,Temp/LAT)


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 1/.style={

##             nodes={text width=2em,font=\bfseries}

##         },

##         column 2/.style={

##             nodes={text width=2em}

##         },

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## &  &  & \\

## &  &  & \\

## &  &  & \\

## &  &  & \\

## &  &  & \\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## &  &  & & |[fill=blue!70]| \\

## &  &  & &\\

## &  &  &  &\\

## &  &  &  &\\

## &  &  &  & \\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {mutate()} ++(second);

## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Mutated)  {Transformed data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(LL=LAT+LONG)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(logTemp=log(Temp))


## ---- results='markdown', eval=TRUE, echo=FALSE-------------------------------
options(width=100)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup', results='hold'----------------------------------------
data.1 %>% mutate(MeanTemp=mean(Temp), cTemp=Temp-MeanTemp)
## OR if just want the centered variable..
#data.1 %>% mutate(cTemp=Temp-mean(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2) %>% as_tibble


## ---- results='markup', results='hold'----------------------------------------
data.1 %>% mutate(Time=factor(Time)) %>% as_tibble


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2) %>% as_tibble


## ---- results='markup', results='hold'----------------------------------------
data.1 %>% mutate(Cond=fct_recode(Cond, High='H',  Medium='M' )) %>%
  as_tibble


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2) %>% as_tibble


## ---- results='markup'--------------------------------------------------------
data.1 %>% pull(Cond)
data.1 %>% mutate(Cond=fct_relevel(Cond, c('L', 'M','H'))) %>%
                    as_tibble() %>% pull(Cond)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2) %>% as_tibble


## ---- results='markup'--------------------------------------------------------
data.1 %>% pull(Cond)
data.1 %>% mutate(Cond=recode_factor(Cond, 'L'='Low', 'M'='Medium')) %>%
                    as_tibble() %>% pull(Cond)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(leadTemp=lead(Temp), lagTemp=lag(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(rankTime=min_rank(Time),
                  denseRankTime=dense_rank(Time))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(rowTemp=row_number(Temp), rowTime=row_number(Time),
                  rankTime=min_rank(Time))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(ntile(Temp,4))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(between(Temp,20,25))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup', results='hold'----------------------------------------
data.1 %>% mutate(fTemp=ifelse(Temp<21, 'Low',
                     ifelse(between(Temp,21,25), 'Medium', 'High')))
## OR
data.1 %>% mutate(fTemp=case_when(Temp<21 ~ 'Low',
                               between(Temp, 21, 25) ~ 'Medium',
                               Temp>25 ~ 'High'))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(fTemp=cut(Temp, breaks=c(0,21,25,100),
                         labels=c('Low','Medium','High')))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(Region = cut(LAT, breaks=3, 
                               labels=c('Northern', 'Centeral', 'Southern'))) 


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% mutate(Region = cut(LAT, breaks=3, 
                               labels=c('Northern', 'Centeral', 'Southern'))) %>%
  mutate(Between = fct_inorder(Region))


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 1/.style={

##             nodes={text width=2em,font=\bfseries}

##         },

##         column 2/.style={

##             nodes={text width=2em}

##         },

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## &  &   & \\

## &  &   & \\

## &  &   & \\

## &  &   & \\

## &  &   & \\

## };

## 

## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!70]| & |[fill=blue!70]| &  |[fill=blue!70]| & |[fill=blue!70]| \\

## |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]|\\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {summarize()} ++(second);

## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Summarized)  {Summarized data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% summarise(MeanTemp=mean(Temp), VarTemp=var(Temp), N=n())


## ---- results='markup'--------------------------------------------------------
SE <- function(x) sd(x)/sqrt(length(x))
data.1 %>% summarise(MeanTemp=mean(Temp), VarTemp=var(Temp), 
          SEM=SE(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% summarise(across(c(Temp,LAT), list(Mean=mean, Var=var)))

## ---- results='markup'--------------------------------------------------------
data.1 %>% summarise(across(where(is.numeric), list(Mean=mean, Var=var)))

## ---- results='markup'--------------------------------------------------------
data.1 %>% summarize( across(where(is.numeric),  mean),
          across(where(is.factor),  length))


## ---- results='markup'--------------------------------------------------------
data.1 %>% count(Cond)


## ---- results='markup'--------------------------------------------------------
data.1 %>% count(Cond,between(Temp,20,30))


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 1/.style={

##             nodes={text width=2em,font=\bfseries}

##         },

##         column 2/.style={

##             nodes={text width=2em}

##         },

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## &  &   & \\

## |[fill=orange!10]| &  &   & \\

## |[fill=orange!30]| &  &   & \\

## |[fill=orange!50]| &  &   & \\

## |[fill=orange!70]| &   &   & \\

## };

## 

## \matrix (second) [above right=1cm and 5cm of first,anchor=north west,table,text width=4em]{

## &  &   & \\

## |[fill=orange!10]| &  &   & \\

## |[fill=orange!30]| &  &   & \\

## };

## 
## \matrix (third) [below=1cm of second,anchor=north,table,text width=4em]{

## &  &   & \\

## |[fill=orange!50]| &  &   & \\

## |[fill=orange!70]| &  &   & \\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {group\_by()} ++($(second.west|-first) +(-0.5cm, 0)$);

## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Grouped)  {Grouped data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(6)

## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
    summarise(Mean=mean(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(6)

## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
    summarise(Mean=mean(Temp), Var=var(Temp), N=n(),First=first(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
    summarise(Mean=mean(Temp))

## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
    mutate(Mean=mean(Temp))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup', echo=2------------------------------------------------
options(width=200)
data.1 %>% group_by(Between,Plot) %>%
  mutate(Mean=mean(Temp), cTemp=Temp-Mean)
options(width=70)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
  summarise(across(everything(), mean))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(-Cond,-Time) %>% group_by(Between,Plot) %>%
    summarise_all(list(mean))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% group_by(Between,Plot) %>%
    summarise(across(c(Temp, LAT, LONG), list(Mean=mean, SE=SE)))


## ---- echo=FALSE--------------------------------------------------------------
options(width=90)


## ---- results='markup'--------------------------------------------------------
tikus[1:10,c(1:3,76:77)]


## ---- results='markdown', eval=FALSE------------------------------------------
## tikus %>% arrange(`Pocillopora damicornis`)


## ---- results='markup'--------------------------------------------------------
tikus %>% group_by(time) %>%
    summarise(MeanAbundance=mean(`Pocillopora damicornis`))


## ---- echo=FALSE--------------------------------------------------------------
options(width=110)


## ---- results='markup'--------------------------------------------------------
nasa = as.data.frame(nasa)
head(nasa)


## ---- results='markup'--------------------------------------------------------
nasa %>% group_by(year) %>%
    summarise(N=n(),Mean=mean(ozone), Var=var(ozone))


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 1/.style={

##             nodes={text width=2em,font=\bfseries}

##         },

##         column 2/.style={

##           nodes={text width=2em, fill=blue!40}

##         },

##         column 3/.style={

##           nodes={text width=2em, fill=blue!40}

##         },

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## 
## \matrix (first) [table,text width=4em]{

## & |[fill=blue!70]| & |[fill=blue!70]| & & |[fill=blue!70]|\\

## &  &  & &\\

## &  &  & &\\

## &  &  & &\\

## &  &  & &\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!70]|& |[fill=blue!70]| & |[fill=blue!70]| \\

## |[fill=blue!40]|&  &  \\

## |[fill=blue!40]|&  &  \\

## |[fill=blue!40]|&  &  \\

## |[fill=blue!40]|&  &  \\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {select()} ++(second);

## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Subset)  {Subset data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% select(Between,Plot,Cond,Time,Temp)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(-LAT,-LONG)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(contains('L'))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(starts_with('L'))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(ends_with('t'))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(matches('^T[a-z]m.'))


## ----regex, cache=TRUE,echo=FALSE---------------------------------------------
system("convert -resize 650x ../resources/regex.pdf ../resources/regex.png")
system("mv ../resources/regex.png ../resources/regex.png")


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% select(Between:Temp)


## ---- echo=FALSE,results='markup'---------------------------------------------
nasa = as.data.frame(nasa)


## ---- echo=TRUE,results='markup'----------------------------------------------
nasa %>% head()


## ---- results='markup', echo=2:10---------------------------------------------
options(width=100)
nasa %>% head()


## ---- results='markup'--------------------------------------------------------
nasa %>% select(lat, long, starts_with("cloud")) %>% head


## ---- echo=FALSE--------------------------------------------------------------
options(width=90)


## ---- results='markup'--------------------------------------------------------
tikus[1:10,c(1:3,76:77)]


## ---- eval=FALSE,results='markup'---------------------------------------------
## tikas %>% dplyr::select(-contains('pora'))
## ## OR if we wanted to alter the order...
## tikas %>% dplyr::select(rep, time, everything(),-contains('pora'))


## ---- eval=TRUE,results='markup'----------------------------------------------
dplyr::select(tikus, `Pocillopora damicornis`)


## ---- eval=TRUE,results='markup'----------------------------------------------
data.1 %>% pull(Temp)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% rename(Condition=Cond, Temperature=Temp)


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## 
## \matrix (first) [table,text width=4em]{

## &  &  &\\

## &  &  &\\

## |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]| \\

## |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]| \\

## &  &  &\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!70]|& |[fill=blue!70]| & |[fill=blue!70]| & |[fill=blue!70]|  \\

## |[fill=blue!40]|& |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]|  \\

## |[fill=blue!40]|& |[fill=blue!40]| & |[fill=blue!40]| & |[fill=blue!40]|  \\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {filter()} ++(second);

## 
## \node [Title,above=0.75cm of first.north,anchor=north] (Raw)  {Raw data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Filtered)  {Filtered data};

## \end{tikzpicture}


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% filter(Cond=='H')


## ---- results='markup'--------------------------------------------------------
data.1 %>% filter(Cond %in% c('H','M'))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% filter(Cond=='H' & Temp<25)


## ---- results='markup'--------------------------------------------------------
data.1 %>% filter(Cond=='H' | Temp<25)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
data.1 %>% filter(Temp<20 & (LAT>20 |  LONG <145))


## ---- results='markup'--------------------------------------------------------
glimpse(nasa)


## ---- eval=FALSE,results='markup'---------------------------------------------
## nasa %>% filter(year==max(year) & month==2) %>%
##     arrange(-ozone) %>% head(5)
## nasa %>% filter(year==max(year) & month==2) %>%
##     arrange(-ozone) %>% slice(1:5)
## ##OR
## nasa %>% filter(year==max(year) & month==2 ) %>%
##     top_n(5, ozone)
## 


## ---- results='markup'--------------------------------------------------------
glimpse(nasa)


## ---- eval=FALSE,results='markup'---------------------------------------------
## nasa %>% filter(ozone > 320 & ozone<325, month==first(month),
##        year==last(year))
## ##OR
## nasa %>% filter(between(ozone,320,325), month==first(month),
##        year==last(year))
## 


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% slice(1:4)


## ---- results='markup'--------------------------------------------------------
data.1 %>% slice(c(1:4,7))


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% sample_n(10, replace=TRUE)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)

## ---- results='markup'--------------------------------------------------------
data.1 %>% sample_frac(0.5, replace=TRUE)


## ---- results='markup'--------------------------------------------------------
data.1 %>% head(2)


## ---- results='markup'--------------------------------------------------------
#examine the levels of the Cond factor
levels(data.1$Cond)


## ---- results='markup'--------------------------------------------------------
#subset the dataset to just Cond H
data.3 <- data.1 %>% filter(Plot=='P1')
#examine subset data
data.3
#examine the levels of the Cond factor
levels(data.3$Cond)
levels(data.3$Plot)
levels(data.3$Between)


## ---- results='markup'--------------------------------------------------------
#subset the dataset to just Cond H
data.3 <-  data.1 %>% filter(Plot=='P1')
#drop the unused factor levels from all factors
data.3 <- data.3 %>% droplevels()
#examine the levels of each factor
levels(data.3$Cond)
levels(data.3$Plot)
levels(data.3$Between)


## ---- results='markup'--------------------------------------------------------
#subset the dataset to just Cond H
data.3 <- data.1 %>% filter(Plot=='P1')
#drop the unused factor levels from Cond
data.3 <- data.3 %>% mutate(Plot=factor(Plot))
#examine the levels of each factor
levels(data.3$Cond)
levels(data.3$Plot)
levels(data.3$Between)


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## &|[fill=blue!40!black!40]|  & |[fill=blue!60!black!60]|  & |[fill=blue!80!black!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## & |[fill=black!60]|& |[fill=black!60]| \\

## & |[fill=blue!40!black!40]|& |[fill=blue!40!white!40]|\\

## & |[fill=blue!40!black!40]|& |[fill=blue!40!white!40]|\\

## & |[fill=blue!60!black!60]|& |[fill=blue!60!white!60]|\\

## & |[fill=blue!60!black!60]|& |[fill=blue!60!white!60]|\\

## & |[fill=blue!80!black!80]|& |[fill=blue!80!white!80]|\\

## & |[fill=blue!80!black!80]|& |[fill=blue!80!white!80]|\\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {pivot\_longer()} ++(second);

## 
## 
## \node [Title,above=0.75cm of first.north,anchor=north] (Wide)  {Wide data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Long)  {Long data};

## \end{tikzpicture}


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\bfseries

##             }

##         }

##     }

## }

## \begin{tikzpicture}

## \matrix (first) [table,text width=4em]{

## & |[fill=black!60]|& |[fill=black!60]| \\

## & |[fill=blue!40!black!40]|& |[fill=blue!40!white!40]|\\

## & |[fill=blue!40!black!40]|& |[fill=blue!40!white!40]|\\

## & |[fill=blue!60!black!60]|& |[fill=blue!60!white!60]|\\

## & |[fill=blue!60!black!60]|& |[fill=blue!60!white!60]|\\

## & |[fill=blue!80!black!80]|& |[fill=blue!80!white!80]|\\

## & |[fill=blue!80!black!80]|& |[fill=blue!80!white!80]|\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## &|[fill=blue!40!black!40]|  & |[fill=blue!60!black!60]|  & |[fill=blue!80!black!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## &|[fill=blue!40!white!40]|  & |[fill=blue!60!white!60]|  & |[fill=blue!80!white!80]|\\

## };

## 
## \draw[-latex,line width=8pt] (first) -- node[code,above,scale=2] {pivot\_wider()} ++(second);

## 
## 
## \node [Title,above=0.75cm of first.north,anchor=north] (Wide)  {Wide data};

## \node [Title,above=0.75cm of second.north,anchor=north] (Long)  {Long data};

## \end{tikzpicture}


## ----data.w,echo=FALSE, results='asis'----------------------------------------
## set.seed(1)
## data.w <- expand.grid(Plot=paste("P",1:4,sep=""))
## data.w$Between <- gl(2,2,4,lab=paste("A",1:2,sep=""))
## data.w <- with(data.w,data.frame(Between,Plot,matrix(rpois(12,10),ncol=3, dimnames=list(paste("R",1:4,sep=""),paste("Time",0:2,sep=":")))))
#library(xtable)
#xTab<-xtable(data.w)
#align(xTab)<-rep("middle",6)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0'  class='plainTable' style='font-size:50%;'")
#)
#save(data.1, data.2, data.w, file='../data/manipulationDatasets.RData')
pandoc.table(data.w)
#knitr::kable(data.w)


## ---- results='markup', results='hold'----------------------------------------
data.w %>% pivot_longer(Time.0:Time.2,  names_to = 'Time',
                        values_to='Count')
## OR
data.w %>% pivot_longer(c(-Between, -Plot),  names_to = 'Time',
                        values_to='Count')


## ----data.w,echo=FALSE, results='asis'----------------------------------------
## set.seed(1)
## data.w <- expand.grid(Plot=paste("P",1:4,sep=""))
## data.w$Between <- gl(2,2,4,lab=paste("A",1:2,sep=""))
## data.w <- with(data.w,data.frame(Between,Plot,matrix(rpois(12,10),ncol=3, dimnames=list(paste("R",1:4,sep=""),paste("Time",0:2,sep=":")))))
#library(xtable)
#xTab<-xtable(data.w)
#align(xTab)<-rep("middle",6)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0'  class='plainTable' style='font-size:50%;'")
#)
#save(data.1, data.2, data.w, file='../data/manipulationDatasets.RData')
pandoc.table(data.w)
#knitr::kable(data.w)


## ---- results='markup', results='hold'----------------------------------------
## OR
data.w %>% pivot_longer(starts_with('Time'),  names_to = 'Time',
                        values_to='Count',
                        names_prefix='Time.')


## ----dataAA,echo=FALSE, results='asis'----------------------------------------
set.seed(1)
data <- expand.grid(Within=paste("B",1:2,sep=""),Subplot=paste("S",1:2,sep=""),Plot=paste("P",1:6,sep=""))
data$Subplot <- gl(12,2,24,lab=paste("S",1:12,sep=""))
data$Between <- gl(3,4,24,lab=paste("A",1:3,sep=""))
data$Resp1 <- rpois(24,10)
data$Resp2 <- rpois(24,20)
data <- with(data,data.frame(Resp1,Resp2,Between,Plot,Subplot,Within))
library(xtable)
xTab<-xtable(data)
align(xTab)<-rep("middle",7)
## print(xTab,type="html",include.rownames=T,only.contents=F,
## 			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),,
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:40%;'")
## )
#save(data.1, data.2, data.w, data, file='../data/manipulationDatasets.RData')
pandoc.table(data)
#knitr::kable(data)


## ---- results='markup'--------------------------------------------------------
data %>% head(2)


## ---- results='markup'--------------------------------------------------------
data %>% select(-Resp2) %>%
  pivot_wider(names_from=Within,  values_from=c(Resp1))


## ---- results='markup'--------------------------------------------------------
data %>% head(2)

## ---- results='markup'--------------------------------------------------------
data %>% pivot_wider(names_from=Within, values_from=c(Resp1, Resp2))


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20,

##             font=\ttfamily\bfseries

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\ttfamily\bfseries

##             }

##         }

##     }

## }

## 
## \begin{tikzpicture}

## 
## \matrix (first) [table,text width=4em]{

## x1&x2\\

## A& 1\\

## B& 2\\

## C& 3\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!60!black!60]| x1 & |[fill=blue!60!black!60]| x3\\

## |[fill=blue!60!white!60]| A& |[fill=blue!60!white!60]| T\\

## |[fill=blue!60!white!60]| B& |[fill=blue!60!white!60]| F\\

## |[fill=blue!60!white!60]| D& |[fill=blue!60!white!60]| T\\

## };

## 
## \matrix (third) [right=5cm of second,anchor=west,table,text width=4em]{

## x1 & x2 & |[fill=blue!60!black!60]| x3\\

## A & 1 & |[fill=blue!60!white!60]| T\\

## B & 2 & |[fill=blue!60!white!60]| F\\

## C & 3 & |[fill=none]| NA\\

## };

## 
## \draw [-latex,line width=8pt](second) -- (third);

## \node at ($(first.east) !0.5! (second.west)$) [code,scale=2] (leftjoin) {left\_join()};

## 
## \node [Title,above=0.75cm of first.north,anchor=north] (A)  {Left dataset};

## \node [Title,above=0.75cm of second.north,anchor=north] (B)  {Right dataset};

## \end{tikzpicture}


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20,

##             font=\ttfamily\bfseries

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\ttfamily\bfseries

##             }

##         }

##     }

## }

## 
## \begin{tikzpicture}

## 
## \matrix (first) [table,text width=4em]{

## x1&x2\\

## A& 1\\

## B& 2\\

## C& 3\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!60!black!60]| x1 & |[fill=blue!60!black!60]| x3\\

## |[fill=blue!60!white!60]| A& |[fill=blue!60!white!60]| T\\

## |[fill=blue!60!white!60]| B& |[fill=blue!60!white!60]| F\\

## |[fill=blue!60!white!60]| D& |[fill=blue!60!white!60]| T\\

## };

## 
## \matrix (third) [right=5cm of second,anchor=west,table,text width=4em]{

## x1 & x2 & |[fill=blue!60!black!60]| x3\\

## A & 1 & |[fill=blue!60!white!60]| T\\

## B & 2 & |[fill=blue!60!white!60]| F\\

## };

## 
## \draw [-latex,line width=8pt](second) -- (third);

## \node at ($(first.east) !0.5! (second.west)$) [code,scale=2] (innerjoin) {inner\_join()};

## 
## \node [Title,above=0.75cm of first.north,anchor=north] (A)  {Left dataset};

## \node [Title,above=0.75cm of second.north,anchor=north] (B)  {Right dataset};

## \end{tikzpicture}


## \usetikzlibrary{shapes,arrows,shadows,positioning,mindmap,backgrounds,decorations, calc,fit, decorations.pathreplacing,decorations.pathmorphing, shadings,shapes.geometric, shapes.multipart,patterns}

## \tikzstyle{Title} = [font={\fontspec[Scale=2]{Complete in Him}}]

## \tikzstyle{code} = [font=\ttfamily]

## \tikzset{

##     table/.style={

##         matrix of nodes,

##         row sep=-\pgflinewidth,

##         column sep=-\pgflinewidth,

##         nodes={

##             rectangle,

##             draw=white,

##             align=center,

##             fill=black!20,

##             font=\ttfamily\bfseries

##         },

##         minimum height=1.5em,

##         text depth=0.5ex,

##         text height=2ex,

##         nodes in empty cells,

## %%

##         %%every even row/.style={

##         %%    nodes={fill=gray!30}

##         %%},

##         column 5/.style={

##             nodes={

##                 text width=4em,

##                 fill=blue!40

##             }

##         },

##         row 1/.style={

##             nodes={

##                 fill=black!40,

##                 text=white,

##                 font=\ttfamily\bfseries

##             }

##         }

##     }

## }

## 
## \begin{tikzpicture}

## 
## \matrix (first) [table,text width=4em]{

## x1&x2\\

## A& 1\\

## B& 2\\

## C& 3\\

## };

## 
## \matrix (second) [right=5cm of first,anchor=west,table,text width=4em]{

## |[fill=blue!60!black!60]| x1 & |[fill=blue!60!black!60]| x3\\

## |[fill=blue!60!white!60]| A& |[fill=blue!60!white!60]| T\\

## |[fill=blue!60!white!60]| B& |[fill=blue!60!white!60]| F\\

## |[fill=blue!60!white!60]| D& |[fill=blue!60!white!60]| T\\

## };

## 
## \matrix (third) [right=5cm of second,anchor=west,table,text width=4em]{

## x1 & x2 & |[fill=blue!60!black!60]| x3\\

## A & 1 & |[fill=blue!60!white!60]| T\\

## B & 2 & |[fill=blue!60!white!60]| F\\

## C & 3 & |[fill=none]| NA\\

## D & |[fill=none]| NA & |[fill=blue!60!white!60]| T\\

## };

## 
## \draw [-latex,line width=8pt](second) -- (third);

## \node at ($(first.east) !0.5! (second.west)$) [code,scale=2] (fulljoin) {full\_join()};

## 
## \node [Title,above=0.75cm of first.north,anchor=north] (A)  {Left dataset};

## \node [Title,above=0.75cm of second.north,anchor=north] (B)  {Right dataset};

## \end{tikzpicture}


## ----data.bio,echo=FALSE, results='asis'--------------------------------------
set.seed(1)
data.bio <- expand.grid(Subplot=paste("S",1:2,sep=""),Plot=paste("P",1:6,sep=""))
data.bio$Subplot <- gl(12,1,12,lab=paste("S",1:12,sep=""))
data.bio$Between <- gl(3,4,12,lab=paste("A",1:3,sep=""))
data.bio$Resp1 <- rpois(12,10)
data.bio$Resp2 <- rpois(12,20)
data.bio <- with(data.bio,data.frame(Resp1,Resp2,Between,Plot,Subplot))
data.bio<-data.bio[-3,]

#xTab<-xtable(data.bio)
##align(xTab)<-rep("middle",6)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:40%;'")
#)
#save(data.1, data.2, data.w, data, data.bio,file='../data/manipulationDatasets.RData')
pandoc.table(data.bio)


## ----data.chem,echo=FALSE, results='asis'-------------------------------------
set.seed(1)
data.chem <- expand.grid(Subplot=paste("S",1:2,sep=""),Plot=paste("P",1:6,sep=""))
data.chem$Subplot <- gl(12,1,12,lab=paste("S",1:12,sep=""))
data.chem$Between <- gl(3,4,12,lab=paste("A",1:3,sep=""))
data.chem$Chem1 <- rlnorm(12,1)
data.chem$Chem2 <- rlnorm(12,.5)
data.chem <- with(data.chem,data.frame(Chem1,Chem2,Between,Plot,Subplot))
data.chem<-data.chem[-7,]
#library(xtable)
#xTab<-xtable(data.chem)
##align(xTab)<-rep("middle",4)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:40%;'")
#)
#save(data.1, data.2, data.w, data, data.bio, data.chem, file='../data/manipulationDatasets.RData')
pandoc.table(data.chem)


## ---- echo=FALSE--------------------------------------------------------------
options(width=70)

## ---- results='markup'--------------------------------------------------------
data.bio %>% inner_join(data.chem)


## ---- echo=FALSE--------------------------------------------------------------
options(width=70)

## ---- results='markup'--------------------------------------------------------
data.bio %>% full_join(data.chem)


## ---- echo=FALSE--------------------------------------------------------------
options(width=70)

## ---- results='markup'--------------------------------------------------------
data.bio %>% left_join(data.chem)


## ---- echo=FALSE--------------------------------------------------------------
options(width=70)

## ---- results='markup'--------------------------------------------------------
data.bio %>% right_join(data.chem)


## ----data.bio1,echo=FALSE, results='markup'-----------------------------------
#library(xtable)
#xTab<-xtable(data.bio)
##align(xTab)<-rep("middle",4)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:40%;'")
#)
data.bio


## ----data.geo,echo=FALSE, results='markup'------------------------------------
set.seed(1)
data.geo <- expand.grid(Plot=paste("P",1:6,sep=""))
data.geo$LAT<-c(17.9605,17.5210,17.0011,18.235,18.9840,20.1154)
data.geo$LONG<-c(145.4326,146.1983,146.3839,146.7934,146.0345,146.4672)
library(xtable)
#xTab<-xtable(data.geo)
#align(xTab)<-rep("middle",4)
#print(xTab,type="html",include.rownames=T,only.contents=F,
#			  include.colnames=T,sanitize.rownames.function=function(x) paste('<b>',x,'</b>'),
#      html.table.attributes=list("border='3' cellpadding='2' cellspacing='0' class='plainTable' style='font-size:40%;'")
#)
#save(data.1, data.2, data.w, data, data.bio, data.bio1, data.chem, data.geo, file='../data/manipulationDatasets.RData')
data.geo


## ---- results='markup'--------------------------------------------------------
data.bio %>% left_join(data.geo,by=c("Plot"))


## ---- echo=FALSE, results='markup'--------------------------------------------
library(mvabund) 
data(tikus)
tikus <- cbind(tikus$abund, tikus$x)
select(tikus, starts_with('Psammocora'), time,rep) %>% slice(1:10)
glimpse(tikus)


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`)


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
##     gather(Species, Abundance,-time,-rep)


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
##     gather(Species, Abundance,-time,-rep) %>%
##         mutate(Cover=Abundance/10)


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
##     gather(Species, Abundance,-time,-rep) %>%
##         mutate(Cover=Abundance/10) %>%
##             separate(Species,c('Genera','Species'))


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
##     gather(Species, Abundance,-time,-rep) %>%
##         mutate(Cover=Abundance/10) %>%
##             separate(Species,c('Genera','Species')) %>%
##                 filter(Genera=='Acropora')


## ---- results='markup', eval=FALSE--------------------------------------------
## tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
##     gather(Species, Abundance,-time,-rep) %>%
##         mutate(Cover=Abundance/10) %>%
##             separate(Species,c('Genera','Species')) %>%
##                 filter(Genera=='Acropora') %>%
##                     group_by(time,rep) %>%
##                         summarise(SumCover=sum(Cover))


## ---- results='markup', eval=TRUE---------------------------------------------
tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
    gather(Species, Abundance,-time,-rep) %>%
        mutate(Cover=Abundance/10) %>%
            separate(Species,c('Genera','Species')) %>%
                filter(Genera=='Acropora') %>%
                    group_by(time,rep) %>%
                        summarise(SumCover=sum(Cover)) %>%
                            group_by(time) %>%
                                summarise(Mean=mean(SumCover),
                                          Var=var(SumCover))


## ---- results='markup', eval=TRUE---------------------------------------------
tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
    gather(Species, Abundance,-time,-rep) %>%
        mutate(Cover=Abundance/10) %>%
            separate(Species,c('Genera','Species')) %>%
                filter(Genera=='Acropora') %>%
                    group_by(time,rep) %>%
                        summarise(SumCover=sum(Cover)) %>%
                            group_by(time) %>%
                                summarise(Mean=mean(SumCover),
                                          Var=var(SumCover))


## ---- results='markup', eval=TRUE---------------------------------------------
tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
    gather(Species, Abundance,-time,-rep) %>%
        mutate(Cover=Abundance/10) %>%
            separate(Species,c('Genera','Species')) %>%
                    group_by(time,rep,Genera) %>%
                        summarise(SumCover=sum(Cover)) %>%
                            group_by(time,Genera) %>%
                                summarise(Mean=mean(SumCover),
                                          Var=var(SumCover))


## ---- results='markup', eval=TRUE---------------------------------------------
tikus %>% rename(`Acropora aspera`=`Acropera aspera`) %>%
    gather(Species, Abundance,-time,-rep) %>%
        mutate(Cover=Abundance/10) %>%
            separate(Species,c('Genera','Species')) %>%
                    group_by(time,rep,Genera) %>%
                        summarise(SumCover=sum(Cover)) %>%
                            group_by(time,Genera) %>%
                                summarise(Mean=mean(SumCover),
                                          Var=var(SumCover)) %>%
                                              top_n(3,Mean) %>%
                                                  arrange(desc(Mean))

