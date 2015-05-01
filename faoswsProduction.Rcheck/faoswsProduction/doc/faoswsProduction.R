## ----setup, include=FALSE, cache=FALSE----------------------------------------
library(knitr)
opts_chunk$set(fig.path='figure/', fig.align='center', fig.show='hold',
               warning=FALSE, message=FALSE, error=FALSE, tidy=FALSE, 
               results='markup', eval=TRUE, echo=TRUE, cache=FALSE, dpi=200)
options(replace.assign=TRUE,width=80)
assign("depthtrigger", 10, data.table:::.global)

