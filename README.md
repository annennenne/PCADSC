# PCADSC

Development version of the R package PCADSC  (ref. to paper/description here)

To install the development version of PCADSC run the following commands
from within R

```{r}
library(devtools)
install_github('annepetersen1/PCADSC')
```

The basic functionality of the package can be inspected using the following lines of code:
```{r}
library(PCADSC)

#load iris data
data(iris)

#Define grouping variable, grouping the observations by whether their species is
#Setosa or not
iris$group <- "setosa"
iris$group[iris$Species != "setosa"] <- "non-setosa"

#make a PCADSC object, splitting the data by "group"
irisPCADSC <- makePCADSC(iris, "group", 
                             var=setdiff(names(iris), c("group", "Species")))

#plot it
qplot(irisPCADSC)

#print object overview
irisPCADSC
```
