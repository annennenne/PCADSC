# PCADSC

Development version of the R package PCADSC  (ref. to paper/description here)

To install the development version of PCADSC run the following commands
from within R:
```{r}
library(devtools)
install_github('annepetersen1/PCADSC')
```

The basic functionality of the package can be inspected using the following lines of code:
```{r}
library(PCADSC)

#Load iris data
data(iris)

#Define grouping variable, grouping the observations by whether their species is
#Setosa or not
iris$group <- "setosa"
iris$group[iris$Species != "setosa"] <- "non-setosa"

#Make a PCADSC object, splitting the data by "group"
irisPCADSC <-PCADSC(iris, "group", var=setdiff(names(iris), c("group", "Species")),
                    doCE = TRUE, doAngle = TRUE, doChroma = TRUE)
                         
#Make a cumulative eigenvalue (CE) plot
CEPlot(irisPCADSC)

#Make an angle plot 
anglePlot(irisPCADSC)

#Make a chroma plot
chromaPlot(irisPCADSC)
```

Our current to-do list for the package is posted below. If you have any further suggestions for added functionality, or have a bug to report, please open an issue and let us know.

_To do_
- Document everything
- Make `print()` methods for `pcaRes`, `CEInfo`, `angleInfo`, `chromaInfo` and `PCASDC` objects
- Make `summary()` methods for `pcaRes`, `CEInfo`, `angleInfo`, `chromaInfo` and `PCASDC` object
