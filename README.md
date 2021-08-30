# CHWplacement

This R package contains functions that can be used to compute CHW placement scenarios, based on gridded population surfaces, friction surfaces and additional datasets such as GPS coordinates of existing health facilities.

This code is associated with the following manuscript:
Champagne C., Rajkumar A. S. , Auxila P., Perrone G., Plötz M., Young A., Napier H.,Le Menach A., Battle K., Cameron E., Alfred J.P., Deslouches Y.G., Pothin E. **"Improving access to care and community health in Haiti with optimized community health worker placement."**


The package can be installed using the devtools package:  

```{r}
library(devtools)  
install_github("SwissTPH/CHWplacement", build_vignettes = TRUE)  
```

The scripts necessary to reproduce the manuscript figures are situated under demo/haiti.  

The vignette presenting how to use the package can be accessed with the following command:
```{r}
browseVignettes("CHWplacement")
```
