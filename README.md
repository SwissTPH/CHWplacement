![Build status: main](https://img.shields.io/github/workflow/status/SwissTPH/VivaxModelR/R-CMD-check/main?style=flat-square)
![Latest commit](https://img.shields.io/github/last-commit/SwissTPH/VivaxModelR/main?style=flat-square)


# CHWplacement

This R package contains functions that can be used to compute CHW placement scenarios, based on gridded population surfaces, friction surfaces and additional datasets such as GPS coordinates of existing health facilities.

This code is associated with the following manuscript:
Champagne C., Rajkumar A. S. , Auxila P., Perrone G., Plötz M., Young A., Napier H.,Le Menach A., Battle K., Cameron E., Alfred J.P., Deslouches Y.G., Pothin E. **"Improving access to care and community health in Haiti with optimized community health worker placement."**


The package can be installed using the devtools package:  

```{r}
library(devtools)  
install_github("SwissTPH/CHWplacement", build_vignettes = TRUE)  
```
The vignette presenting how to use the package can be accessed with the following command:
```{r}
browseVignettes("CHWplacement")
```

The scripts necessary to reproduce the manuscript figures are situated under demo/haiti.  The code is designed for running on a high performance cluster (HPC) with the slurm job scheduler. The scenarios are launched from the file called launch_all_scenarios.R and requires that the input data is stored in a sub-folder called inputs. The code for visualizing the outputs and reproduce the manuscript figures is in Post_Processing_figures.

