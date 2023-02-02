![Build status: main](https://img.shields.io/github/actions/workflow/status/SwissTPH/CHWplacement/r.yml?branch=main&style=flat-square)
![Latest commit](https://img.shields.io/github/last-commit/SwissTPH/CHWplacement/main?style=flat-square)
![coverage](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/clchampag/0ee1b8b341f3cca754974cab9bf9892b/raw/chw.json)


# CHWplacement

This R package contains functions that can be used to compute CHW placement scenarios, based on gridded population surfaces, friction surfaces and additional datasets such as GPS coordinates of existing health facilities.

This code is associated with the following manuscript:
 [**"Improving access to care and community health in Haiti with optimized community health worker placement."**](https://doi.org/10.1371/journal.pgph.0000167), Champagne C., Rajkumar A. S. , Auxila P., Perrone G., Plötz M., Young A., Bazaz Jazayeri S., Napier H., Le Menach A., Battle K., Cameron E., Alfred J.P., Deslouches Y.G., Pothin E., Plos Global Public Health 2022.


The package can be installed using the devtools package:  

```{r}
library(devtools)  
install_github("SwissTPH/CHWplacement")  
```
A tutorial presenting how to use the package can be accessed [here](https://swisstph.github.io/CHWplacement/articles/CHWplacement-vignette.html).

The scripts necessary to reproduce the manuscript figures are situated under [demo/haiti](https://github.com/SwissTPH/CHWplacement/tree/main/demo/haiti).  The code is designed for running on a high performance cluster (HPC) with the slurm job scheduler. The scenarios are launched from the file called launch_all_scenarios.R and requires that the input data is stored in a sub-folder called inputs. The code for visualizing the outputs and reproduce the manuscript figures is in Post_Processing_figures.

