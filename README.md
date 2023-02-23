# cytoviewer

An interactive multi-channel image viewer for R. 

This shiny application allows users to interactively visualize multi-channel 
images and masks. The `cytoviewer` package is divided into image-level (Composite and Channels) 
and cell-level visualization (Masks). It allows users to overlay individual images 
with masks and integrates well with [SingleCellExperiment](https://bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html) 
and [SpatialExperiment](https://bioconductor.org/packages/release/bioc/html/SingleCellExperiment.html) objects for metadata visualization. 


## Requirements

The `cytoviewer` package requires R version >= 4.0.
It builds on data objects and functions contained in the [cytomapper](https://bioconductor.org/packages/release/bioc/html/cytomapper.html) package. 

## Installation 

Please note that `cytoviewer` is under active development.

The development version of the `cytoviewer` package can be installed from Github 
using `remotes` in R as follows:

```r
# install.packages("remotes")

remotes::install_github("BodenmillerGroup/cytoviewer", build_vignettes = TRUE, dependencies = TRUE)
```

To load the package in your R session, type the following:

```r
library(cytoviewer)
```

## Contributing

For feature requests, please open an issue [here](https://github.com/BodenmillerGroup/cytoviewer/issues).

Alternatively, feel free to fork the repository, add your changes and issue a pull request.

## Authors

[Lasse Meyer](https://github.com/lassedochreden) lasse.meyer 'at' dqbm.uzh.ch

[Nils Eling](https://github.com/nilseling) nils.eling 'at' dqbm.uzh.ch
