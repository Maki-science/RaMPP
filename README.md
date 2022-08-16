
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RaMPP

<!-- badges: start -->
<!-- badges: end -->

It is well known that the spectral data of environmental microplastic
particles, in most cases, do not establish an accurate spectral match to
its pristine counterpart. We performed a study on how the Raman spectra
of a range of microplastic particles were investigated.Those have
created the so-called plastisphere. A surrounding layer of
microorganism-born biomolecules like, for example, carrotenoids. The
corresponding study can be found [here (TODO)](maki-science.org) for
further reading.

This package provides a library of Raman spectra of those mircroplastic
particles. Spectra are available for 11 different polymers. Each comes
along with a bunch of spectra for freshwater and sea water incubated
particles.

The data can be accessed directly (provided through this package), or
via the provided shiny app. The app provides easy and interactive access
and visualization of the Raman spectres of *RaMPP*. Additionally, this
app allows the comparison of the available spectra and also with a user
input spectrum. Upon mouse hovering on the provided graphs, it will
further show which components underlie the current positions’ spectrum
and whether this is a typical peak for a certain polymer.

## Installation

You can install the development version of RaMPP from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Maki-science/RaMPP")
```

## Example

For conveniently visualise and exploring the library, you can use a
shiny app, delivered with this package:

``` r
library(RaMPP)
# start the shiny app
RaMPP.lib()
```

The underlying data for this app can be accessed directly by loading
into the environment:

``` r
data("specData", envir = environment())
```
