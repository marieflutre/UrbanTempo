##' Package "UrbanTempo"
##'
##' This package manages urban temporalities from observational data, similarly to figure 4.6 in Marie Gibert's PhD thesis (\href{https://www.academia.edu/7549254}{2014}).
##'
##' First of all, before using the UrbanTempo package, you need to define your research question and choose the urban location you are interested in.
##' Then, you can start the long process of collecting data on urban temporalities.
##' Once this is done, the goal of this package is to help you drawing a figure representing the urban temporalities you observed.
##'
##' In order to use the UrbanTempo package, you will need to save your data into a "plain text" file formatted in a specific way so that it can be loaded without error.
##' To help you preparing such a file, you can see an example available \href{https://github.com/marieflutre/UrbanTempo#example-of-input-file}{online}.
##' Once your data are properly formatted, you can load them via the \code{\link{readTemporalities}} function.
##' The documentation of this function, available via \code{?readTemporalities}, provides an example.
##'
##' Once your data are loaded, you will need to specify how you want them to appear on the figure (their colors, etc).
##' Such appearances should be saved into a "plain text" file (an example is also available \href{https://github.com/marieflutre/UrbanTempo#example-of-input-file}{online}).
##' Such a file can then be loaded via the \code{\link{readAppearances}} function.
##' The documentation of this function, available via \code{?readAppearances}, provides an example.
##'
##' At this stage, you will be ready to make your first figure, via the \code{\link{plotTemporalities}} function.
##' See also the \code{\link{plotLegend}} function.
##'
##' An example of the full process is described in the tutorial available via the following command: \code{browseVignettes("UrbanTempo")}.
"_PACKAGE"
##> [1] "_PACKAGE"
