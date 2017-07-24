**UrbanTempo**
==============

This directory contains the UrbanTempo package for the [R](https://www.r-project.org/) programming language and software environment for statistical computing.
This package contains computer code to **plot urban temporalities from observational data**, similarly to figure 4.6 in Marie Gibert's [PhD thesis](https://www.academia.edu/7549254) (2014) as well as a figure in her forthcoming book chapter (Southeast Asia urbanization, Routledge handbook).
As such, this package can be **of interest to urban geographers and anthropologists**.

This software is available under a [free software](https://en.wikipedia.org/wiki/Free_software) license, the [GNU Affero General Public License](https://www.gnu.org/licenses/agpl.html) (version 3 and later).
See the COPYING file for details.
For more details about free software, you can refer to the [information](https://www.gnu.org/philosophy/philosophy.en.html) provided by the [Free Software Foundation](https://en.wikipedia.org/wiki/Free_Software_Foundation).

The content of this package is versioned using the [git](http://www.git-scm.com/) software, the central repository being hosted on the [GitHub](https://github.com/marieflutre/UrbanTempo) website.


# Installation

You, the user, first need to install on your computer the R software (details [here](https://cloud.r-project.org/)).
This software is available under a free software license, hence is best used on a computer with a free operating system such as GNU/Linux, but it can also be used on Windows and Mac operating systems.

Once this is done, it is recommended to also install the RStudio software (the Desktop version; details [here](https://www.rstudio.com/products/rstudio/#Desktop)).
This software is also available under a free software license, hence is best used on a computer with a free operating system such as GNU/Linux, but it can also be used on Windows and Mac operating systems.
For Mac users, if asked, there is no need to install "git" or "XCode" tools.

Once this is done, launch RStudio, which will also automatically launch an interactive R session.
At this stage, you still need to install the devtools package, along with its dependencies.
This is easily done via the following command, to be copy-pasted and executed in the R console of RStudio:
```
install.packages("devtools")
```

Once this is done, you need to load the devtools package into the session, and install the UrbanTempo package, via the following commands, to be copy-pasted and executed in the R console:
```
library(devtools)
install_github("marieflutre/UrbanTempo", build_vignettes=TRUE)
```

Once this is done, the UrbanTempo package should be available on your computer.


# Usage

Once the Urban Tempo package is available on your computer, it can be loaded into a R session:
```
library(UrbanTempo)
```

You are encouraged to first read the introductory tutorial in details:
```
browseVignettes("UrbanTempo")
```

This should automatically display the R vignette coming with the package into your default web browser, such as [Firefox](https://en.wikipedia.org/wiki/Firefox) (free software).

Some information about the package is also available directly via the R console:
```
help(package="UrbanTempo")
```

For any given function, its documentation is available by adding the question mark symbol, `?`, in front of it, for instance:
```
?readTemporalities
```

If you are new to R, it is advised to read its official [documentation](https://cran.r-project.org/manuals.html).
Other useful sources of information are listed below:

* https://cran.r-project.org/faqs.html

* https://cran.r-project.org/doc/contrib/Baggott-refcard-v2.pdf

* http://www.statmethods.net

* http://grunwaldlab.github.io/Reproducible-science-in-R

* http://swirlstats.com


# Citation

The authors invested time and effort in creating the UrbanTempo package, please cite it when using it:
```
citation("UrbanTempo")
```

See also `citation()` for citing the R software itself.


# Example of input file

In the introductory tutorial (R vignette mentioned above), you will see how to reproduce part of figure 4.6 from Marie Gibert's PhD thesis and her forthcoming book chapter.
The data used to reproduce this figure are provided with the package as an example.
Once the package is installed on your computer, you can execute the following command in a R console:
```
system.file("extdata", "Gibert_2014_data-PhD-thesis.tsv",
            package="UrbanTempo")
```
which will return the path to the file.

But, more easily, you can also download the example file, by clicking [here](https://raw.githubusercontent.com/marieflutre/UrbanTempo/master/inst/extdata/Gibert_2014_data-PhD-thesis.tsv).
Do "right-click", then "Save as...".

The resulting file is in "plain text" format, with columns separated by a tabulation.
Such a file can be opened with virtually all softwares on all computers.
A typical usage would be to open it with the Calc software from the [LibreOffice](https://www.libreoffice.org/) suite (free software), or with the Excel software from the Microsoft Office suite.


# Issues

When encountering a problem with the package, you can report issues on GitHub directly ([here](https://github.com/marieflutre/UrbanTempo/issues)).
Remember to copy-paste the output of `sessionInfo()` to help efficiently diagnose the problem and find a solution.
