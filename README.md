
<!-- README.md is generated from README.Rmd. Please edit that file -->

# c4app

<!-- badges: start -->
<!-- badges: end -->

This shiny app is a project done as part of the
[EPFL’s](https://www.extensionschool.ch/) Data visualisation and
communication
[course](https://www.extensionschool.ch/learn/applied-data-science-communication-visualization).

Once I am done I will most likely put it on [shinyapps.io](), so you can
try it live :-)

The data shown is from the [open government data portal from the city of
zurich](https://data.stadt-zuerich.ch/dataset?q=schutz+und+rettung) and
concerns the emergency services: How fast do they respond? Which areas
(“Stadtkreise”) of the city do they reach within the target time of 10
minutes?

## Installation

You can install the development version of c4app from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("shaenzi/R_course_04_project")
```

Reminder to myself:

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

For EPFL folks: I note down my questions in the [dev
notes](./dev/dev_notes.html)
