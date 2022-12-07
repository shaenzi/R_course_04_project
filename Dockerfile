FROM rocker/tidyverse:4.2.2
RUN install2.r rsconnect bslib config dplyr ggplot2 golem htmltools janitor leaflet
RUN install2.r lubridate magrittr markdown pkgload reactable readr sf shiny stringr tidyr
WORKDIR /home/c4app
COPY app.R app.R
COPY R R
COPY DESCRIPTION DESCRIPTION
COPY NAMESPACE NAMESPACE
COPY inst inst
COPY data data
COPY .Rbuildignore .Rbuildignore
COPY dev/deploy.R dev/deploy.R
CMD Rscript dev/deploy.R
