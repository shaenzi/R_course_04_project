# read the locally saved/raw shape files. These are available as open government data from https://www.stadt-zuerich.ch/geodaten/download/Stadtkreise?format=10007
zurich_kreise <- sf::st_read("data-raw/stadtkreise/stzh.adm_stadtkreise_a.shp") %>%
  dplyr::select(-objid, -entstehung)

usethis::use_data(zurich_kreise, overwrite = TRUE)
