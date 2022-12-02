# read the locally saved/raw shape files. These are available as open government data from https://www.stadt-zuerich.ch/geodaten/download/Stadtkreise?format=10007

geojson_link <- "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Stadtkreise?service=WFS&version=1.1.0&request=GetFeature&outputFormat=GeoJSON&typename=adm_stadtkreise_a"

zurich_kreise <- rjson::fromJSON(file=geojson_link)

usethis::use_data(zurich_kreise, overwrite = TRUE)
