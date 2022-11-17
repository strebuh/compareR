# library(help = "datasets")
# ls(datasets)


# Gerenrate testing data
usa_data <- data.table::copy(state.x77) %>% as.data.frame %>% data.table::setDT(.) %>%
  .[, `:=`(state = datasets::state.name,
           region = datasets::state.region,
           lon = datasets::state.center$x,
           lat = datasets::state.center$y)] %>%
  data.table::setcolorder(., c((ncol(.)-3):ncol(.), 1:(ncol(.)-4)))
usa_data[, rand_status := sample(c("status1", "status2"), nrow(usa_data), replace=T, prob=c(.4, .6))]
usa_data[, rand_class := sample(c("class1", "class2", "class3"), nrow(usa_data), replace=T, prob=c(.33, .33, .33))]
categ <- c(1:4, 13, 14)
data.table::setcolorder(usa_data, c(categ, setdiff(seq_len(ncol(usa_data)), categ)))
usethis::use_data(usa_data, overwrite = TRUE)


# Generate testing data with NA values
categ = 1:6
numb = names(usa_data)[setdiff(seq_len(ncol(usa_data)), categ)]
na_perc <- 0.05
usa_data_na <- cbind(usa_data[, ..categ], purrr::map_df(usa_data[,..numb], ~.x[sample(c(TRUE, NA), prob = c(1-na_perc, na_perc), size = length(.x), replace = TRUE)]))
data.table::setDT(usa_data_na)
usethis::use_data(usa_data_na, overwrite = TRUE)



# USA sf object
usa_sf <- USAboundaries::states_contemporary_lores
usa_sf <- usa_sf[,c("geoid", "state_name", "state_abbr", "jurisdiction_type", "aland", "awater")]
usethis::use_data(usa_sf, overwrite = TRUE)


# USA mainland (without Alaska, Hawaii and other oversease territories) sf object
usa_mainland_sf <- USAboundaries::states_contemporary_lores
usa_mainland_sf <- usa_mainland_sf[,c("geoid", "state_name", "state_abbr", "jurisdiction_type", "aland", "awater")]
usa_mainland_sf <- prep_sf(usa_mainland_sf, lon="lon", lat="lat", crs=4326, bbox=c(xmin=-125.200195, ymin=23.523700, xmax=-66.533203, ymax=49.439557), spat_obj_type="MULTIPOLYGON")
# plot(sf::st_geometry(usa_mainland_sf))
usethis::use_data(usa_mainland_sf, overwrite = TRUE)

