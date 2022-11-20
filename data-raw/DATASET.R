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



# == USA state sf object
usa_sf <- USAboundaries::states_contemporary_lores
usa_sf <- usa_sf[,c("geoid", "state_name", "state_abbr", "jurisdiction_type", "aland", "awater")]
usethis::use_data(usa_sf, overwrite = TRUE)


# USA state mainland (without Alaska, Hawaii and other oversease territories) sf object
usa_mainland_sf <- USAboundaries::states_contemporary_lores
usa_mainland_sf <- usa_mainland_sf[,c("geoid", "state_name", "state_abbr", "jurisdiction_type", "aland", "awater")]
usa_mainland_sf <- prep_sf(usa_mainland_sf, lon="lon", lat="lat", crs=4326, bbox=c(xmin=-125.200195, ymin=23.523700, xmax=-66.533203, ymax=49.439557), spat_obj_type="MULTIPOLYGON")
# plot(sf::st_geometry(usa_mainland_sf))
usethis::use_data(usa_mainland_sf, overwrite = TRUE)


# == USA - region sf
usa_region_sf = sf::st_read(dsn="D:/spatial/usa_region", layer="cb_2018_us_region_20m")
lobstr::obj_size(usa_region_sf)
plot(sf::st_geometry(usa_region_sf))
lobstr::obj_size(usa_region_sf)
class(usa_region_sf)
usa_region_sf = if(grepl("+init=epsg:4326", sf::st_crs(usa_region_sf)$input)) usa_region_sf else sf::st_transform(usa_region_sf, "+init=epsg:4326")
unique(sf::st_geometry_type(usa_region_sf))
usethis::use_data(usa_region_sf, overwrite = TRUE)
usethis::use_r("usa_region_sf")


usa_region_mainland_sf <- prep_sf(usa_region_sf, lon="lon", lat="lat", crs=4326, bbox=c(xmin=-125.200195, ymin=23.523700, xmax=-66.533203, ymax=49.439557), spat_obj_type="MULTIPOLYGON")
# plot(sf::st_geometry(usa_region_mainland_sf))
usethis::use_data(usa_region_mainland_sf, overwrite = TRUE)
usethis::use_r("usa_region_mainland_sf")


# == USA county
usa_county_sf = sf::st_read(dsn="D:/spatial/usa_country", layer="cb_2018_us_county_20m")
lobstr::obj_size(usa_county_sf)
plot(sf::st_geometry(usa_county_sf))
lobstr::obj_size(usa_county_sf)
# class(usa_county_sf)
usa_county_sf = if(grepl("+init=epsg:4326", sf::st_crs(usa_county_sf)$input)) usa_county_sf else sf::st_transform(usa_county_sf, "+init=epsg:4326")
# unique(sf::st_geometry_type(usa_county_sf))
usethis::use_data(usa_county_sf, overwrite = TRUE)
usethis::use_r("usa_county_sf")


usa_county_mainland_sf <- prep_sf(usa_countrt_sf, lon="lon", lat="lat", crs=4326, bbox=c(xmin=-125.200195, ymin=23.523700, xmax=-66.533203, ymax=49.439557), spat_obj_type="MULTIPOLYGON")
plot(sf::st_geometry(usa_county_mainland_sf))
lobstr::obj_size(usa_county_mainland_sf)
usethis::use_data(usa_county_mainland_sf, overwrite = TRUE)
usethis::use_r("usa_county_mainland_sf")


# == Poland sf - gmina
# https://www.gis-support.pl/downloads/Gminy.zip?_ga=2.98575134.1242208255.1668900021-762670530.1668900021
poland_commune_sf_raw = sf::st_read(dsn="D:/spatial/Gminy", layer="Gminy")
poland_commune_sf = rmapshaper::ms_simplify(input = poland_commune_sf_raw,
                                   keep = 0.0085,
                                   method = NULL,
                                   weighting = 0.7,
                                   keep_shapes = FALSE,
                                   no_repair = FALSE,
                                   snap = TRUE,
                                   explode = FALSE,
                                   force_FC = TRUE,
                                   drop_null_geometries = TRUE,
                                   snap_interval = NULL,
                                   sys = FALSE,
                                   sys_mem = 8)
# rm(poland_commune_sf_raw)
plot(sf::st_geometry(poland_commune_sf))
lobstr::obj_size(poland_commune_sf)
# class(poland_commune_sf)
poland_commune_sf = poland_commune_sf[, c("JPT_KOD_JE", "JPT_NAZWA_", "WERSJA_OD", "WERSJA_DO")]
poland_commune_sf = if(grepl("+init=epsg:4326", sf::st_crs(poland_commune_sf)$input)) poland_commune_sf else sf::st_transform(poland_commune_sf, "+init=epsg:4326")
unique(sf::st_geometry_type(poland_commune_sf))
centroids = sf::st_centroid(poland_commune_sf)
# unique(sf::st_geometry_type(centroids))
centroids = data.table::rbindlist(lapply(1:nrow(centroids), function(row){
  row_data = centroids[row,]
  data.table::data.table(JPT_KOD_JE = row_data$JPT_KOD_JE,
                         centroid_lon = row_data$geometry[[1]][1],
                         centroid_lat = row_data$geometry[[1]][2])
}))
commune_types <- c("miejska", "wiejska", "miejsko-wiejska", "miejsko-wiejska: miasto", "miejsko-wiejska: obszar wiejski", "dzielnica Warszawy", "dzielnica/delegatura")
centroids[, commune_type := NA,]
for(n in c(1:5, 8:9)){
  centroids[, commune_type := ifelse(grepl(paste0(n, "$"), JPT_KOD_JE), commune_types[n], commune_type)]
}
unique(centroids$commune_type)
poland_commune_sf = merge(poland_commune_sf, centroids, by = "JPT_KOD_JE", all.x=T, all.y=T)
# saveRDS(gminy_sf, "D:/spatial/Gminy.RDS")
# gminy_sf = readRDS("D:/spatial/Gminy.RDS")
# data.table::setDT(poland_commune_sf)
# class(poland_commune_sf)
usethis::use_data(poland_commune_sf, overwrite = TRUE)
usethis::use_r("poland_commune_sf")


# == Poland country sf
poland_county_sf_raw = sf::st_read(dsn="D:/spatial/Powiaty", layer="Powiaty")
poland_county_sf = rmapshaper::ms_simplify(input = poland_county_sf_raw,
                                     keep = 0.005,
                                     method = NULL,
                                     weighting = 0.8,
                                     keep_shapes = FALSE,
                                     no_repair = FALSE,
                                     snap = TRUE,
                                     explode = FALSE,
                                     force_FC = TRUE,
                                     drop_null_geometries = TRUE,
                                     snap_interval = NULL,
                                     sys = FALSE,
                                     sys_mem = 8)
rm(poland_county_sf_raw)
plot(sf::st_geometry(poland_county_sf))
lobstr::obj_size(poland_county_sf)
# class(poland_county_sf)
poland_county_sf = poland_county_sf[, c("JPT_KOD_JE", "JPT_NAZWA_", "WERSJA_OD", "WERSJA_DO")]
poland_county_sf = if(grepl("+init=epsg:4326", sf::st_crs(poland_county_sf)$input)) poland_county_sf else sf::st_transform(poland_county_sf, "+init=epsg:4326")
centroids = sf::st_centroid(poland_county_sf)
centroids = data.table::rbindlist(lapply(1:nrow(centroids), function(row){
  row_data = centroids[row,]
  data.table::data.table(JPT_KOD_JE = row_data$JPT_KOD_JE,
                         centroid_lon = row_data$geometry[[1]][1],
                         centroid_lat = row_data$geometry[[1]][2])
}))
poland_county_sf = merge(poland_county_sf, centroids, by = "JPT_KOD_JE", all.x=T, all.y=T)
# saveRDS(poland_county_sf, "D:/spatial/Powiaty.RDS")
# poland_county_sf = readRDS("D:/spatial/Powiaty.RDS")
usethis::use_data(poland_county_sf, overwrite = TRUE)
usethis::use_r("poland_county_sf")


# == Poland voivodship sf
poland_voivodship_sf_raw = sf::st_read(dsn="D:/spatial/Wojewodztwa", layer="Wojewodztwa")
poland_voivodship_sf = rmapshaper::ms_simplify(input = poland_voivodship_sf_raw,
                                         keep = 0.01,
                                         method = NULL,
                                         weighting = 0.7,
                                         keep_shapes = FALSE,
                                         no_repair = FALSE,
                                         snap = TRUE,
                                         explode = FALSE,
                                         force_FC = TRUE,
                                         drop_null_geometries = TRUE,
                                         snap_interval = NULL,
                                         sys = FALSE,
                                         sys_mem = 8)
# plot(sf::st_geometry(poland_voivodship_sf))
# lobstr::obj_size(poland_voivodship_sf)
poland_voivodship_sf = poland_voivodship_sf[, c("JPT_KOD_JE", "JPT_NAZWA_", "WERSJA_OD", "WERSJA_DO")]
centroids = sf::st_centroid(poland_voivodship_sf)
centroids = data.table::rbindlist(lapply(1:nrow(centroids), function(row){
  row_data = centroids[row,]
  data.table::data.table(JPT_KOD_JE = row_data$JPT_KOD_JE,
                         centroid_lon = row_data$geometry[[1]][1],
                         centroid_lat = row_data$geometry[[1]][2])
}))
poland_voivodship_sf = merge(poland_voivodship_sf, centroids, by = "JPT_KOD_JE", all.x=T, all.y=T)
# saveRDS(poland_voivodship_sf, "D:/spatial/Wojewodztwa.RDS")
# poland_voivodship_sf = readRDS("D:/spatial/Wojewodztwa.RDS")
usethis::use_data(poland_voivodship_sf, overwrite = TRUE)
usethis::use_r("poland_voivodship_sf")
