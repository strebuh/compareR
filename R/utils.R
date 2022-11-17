

#' Scale data to be within given range
#' @description Agregates data by given groups to averages
#'
#' @param data data.frame or data.table object
#' @param num_col character vector of columns names that hold values to be scaled
#' @param min min of a new range
#' @param max max of a new range
#'
#' @return data.table
#' @export
#'
# @examples
scale_data <- function(data, num_col, min=0, max=10){
  data_s <- data.table::copy(data) %>% .[, c(num_col):=lapply(.SD, function(x) round(scales::rescale(x, to=c(min, max)), 2)), .SDcols=num_col]
  return(data_s)
}


#' Get averages by groups
#' @description Aggregates data by given groups to averages
#'
#' @param data data.frame or data.table object
#' @param grp_col character vector of column names that specify the groups
#' @param val_col character vector of columns names that hold values to be aggregated
#'
#' @return data.table
#' @export
#'
# @examples
get_avg <- function(data, grp_col, val_col){
  group <- NULL # no visible binding for global variable 'group'
  data.table::setDT(data)
  new_gr <- grp_col[length(grp_col)]
  data <- data.table::copy(data) %>% .[, c(new_gr) := paste0(eval(as.symbol(new_gr)), " - mean")] %>% # add mean to last grouping variable
    .[, lapply(.SD, function(x) round(mean(x[!is.nan(x)], na.rm = T), 2)), by=grp_col, .SDcols=val_col] %>% # compute means per group for each variable
    .[, group := apply(.[, grp_col, with = FALSE], 1, paste, collapse=", ")] %>%  # new , combined grouping variable
    .[, c(grp_col) := lapply(grp_col, function(x) NULL)] # removeing separate grouping variables
  data
}


#' Get mean, median or sd within groups
#' @description Aggregates data by given groups to mean, median or sd
#'
#' @param data data.frame or data.table object
#' @param grp_col character vector of column names that specify the groups
#' @param val_col character vector of columns names that hold values to be agregated
#' @param agr_type type of aggregation of mean, median, sd
#'
#' @return data.table
#' @export
# @examples
get_agr <- function(data, grp_col, val_col, agr_type="mean"){
  # agregat <- NULL # no visible binding for global variable 'agregat'
  if(agr_type %in% c("mean", "median", "sd")){
    stop("Unsuported agregation. Only mean, median and sd available.")
  }
  data.table::setDT(data)
  agreg <- get(agr_type)
  data <- data.table::copy(data) %>% .[, agregat := agr_type] %>% # add mean to last grouping variable
    .[, lapply(.SD, function(x) round(agreg(x[!is.nan(x)], na.rm = T), 2)), by=c(grp_col, "agregat"), .SDcols=val_col] # compute means per group for each variable
  data
}


#' Get matrix of radius distances
#' @description Compute matrix of radius distances between points in two data.frames/data.table
#'
#' @param data1 data.frame or data.table containing lon and lat columns
#' @param lonlat1 character vector of column names in data1 holding coordinates (longittude and lattitude)
#' @param data2 optional data.frame or data.table containing lon and lat columns, if not given square matrix between points in data1 is computed
#' @param lonlat2 character vector of column names in data2 holding coordinates (longittude and lattitude)
#'
#' @return data.table
#' @export
#'
# @examples
get_rad_dist_matrix <- function(data1, lonlat1, data2=NULL, lonlat2=NULL){
  data.table::setDT(data1)
  data.table::setDT(data2)
  if(is.null(data2)){
    dist_matrix = geodist::geodist(data1[, lonlat1, with = FALSE], measure="geodesic")
  } else {
    dist_matrix = geodist::geodist(data1[, lonlat1, with = FALSE], data2[, lonlat2, with = FALSE], measure="geodesic")
  }

  dist_matrix = as.data.frame(dist_matrix)
  data.table::setDT(dist_matrix)
  return(dist_matrix)
}


#' Count number of points within radius distance
#' @description Count number of points within radius distance of given number of kilometers
#'
#' @param radius radius km distance
#' @param data1 data.frame or data.table containing lon and lat columns
#' @param lonlat1 character vector of column names in data1 holding coordinates (longittude and lattitude)
#' @param data2 optional data.frame or data.table containing lon and lat columns, if not given square matrix between points in data1 is computed
#' @param lonlat2 character vector of column names in data2 holding coordinates (longittude and lattitude)
#'
#' @return data.table
#' @export
#'
# @examples
count_in_rad <- function(radius, data1, lonlat1, data2=NULL, lonlat2=NULL){
  dist_matrix <- get_rad_dist_matrix(data1, lonlat1, data2, lonlat2)
  res <- rowSums(dist_matrix[, lapply(.SD, function(x) x<radius*1000), .SDcols=names(dist_matrix)])
  return(res)
}


#' Find records within radius distance
#'
#' @param radius radius distance in km
#' @param data1 data.frame or data.table containing lon and lat columns
#' @param id1 character of column name holding identifiers in data1
#' @param lonlat1 character vector of column names in data1 holding coordinates (longittude and lattitude)
#' @param data2 optional data.frame or data.table containing lon and lat columns, if not given square matrix between points in data1 is computed
#' @param id2 character of column name holding identifiers in data2
#' @param lonlat2 character vector of column names in data2 holding coordinates (longittude and lattitude)
#'
#' @return data.table
#' @export
#'
# @examples
get_n_in_rad <- function(radius, data1, id1, lonlat1, data2=NULL, id2=NULL, lonlat2=NULL){
  dist_matrix <- get_rad_dist_matrix(data1, lonlat1, data2, lonlat2)
  if(is.null(data2)) diag(dist_matrix)=NA
  dist_matrix <- as.data.frame(t(dist_matrix))

  id2 <- if(is.null(id2)) id1 else id2
  data2 <- if(is.null(data2)) data1 else data2 # no need to transform to data.table?
  res <- lapply(dist_matrix, function(x) {
    within <- which(x < radius*1000)
    temp <- cbind(data2[within,], data.frame(distance=round(x[within]/1000, 2)))
    return(temp)
  })
  names(res) <- data1[[id1]]
  return(res)
}


#' Find records given number of closests records from given one
#'
#' @param n number of closest locations to be returned
#' @param data1 data.frame or data.table containing lon and lat columns
#' @param id1 character of column name holding identifiers in data1
#' @param lonlat1 character vector of column names in data1 holding coordinates (longittude and lattitude)
#' @param data2 optional data.frame or data.table containing lon and lat columns, if not given square matrix between points in data1 is computed
#' @param id2 character of column name holding identifiers in data2
#' @param lonlat2 character vector of column names in data2 holding coordinates (longittude and lattitude)
#'
#' @return data.table
#' @export
#'
# @examples
get_n_closest<- function(n, data1, id1, lonlat1, data2=NULL, id2=NULL, lonlat2=NULL){
  dist_matrix <- get_rad_dist_matrix(data1, lonlat1, data2, lonlat2)
  if(is.null(data2)) diag(dist_matrix)=NA
  dist_matrix <- as.data.frame(t(dist_matrix))
  id2 <- if(is.null(id2)) id1 else id2
  data2 <- if(is.null(data2)) data1 else data2
  res <- lapply(dist_matrix, function(x) {
    treshhold <- sort(x)[n]
    within <- which(x <= treshhold)
    temp <- cbind(data2[within,], data.frame(distance=round(x[within]/1000, 2)))
    data.table::setorderv(temp, "distance")
    return(temp)
  })
  names(res) <- data1[[id1]]
  return(res)
}


#' Crop and cast sf data.frame object
#'
#' @param sf_obj sf data.frame object
#' @param bbox list containing: xmin, ymin, xmax, ymax that denote extent of cropping area
#' @param spat_obj_type character, name of type of each geometry in sf data.frame (only needed if cropping causes ambiguity)
#'
#' @return sf object
#' @export
#'
# @examples
crop_n_cast <- function(sf_obj, bbox, spat_obj_type=NULL){
  sf_obj <- tryCatch({sf::st_crop(sf_obj, bbox)},
                     warning=function(w){
                       if(grepl("attribute variables are assumed to be spatially constant throughout all geometries", w$message) & !is.null(spat_obj_type)){
                         return(suppressWarnings(sf::st_cast(sf::st_crop(sf_obj, bbox), spat_obj_type)))
                       } else {
                         return(NULL)
                       }
                     }
  )
  if(is.null(sf_obj)){
    stop("Attribute variables are not spatially constant throughout all geometries, provide type they are to be cast to as 'spat_obj_type'")
  }
  return(sf_obj)
}


#' Transform data.frame or data.table to sf object
#'
#' @param data data.frame or data.table to be transformed to sf object
#' @param lon character, longitude column name
#' @param lat character, latitude column name
#' @param crs EPSG CRS code of Coordinate Reference Systems
#' @param bbox list containing: xmin, ymin, xmax, ymax that denote extent of cropping area
#' @param spat_obj_type haracter, name of type of each geometry in sf data.frame (only needed if cropping causes ambiguity)
#'
#' @return sf object
#' @export
#'
# @examples
prep_sf <- function(data, lon, lat, crs, bbox=NULL, spat_obj_type=NULL){
  if(inherits(data, "sf")){
    message("Data is already of 'sf' class.")
    coords_sf <- data
  } else {
    coords_sf <- sf::st_as_sf(data, coords = c(lon, lat))
  }
  coords_sf <- sf::st_set_crs(coords_sf, crs)
  if(!is.null(bbox)){
    coords_sf <- crop_n_cast(coords_sf, bbox, spat_obj_type)
  }
  return(coords_sf)
}


#' Transform sf of data.frame to geojson of type FeatureCollection
#' @description Transform sf of data.frame to geojson of type FeatureCollection used for drawing highcharter map.
#'
#' @param data data.frame, data.table or sf frame containing data
#' @param lon character, longitude column name
#' @param lat character, latitude column name
#' @param crs EPSG CRS code of Coordinate Reference Systems
#' @param bbox list containing: xmin, ymin, xmax, ymax that denote extent of cropping area
#' @param spat_obj_type haracter, name of type of each geometry in sf data.frame (only needed if cropping causes ambiguity)
#'
#' @return FeatureCollection object
#' @export
#'
# @examples
prep_geojson <- function(data, lon, lat, crs, bbox=NULL, spat_obj_type=NULL){
  if(!inherits(data, "sf")){
    coords_sf <- prep_sf(data, lon, lat, crs, bbox, spat_obj_type)
  } else {
    coords_sf <- data
  }
  if(!is.null(bbox) & !is.null(spat_obj_type)){
    coords_sf <- crop_n_cast(coords_sf, bbox, spat_obj_type)
  }
  mappoint_geojson <-  geojsonio::geojson_json(coords_sf, lat = lat, lon = lon)
  return(mappoint_geojson)
}


