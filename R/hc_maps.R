

#' Draw highchart base map (contours)
#'
#' @param map_obj spatial object of class geofeaturecollection (can be obtained with from sf object with prep_geojson)
#'
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
draw_hc_map <- function(map_obj){
  map_obj <- highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series(mapData = map_obj,
                               type = "map",
                               showInLegend = FALSE
                               )
  return(map_obj)
}



#' Add points series(es) to highchart htmlwidget base contour map
#'
#' @param hc_map "highchart htmlwidget" object, a base map (produced with draw_hc_map)
#' @param points_obj list of spatial object of class geofeaturecollection, or signle geofeaturecollection object, these denote points to be placed on the map (can be obtained with from sf object with prep_geojson)
#'
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
add_hc_mappoins <- function(hc_map, points_obj){

  if(inherits(points_obj, "geofeaturecollection")){
    points_obj <- list(points_obj)
  } else if(!inherits(points_obj, "list")){
    stop("Neither geofeaturecollection/geojson nor list of such objects given.")
  }  else {
    gjson_check <- any(sapply(points_obj, function(gjson) !inherits(gjson, "geofeaturecollection")))
    if(gjson_check){
      stop("One or more elemetns of points is not of class 'geofeaturecollection/geojson'.")
    }
  }

  for(temp_points in points_obj){
    hc_map <- hc_map %>%
      highcharter::hc_add_series(
        type='mappoint',
        dataLabels = list(enabled = FALSE),
        showInLegend = TRUE,
        data = temp_points#,
        # geojson = TRUE
      )
  }
  return(hc_map)
}




#' Draw map with points from base map object and points object
#' @description Base map must be of geofeaturecollection class, points_obj can be either a signle object of a list of objects of geofeaturecollection
#'
#' @param map_obj spatial object of class geofeaturecollection for base map (can be obtained with from sf object with prep_geojson)
#' @param points_obj list of spatial object of class geofeaturecollection, or signle geofeaturecollection object, these denote points to be placed on the map (can be obtained with from sf object with prep_geojson)
#'
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
draw_hc_full_map <- function(map_obj, points_obj){

  if(inherits(points_obj, "geofeaturecollection")){
    points_obj <- list(points_obj)
  } else if(!inherits(points_obj, "list")){
    stop("Neither geofeaturecollection/geojson nor list of such objects given.")
  }  else {
    gjson_check <- any(sapply(points_obj, function(gjson) !inherits(gjson, "geofeaturecollection")))
    if(gjson_check){
      stop("One or more elemetns of points is not of class 'geofeaturecollection/geojson'.")
    }
  }
  map_obj <- highcharter::highchart(type = "map") %>%
    highcharter::hc_add_series(mapData = map_obj,
                               type = "map",
                               showInLegend = FALSE
                               )
  for(temp_points in points_obj){
    map_obj <- map_obj %>%
      highcharter::hc_add_series(
        type='mappoint',
        dataLabels = list(enabled = FALSE),
        showInLegend = TRUE,
        data = temp_points#,
        # geojson = TRUE
      )
  }
  return(map_obj)
}
