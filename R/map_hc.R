#' Draw highchart base map (contours)
#'
#' @param map_obj spatial object of class geofeaturecollection (can be obtained with from sf object with prep_geojson)
#'
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
draw_hc_map <- function(map_obj){

  raise_if_not_geojson(map_obj)
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
#' @param points_obj list of spatial object of class geofeaturecollection, or single geofeaturecollection object, these denote points to be placed on the map (can be obtained with from sf object with prep_geojson)
#' @param points_names character vector containing names for each series to be drawn
#' @param hover_info named list, specify columns to show on tooltip hover, names must be column names, elements either empty string or text to show in front of the value
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
add_hc_mappoins <- function(hc_map, points_obj, points_names=NULL, hover_info=NULL){

  points_obj <- verify_points_object(points_obj, points_names)
  for(n in seq_along(points_obj)){
    hc_map <- hc_map %>%
      add_mappoint(points_obj[[n]], points_names[n])
  }
  if(!is.null(hover_info)){
    hc_map <- add_custom_hover(hc_map, hover_info)
  }
  return(hc_map)
}



#' Draw chart from geofeaturecollection geojsons (map geojson and point geojson(s))
#' @description Base map must be of geofeaturecollection class, points_obj can be either a single object of a list of objects of geofeaturecollection
#'
#' @param map_obj spatial object of class geofeaturecollection for base map (can be obtained with from sf object with prep_geojson)
#' @param points_obj list of spatial object of class geofeaturecollection, or single geofeaturecollection object, these denote points to be placed on the map (can be obtained with from sf object with prep_geojson)
#' @param points_names character vector containing names for each series to be drawn
#' @param hover_info named list, specify columns to show on tooltip hover, names must be column names, elements either empty string or text to show in front of the value
#'
#' @return object of class "highchart htmlwidget"
#' @export
#'
# @examples
draw_hc_full_map <- function(map_obj, points_obj, points_names=NULL, hover_info=NULL){

  points_obj <- verify_points_object(points_obj, points_names)
  hc_map <- draw_hc_map(map_obj)
  for(n in seq_along(points_obj)){
    hc_map <- hc_map %>%
      add_mappoint(points_obj[[n]], points_names[n])
  }
  if(!is.null(hover_info)){
    hc_map <- add_custom_hover(hc_map, hover_info)
  }
  return(hc_map)
}




#' Adds single mappoint to a higcharter map widglet
#'
#' @param hc_map "highchart htmlwidget" object, a base map (produced with draw_hc_map)
#' @param points_geojson spatial object of class geofeaturecollection (can be obtained with from sf object with prep_geojson)
#' @param series_name character of length 1, a name of the series to be drawn
#' @param hover_info named list, specify columns to show on tooltip hover, names must be column names, elements either empty string or text to show in front of the value
#'
#' @return highchart htmlwidget
#'
add_mappoint <- function(hc_map, points_geojson, series_name, hover_info){
  # browser()
  raise_if_not_geojson(points_geojson)
  hc_map <- hc_map %>%
    highcharter::hc_add_series(
      type='mappoint',
      dataLabels = list(enabled = FALSE),
      showInLegend = TRUE,
      data = points_geojson,
      geojson = TRUE
    )

  if(!is.null(series_name)){
    hc_map <- hc_map %>% highcharter::hc_plotOptions(series = list(name = series_name))
  }
  return(hc_map)
}


#' Transform hover_info list to hc accepter sting and add custom tooltip info
#'
#' @param hc_map "highchart htmlwidget" object, a base map (produced with draw_hc_map)
#' @param hover_info named list, specify columns to show on tooltip hover, names must be column names, elements either empty string or text to show in front of the value
#'
#' @return "highchart htmlwidget"
#'
add_custom_hover <- function(hc_map, hover_info){
    hover_info <- paste(purrr::map_chr(seq_along(hover_info),
                                       ~ ifelse(hover_info[[.x]]!="",
                                                paste(hover_info[[.x]], paste0("{point.properties.", names(hover_info)[.x], "}"), sep=": "),
                                                paste(hover_info[[.x]], paste0("{point.properties.", names(hover_info)[.x], "}"), sep=""))),
                        collapse = " <br> ")
    hc_map <- hc_map %>% highcharter::hc_plotOptions(series = list(tooltip = list(pointFormat = hover_info)))
  return(hc_map)
}



#' Check if object is geofeaturecollection, otherwise raise error
#'
#' @param object object the check is performed against
#'
raise_if_not_geojson <- function(object){
  if(!inherits(object, "geofeaturecollection")){
    stop("An object for plotting point is not geofeaturecollection class.")
  }
}



#' Verify if object only consists of geofeaturecollection, and return it if so
#'
#' @param points_obj object the check is performed against
#' @param points_names character vector containing names for each series to be drawn
#'
#' @return geofeaturecollection
#'
verify_points_object <- function(points_obj, points_names){
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
  # browser()
  if(!is.null(points_names)){
    if(length(points_names)!=length(points_obj)){
      stop("length of points_names and points_obj are different. Expected a name for each element of points_obj.")
    }
  }
  return(points_obj)
}
