#' US regions sf data.frame class object restricted by bbox
#' @description US regions sf data.frame class object restricted to: min lon-125.200195 degrees, min lat 23.523700 degrees, max lon -66.533203 degrees, max lat 49.439557 degrees, without Alaska, Hawaii and Puerto Rico.
#'
#'
#' @format sf data.frame with 4 rows and 7 variables:
#' \describe{
#'   \item{REGIONCE}{}
#'   \item{AFFGEOID}{}
#'   \item{GEOID}{}
#'   \item{NAME}{}
#'   \item{LSAD}{}
#'   \item{ALAND}{}
#'   \item{AWATER}{}
#'   \item{geometry}{}
#' }
#' @source \url{https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html}
"usa_region_mainland_sf"
