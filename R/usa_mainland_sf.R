#' US states sf data.frame class object restricted by bbox
#' @description US states sf data.frame class object restricted to: min lon-125.200195 degrees, min lat 23.523700 degrees, max lon -66.533203 degrees, max lat 49.439557 degrees, without Alaska, Hawaii and Puerto Rico.
#'
#'
#' @format sf data.frame with 49 rows and 7 variables:
#' \describe{
#'   \item{geoid}{}
#'   \item{state_name}{}
#'   \item{state_abbr}{}
#'   \item{jurisdiction_type}{}
#'   \item{aland}{}
#'   \item{awater}{}
#'   \item{geometry}{}
#' }
#' @source \url{USAboundaries::states_contemporary_lores}
"usa_mainland_sf"
