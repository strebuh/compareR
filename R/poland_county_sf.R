#' Poland country (powiat) sf data.frame class object
#'
#'
#' @format sf data.frame with 379 rows and 7 variables:
#' \describe{
#'   \item{JPT_KOD_JE}{National Official Register of the Territorial Division of the Country (TERYT) }
#'   \item{JPT_NAZWA_}{Commune name}
#'   \item{WERSJA_OD}{Valid from}
#'   \item{WERSJA_DO}{Valid to}
#'   \item{centroid_lon}{Centroid longitude}
#'   \item{centroid_lat}{Centroid latitude}
#'   \item{geometry}{}
#' }
#' @source \url{https://gis-support.pl/baza-wiedzy-2/dane-do-pobrania/granice-administracyjne/ simplified with rmapshaper::ms_simplify}
"poland_county_sf"
