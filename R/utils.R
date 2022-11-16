


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
#' @examples
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
#' @examples
get_avg <- function(data, grp_col, val_col){
  data.table::setDT(data)
  new_gr <- grp_col[length(grp_col)]
  data <- data.table::copy(data) %>% .[, c(new_gr) := paste0(eval(as.symbol(new_gr)), " - mean")] %>% # add mean to last grouping variable
    .[, lapply(.SD, function(x) round(mean(x[!is.nan(x)], na.rm = T), 2)), by=grp_col, .SDcols=val_col] %>% # compute means per group for each variable
    .[, group := apply(.[, ..grp_col], 1, paste, collapse=", ")] %>%  # new , combined grouping variable
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
#'
#' @examples
get_agr <- function(data, grp_col, val_col, agr_type="mean"){
  if(arg_type %in% c("mean", "median", "sd")){
    stop("Unsuported agregation. Only mean, median and sd available.")
  }
  agreg <- get(agr_type)
  data <- data.table::copy(data) %>% .[, agregat := agr_type] %>% # add mean to last grouping variable
    .[, lapply(.SD, function(x) round(agreg(x[!is.nan(x)], na.rm = T), 2)), by=c(grp_col, "agregat"), .SDcols=val_col] # compute means per group for each variable
  data
}

