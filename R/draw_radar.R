

#' Draw plotly radar plot by categories
#' @description Draw plotly radar plot for given categories based on given numeric columns
#'
#' @param data data.frame or data.table object
#' @param filters list objects, it's elements are categorical column names, list values are corresponding values; list for restraining data, so plot show only selected records, not all records in dataset
#' @param id_var character, a column name in data that holds record identifier
#' @param grouper character, column name that holds categories that defines groups by which individual records can be compared, if NULL no grouping
#' @param mapped_vars character, columns names' which hold measure variables to be displayed on radar
#' @param agreg_type type of the aggregation do be displayed, one of available in get_agr function (mean, median, sd); if NULL, no aggregation displayed
#'
#' @return plotly object
#' @export
#'
# @examples
draw_radar <- function(data, id_var, grouper, mapped_vars, agreg_type, filters=NULL){

  data.table::setDT(data)
  n_id_records <- data.table::uniqueN(data[[id_var]])
  if(n_id_records != nrow(data)){
    stop("Number of rows in data is higher than number of unique id's, which means that data has mutliple records with the same id.")
  }

  # Prepare data for hovers
  grouped_data <- data  %>% split(., .[, grouper, with = FALSE]) # split data into subsets by grouper variable(s)
  grouped_data_a <- data %>%
    get_agr(grp_col=grouper, val_col=mapped_vars, agr_type=agreg_type) %>%
    split(., .[, grouper, with = FALSE]) # aggregated by groups and split into subsets, used for plot hovers
  # filtered_grouped_data <- purrr::map(grouped_data, function(df) dplyr::filter(df, dplyr::across(dplyr::all_of(names(filters)), ~ .x %in% filters[[cur_column()]])))
  if(grouper %in% names(filters)){
    grouped_data <- grouped_data[filters[[grouper]]]
    grouped_data_a <- grouped_data_a[filters[[grouper]]]
  }


  # Prepare scaled data for drawing radar
  data_s <- scale_data(data, mapped_vars) # scaled data
  grouped_data_s <- data_s %>% split(., .[, grouper, with = FALSE])
  grouped_data_s_a <- data_s %>%
    get_agr(grp_col=grouper, val_col=mapped_vars, agr_type=agreg_type) %>%
    split(., .[, grouper, with = FALSE])

  # Filter only
  if(grouper %in% names(filters)){
    grouped_data_s <- grouped_data_s[filters[[grouper]]]
    grouped_data_s_a <- grouped_data_s_a[filters[[grouper]]]
  }

  # Apply filters to dataset
  if(!is.null(filters)){
    grouped_data_s <-  purrr::map(grouped_data_s, function(df) dplyr::filter(df, dplyr::across(dplyr::all_of(names(filters)), ~ .x %in% filters[[dplyr::cur_column()]])))
  } else if(n_id_records>5){
    warning(paste0("No filters applied to restrict data, while total number of records is ", n_id_records, ". Without applying filters the plot might be unreadable."))
  }

  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  )

  # aggregated level for each group
  for(g in seq_along(grouped_data)){
    grp_agr <- grouped_data_s_a[[g]] # sub df of scaled data of a given group
    grp <- grouped_data_a[[g]][, mapped_vars, with = FALSE]

    fig <- fig %>%
      plotly::add_trace(
        r = unlist(grp_agr[, mapped_vars, with = FALSE]),
        theta = mapped_vars,
        name = paste(unlist(grp_agr[, grouper, with = FALSE]), grp_agr$agregat, sep = " - "),
        text = unname(unlist(grp)),
        hoverinfo = 'text'
      )
  }

  # Individuals records
  for(g in seq_along(grouped_data_s)){
    grp_agr <- grouped_data_s[[g]]
    grp <- grouped_data[[g]]
    for(row in seq_len(nrow(grp_agr)) ){
      record_s <- grp_agr[row,]
      record <- grp[row,][, mapped_vars, with = FALSE]
      fig <- fig %>%
        plotly::add_trace(
          r = unlist(record_s[, mapped_vars, with = FALSE]),
          theta = mapped_vars,
          name = paste(unlist(record_s[, id_var, with = FALSE]), unlist(record_s[, grouper, with = FALSE]), sep=", "),
          text = unname(unlist(record)),
          hoverinfo = 'text'
        )
    }
  }

  fig <- fig %>%
    plotly::layout(
      polar = list(
        radialaxis = list(
          visible = F
        )
      )
    )

  return(fig)
}
