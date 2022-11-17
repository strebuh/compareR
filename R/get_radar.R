

#' Draw plotly radar plot by categories
#' @description Draw plotly radar plot for given categories based on given numeric columns
#'
#' @param data data.frame or data.table object
#' @param filters list objects where column names are list elements and categories are corresponding values, data gets filtered using a list
#' @param id_var character, a column name in data that holds record identifier
#' @param grouper character, column name that holds categories for grouping data, if NULL no grouping
#' @param mapped_vars character, columns names' which hold measure variables to be displayed on radar
#' @param aggregation type of the aggregation do be displayed, one of avaliables in get_agr function (mean, median, sd); if NULL, no aggregation displayed
#'
#' @return plotly object
#' @export
#'
# @examples
get_radar <- function(data, filters, id_var, grouper, mapped_vars, aggregation){

  data.table::setDT(data)

  # Prepare data
  grouped_data <- data  %>% split(., .[, grouper, with = FALSE])
  grouped_data_a <- data %>%
    get_agr(grp_col=grouper, val_col=mapped_vars, agr_type=aggregation) %>%
    split(., .[, grouper, with = FALSE])
  filtered_grouped_data <- purrr::map(grouped_data, function(df) dplyr::filter(df, dplyr::across(dplyr::all_of(names(filters)), ~ .x %in% filters[[cur_column()]])))

  # Prepare scaled data
  data_s <- scale_data(data, mapped_vars)
  grouped_data_s <- data_s %>% split(., .[, grouper, with = FALSE])
  grouped_data_s_a <- data_s %>%
    get_agr(grp_col=grouper, val_col=mapped_vars, agr_type=aggregation) %>%
    split(., .[, grouper, with = FALSE])
  filtered_grouped_data_s <- purrr::map(grouped_data_s, function(df) dplyr::filter(df, dplyr::across(dplyr::all_of(names(filters)), ~ .x %in% filters[[cur_column()]])))

  fig <- plotly::plot_ly(
    type = 'scatterpolar',
    fill = 'toself'
  )

  # Agregated level for each group
  for(g in seq_along(grouped_data)){
    grp_agr <- grouped_data_s_a[[g]]
    grp <- grouped_data_a[[g]][, mapped_vars, with = FALSE]

    fig <- fig %>%
      plotly::add_trace(
        r = unlist(grp_agr[, mapped_vars, with = FALSE]),
        theta = mapped_vars,
        name = paste(grp_agr[, grouper, with = FALSE], grp_agr$agregat, sep = " - "),
        text = unname(unlist(grp)),
        hoverinfo = 'text'
      )
  }

  # Individuals records
  for(g in seq_along(filtered_grouped_data_s)){
    grp_agr <- filtered_grouped_data_s[[g]]
    grp <- grouped_data[[g]]
    for(row in seq_len(nrow(grp_agr)) ){
      record_s <- grp_agr[row,]
      record <- grp[row,][, mapped_vars, with = FALSE]
      fig <- fig %>%
        plotly::add_trace(
          r = unlist(record_s[, mapped_vars, with = FALSE]),
          theta = mapped_vars,
          name = paste(record_s[, id_var, with = FALSE], record_s[, grouper, with = FALSE], sep=", "),
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
