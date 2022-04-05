#' event_timeseries_dual
#'
#' @param var_in
#' @param data_path
#' @param view_start
#' @param view_end
#' @param stn_wq
#' @param stn_met
#' @param param_primary
#' @param param_secondary
#' @param keep_flags
#' @param ...
#' @param storm_nm
#' @param skip
#'
#' @return
#' @export
#'
#' @examples
event_timeseries_dual <- function(var_in,
                                  data_path = NULL,
                                  storm_nm = NULL,
                                  view_start = NULL,
                                  view_end = NULL,
                                  stn_wq = NULL,
                                  stn_met = NULL,
                                  param_primary = NULL,
                                  param_secondary = NULL,
                                  keep_flags = NULL,
                                  skip = NULL,
                                  ...) {

  ### 0. Read variables ########################################################

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "timeseries_dual")


  #b.  Read the following variables from template spreadsheet if not provided as optional arguments


  if(is.null(view_start)) view_start <- input_Parameters[1,2]
  if(is.null(view_end)) view_end <- input_Parameters[2,2]
  if(is.null(stn_wq)) stn_wq <- input_Parameters[3,2] #if both null, can ignore pending param_primary and param_secondary. build logic below.
  if(is.null(stn_met)) stn_met <- input_Parameters[4,2] #if both null, can ignore param_primary and param_secondary. build logic below.
  if(is.null(param_primary)) param_primary <- input_Parameters[5,2]
  if(is.null(param_secondary)) param_secondary <- input_Parameters[6,2]
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[7,2],", "))
  if(is.null(skip)) skip <- input_Parameters[8,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'


  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping event_timeseries_dual"))}



  ### 1. Load and Aggregate Datasets ###########################################


  ### Define parameter set
  params_of_interest <- c(param_primary, param_secondary)

  ### Load wq data



  if(!is.na(stn_wq)) {

    dat_wq <- SWMPr::import_local(path = data_path, stn_wq)
    dat_wq <- SWMPr::qaqc(dat_wq, qaqc_keep = keep_flags)

    # tidy the data ---------------------------------------------

    dat_wq <- tidyr::pivot_longer(dat_wq
                                 , !datetimestamp
                                 , names_to = 'parameter'
                                 , values_to = 'value')

    # filter for params of interest, selected viewport ----------

    dat_wq_filtered <- dat_wq %>%
      dplyr::filter(parameter %in% params_of_interest) %>%
      dplyr::filter(dplyr::between(datetimestamp
                                   , as.POSIXct(view_start)
                                   , as.POSIXct(view_end)))

  }

  ### Load met data ----->  NEED TO CONVERT UNITS

  if(!is.na(stn_met)) {

    dat_met <- SWMPr::import_local(path = data_path, stn_met)
    dat_met <- SWMPr::qaqc(dat_met, qaqc_keep = keep_flags)

    # tidy the data --------------------------------------------
    dat_met <- tidyr::pivot_longer(dat_met
                                  , !datetimestamp
                                  , names_to = 'parameter'
                                  , values_to = 'value')


    # filter for params of interes, selected viewport ----------

    dat_met_filtered <- dat_met %>%
      dplyr::filter(parameter %in% params_of_interest) %>%
      dplyr::filter(dplyr::between(datetimestamp
                                   , as.POSIXct(view_start)
                                   , as.POSIXct(view_end)))


  }

  ### Merge met and wq data (if applicable)

  dat_merged <- if(!exists("dat_met_filtered")) {
    dat_wq_filtered
  } else if(!exists("dat_wq_filtered")) {
    dat_met_filtered
  } else {
    rbind(dat_wq_filtered, dat_met_filtered)
  }

  dat_merged_wide <- dat_merged %>% tidyr::pivot_wider(names_from = parameter, values_from = value)

  dat_merged_wide <- dat_merged_wide %>%
    dplyr::rename(param_primary = paste(param_primary),
                  param_secondary = paste(param_secondary))


  ### Define axis scaling

  scales <- dat_merged %>%
    dplyr::mutate(axis = dplyr::case_when(parameter == param_primary ~ "primary",
                                   TRUE ~ "secondary")) %>%
    dplyr::group_by(axis) %>%
    dplyr::summarize(min = min(value, na.rm = TRUE),
              max = max(value, na.rm = TRUE),
              mean = mean(value, na.rm = TRUE))

  scaler <- scales$max[1]/scales$max[2]

  dat_merged <- dat_merged %>%
    dplyr::mutate(axis = dplyr::case_when(parameter == param_primary ~ "primary",
                                          TRUE ~ "secondary")) %>%
    dplyr::mutate(val_scaled = dplyr::case_when(axis == "primary" ~ value,
                                  TRUE ~ value*scaler))

  ### plot

  ttl <-   stn_ttl <- ifelse(!exists("dat_met_filtered"), stn_wq, stn_met)



  p1 <- ggplot2::ggplot(dat_merged, ggplot2::aes(x=datetimestamp, y=val_scaled)) +
    ggplot2::geom_line(ggplot2::aes(color = parameter)) +
    ggplot2::scale_color_manual(name = "", values = c("steelblue1", "steelblue4")) +
    ggplot2::scale_y_continuous(name = SWMPrStorm::y_labeler(param_primary)
                                , sec.axis = sec_axis(~./scaler, name = SWMPrStorm::y_labeler(param_secondary))) +
    ggplot2::scale_x_datetime(date_breaks = '2 days', date_labels = '%b %d', guide = guide_axis(check.overlap = TRUE)) +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = ttl)) +
    ggplot2::xlab("Datetime") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
                   strip.background = element_blank(),
                   panel.grid = element_blank(),
                   panel.border = element_rect(color = 'black', fill = NA),
                   axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                   axis.title.y.right = element_text(margin = unit(c(0,0,0,8), 'pt')),
                   text = element_text(size = 16),
                   legend.position = 'top')



  ### save plot

  stn_ttl <- if(!exists("dat_met_filtered")) {
    stn_wq
  } else if(!exists("dat_wq_filtered")) {
    stn_met
  } else {
    paste0(stn_wq, "_", stn_met)
  }


  plt_ttl <- paste0("output/combined/timeseries_dual_axis/", stn_ttl, "_", param_primary, "_", param_secondary, ".png")

  ggplot2::ggsave(filename = plt_ttl, plot = p1, height = 4, width = 6, units = 'in', dpi = 300)

}
