#' event_timeseries
#'
#' @param var_in
#' @param storm_start
#' @param storm_end
#' @param view_start
#' @param view_end
#' @param recovery_start
#' @param recovery_end
#' @param stn_wq
#' @param keep_flags
#' @param data_path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
event_timeseries <- function(var_in,
                             data_path = NULL,
                             storm_start = NULL,
                             storm_end = NULL,
                             view_start = NULL,
                             view_end = NULL,
                             recovery_start = NULL,
                             recovery_end = NULL,
                             stn_wq = NULL,
                             keep_flags = NULL,
                             ...) {

  ### 0. Read variables ########################################################

  #a.  Read in the variable input template, var_in

  input_data <- xlsx::read.xlsx(var_in, sheetName = "Data")
  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "Parameters")
  input_Sites <- xlsx::read.xlsx(var_in, sheetName = "Sites")
  input_Flags <- xlsx::read.xlsx(var_in, sheetName = "Flags")

  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(is.null(storm_start)) storm_start <- input_Parameters[2,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[3,2]
  if(is.null(view_start)) view_start <- input_Parameters[4,2]
  if(is.null(view_end)) view_end <- input_Parameters[5,2]
  if(is.null(recovery_start)) recovery_start <- storm_end
  if(is.null(recovery_end)) recovery_end <- input_Parameters[6,2]
  if(is.null(reserve)) reserve <- input_Parameters[7,2]
  if(is.null(stn_wq)) stn_wq <- input_Parameters[9,2]
  if(is.null(keep_flags)) keep_flags <- input_Flags$keep_flags
  if(is.null(data_path)) data_path <- 'data/cdmo'


  ### 1. Read in data and wrangle ##############################################
  #data will be placed in the data folder within the Event Level Template
  dat_wq <- SWMPr::import_local(path = data_path, stn_wq)
  dat_wq <- SWMPr::qaqc(dat_wq, qaqc_keep = keep_flags)

  # tidy the data ------------------------------------
  dat_wq <- tidyr::pivot_longer(dat_wq, cols = 2:13
                         , names_to = 'parameter'
                         ,values_to = 'value')

  parms <- unique(dat_wq$parameter)


  ### Plot Data ################################################################

  df <- dat_wq %>%
    dplyr::filter(dplyr::between(datetimestamp, as.POSIXct(view_start), as.POSIXct(view_end)))

  parm_wq <- unique(dat_wq$parameter)

  # one station, daily smooth -------------------------------------------------

  #df_day <- dat_wq %>%
  #  dplyr::filter(between(datetimestamp
  #                 , as.POSIXct(view_start)
  #                 , as.POSIXct(view_end))) %>%
  #  dplyr::mutate(datetimestamp_day = floor_date(datetimestamp, unit = 'day')) %>%
  #  dplyr::group_by(datetimestamp_day, parameter) %>%
  #  dplyr::summarize(value = mean(value, na.rm = T))
  #
  #for(i in 1:length(parms)){
  #  parm = parms[i]
  #
  #  # Create a dummy data.frame for events and recovery
  #  df<-data.frame(xmin=as.POSIXct(storm_start),
  #                 xmax=as.POSIXct(storm_end),
  #                 ymin=c(-Inf),
  #                 ymax=c(Inf),
  #                 years=c('Event Duration'))
  #
  #  x <-
  #    df_day %>%
  #    dplyr::filter(parameter == parm) %>%
  #    ggplot2::ggplot(., aes(x = datetimestamp_day, y = value)) +
  #    ggplot2::geom_line(aes(color = 'Daily Avg')) +# 'steelblue3') +
  #    ggplot2::geom_rect(data=df,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
  #              alpha=0.2,inherit.aes=FALSE) +
  #    ggplot2::scale_x_datetime(date_breaks = '2 weeks', date_labels = '%b %d') +
  #    ggplot2::labs(x = '', y = SWMPrStorm::y_labeler(parm))
  #
  #  x <-
  #    x +
  #    ggplot2::scale_color_manual('', values = c('steelblue3')) +
  #    ggplot2::scale_fill_manual('', values = 'steelblue3')
  #
  #  x <- x +
  #    ggplot2::theme_bw() +
  #    ggplot2::theme(strip.background = element_blank(),
  #          panel.grid = element_blank(),
  #          panel.border = element_rect(color = 'black', fill = NA)) +
  #    ggplot2::theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
  #    ggplot2::theme(text = element_text(size = 16)) +
  #    ggplot2::theme(legend.position = 'top')
  #
  #  print(x)
  #
  #}

  # one station, daily smooth, with recovery -----------------------------------

  df_day <- dat_wq %>%
    dplyr::filter(dplyr::between(datetimestamp
                   , as.POSIXct(view_start)
                   , as.POSIXct(view_end))) %>%
    dplyr::mutate(datetimestamp_day = lubridate::floor_date(datetimestamp, unit = 'day')) %>%
    dplyr::group_by(datetimestamp_day, parameter) %>%
    dplyr::summarize(value = mean(value, na.rm = T))

  for(i in 1:length(parms)){
    parm = parms[i]

    # Create a dummy data.frame for events and recovery
    df<-data.frame(xmin=as.POSIXct(c(storm_start, recovery_start)),
                   xmax=as.POSIXct(c(storm_end, recovery_end)),
                   ymin=c(-Inf, -Inf),
                   ymax=c(Inf, Inf),
                   years=c('Event Duration', 'Event Recovery'))

    x <-
      df_day %>%
      dplyr::filter(parameter == parm) %>%
      ggplot2::ggplot(., ggplot2::aes(x = datetimestamp_day, y = value)) +
      ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_wq)) +
      ggplot2::geom_line(ggplot2::aes(color = 'Daily Avg'), lwd = 1) +
      ggplot2::geom_rect(data=df,ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                alpha=0.1,inherit.aes=FALSE) +
      ggplot2::scale_x_datetime(date_breaks = '2 weeks', date_labels = '%b %d', guide = guide_axis(check.overlap = TRUE)) +
      ggplot2::labs(x = '', y = SWMPrStorm::y_labeler(parm))


    x <-
      x +
      ggplot2::scale_color_manual('', values = c('steelblue3')) +
      ggplot2::scale_fill_manual('', values = c('steelblue3', 'green'))

    x <- x +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5),
                     strip.background = element_blank(),
                     panel.grid = element_blank(),
                     panel.border = element_rect(color = 'black', fill = NA),
                     axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                     text = element_text(size = 16),
                     legend.position = 'top')


    x_ttl <- paste('output/wq/timeseries_event_recovery/timeseries_event_recovery_', stn_wq, '_', parm, '.png', sep = '')

    ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

  }


}
