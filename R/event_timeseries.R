#' evet_timeseries
#'
#' @param var_in
#'
#' @return
#' @export
#'
#' @examples
event_timeseries <- function(var_in,
                             storm_nm = NULL,
                             storm_start = NULL,
                             storm_end = NULL,
                             view_start = NULL,
                             view_end = NULL,
                             recovery_start = NULL,
                             recovery_end = NULL,
                             reserve = NULL,
                             stn_wq = NULL,
                             stn_met = NULL,
                             keep_flags = NULL,
                             ...) {

  ### 0. Read variables ########################################################

  #a.  Read in the variable input template, var_in

  inputs <- xlsx::read.xlsx()
  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(!is.null(storm_nm)) storm_nm <- ###### finish with proper location in excel file, var_in
  if(!is.null(storm_start)) storm_start <-
  if(!is.null(storm_end)) storm_end <-
  if(!is.null(view_start)) view_start <-
  if(!is.null(view_end)) view_end <-
  if(!is.null(recovery_start)) recovery_start <-
  if(!is.null(recovery_end)) recovery_end <-
  if(!is.null(reserve)) reserve <-
  if(!is.null(stn_wq)) stn_wq <-
  if(!is.null(stn_met)) stn_met <-
  if(!is.null(keep_flags)) keep_flags <-

  ### 1. Read in data and wrangle ##############################################
  #data will be placed in the data folder within the Event Level Template
  dat_wq <- SWMPr::import_local(path = 'data/cdmo', stn_wq)
  dat_wq <- SWMPr::qaqc(dat_wq, qaqc_keep = keep_flags)

  # tidy the data ------------------------------------
  dat_wq <- pivot_longer(dat_wq, cols = 2:13
                         , names_to = 'parameter'
                         ,values_to = 'value')

  parms <- unique(dat_wq$parameter)


  ### Plot Data ################################################################

  df <- dat_wq %>%
    filter(between(datetimestamp, as.POSIXct(view_start), as.POSIXct(view_end)))

  parm_wq <- unique(dat_wq$parameter)

  # one station, daily smooth -------------------------------------------------

  #df_day <- dat_wq %>%
  #  filter(between(datetimestamp
  #                 , as.POSIXct(view_start)
  #                 , as.POSIXct(view_end))) %>%
  #  mutate(datetimestamp_day = floor_date(datetimestamp, unit = 'day')) %>%
  #  group_by(datetimestamp_day, parameter) %>%
  #  summarize(value = mean(value, na.rm = T))
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
  #    df_day %>% filter(parameter == parm) %>%
  #    ggplot(., aes(x = datetimestamp_day, y = value)) +
  #    geom_line(aes(color = 'Daily Avg')) +# 'steelblue3') +
  #    geom_rect(data=df,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
  #              alpha=0.2,inherit.aes=FALSE) +
  #    scale_x_datetime(date_breaks = '2 weeks', date_labels = '%b %d') +
  #    labs(x = '', y = SWMPrExtension::y_labeler(parm))
  #
  #  x <-
  #    x +
  #    scale_color_manual('', values = c('steelblue3')) +
  #    scale_fill_manual('', values = 'steelblue3')
  #
  #  x <- x +
  #    theme_bw() +
  #    theme(strip.background = element_blank(),
  #          panel.grid = element_blank(),
  #          panel.border = element_rect(color = 'black', fill = NA)) +
  #    theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
  #    theme(text = element_text(size = 16)) +
  #    theme(legend.position = 'top')
  #
  #  print(x)
  #
  #}

  # one station, daily smooth, with recovery -----------------------------------

  df_day <- dat_wq %>%
    filter(between(datetimestamp
                   , as.POSIXct(view_start)
                   , as.POSIXct(view_end))) %>%
    mutate(datetimestamp_day = floor_date(datetimestamp, unit = 'day')) %>%
    group_by(datetimestamp_day, parameter) %>%
    summarize(value = mean(value, na.rm = T))

  for(i in 1:length(parms)){
    parm = parms[i]

    # Create a dummy data.frame for events and recovery
    df<-data.frame(xmin=as.POSIXct(c(storm_start, recovery_start)),
                   xmax=as.POSIXct(c(storm_end, recovery_end)),
                   ymin=c(-Inf, -Inf),
                   ymax=c(Inf, Inf),
                   years=c('Event Duration', 'Event Recovery'))

    x <-
      df_day %>% filter(parameter == parm) %>%
      ggplot(., aes(x = datetimestamp_day, y = value)) +
      ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_wq)) +
      geom_line(aes(color = 'Daily Avg'), lwd = 1) +# 'steelblue3') +
      geom_rect(data=df,aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                alpha=0.1,inherit.aes=FALSE) +
      scale_x_datetime(date_breaks = '2 weeks', date_labels = '%b %d') +
      labs(x = '', y = SWMPrExtension::y_labeler(parm))

    x <-
      x +
      scale_color_manual('', values = c('steelblue3')) +
      scale_fill_manual('', values = c('steelblue3', 'green'))

    x <- x +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(strip.background = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_rect(color = 'black', fill = NA)) +
      theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
      theme(text = element_text(size = 16)) +
      theme(legend.position = 'top')


    x_ttl <- paste('output/wq/timeseries_event_recovery/timeseries_event_recovery_', stn_wq, '_', parm, '.png', sep = '')

    ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

  }


}
