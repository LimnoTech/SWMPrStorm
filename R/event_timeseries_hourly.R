#' event_timeseries_hourly
#'
#' @param var_in
#'
#' @return
#' @export
#'
#' @examples
event_timeseries_hourly <- function(var_in){


  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------



  ### Read the following variables from template spreadsheet, 'var_in'
  #var_in will be defined in the analysis scripts within the Event Level Template

  storm_nm <- 'Matthew'
  storm_start <- '2016-09-28 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-09 23:45:00' # '2016-10-10 00:00:00'
  view_start <- '2016-09-01 00:00:00' # '2017-08-30 00:00:00'
  view_end <- '2016-12-01 00:00:00' # '2017-09-14 00:00:00'
  recovery_start <- storm_end
  recovery_end <- '2016-11-15 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'gtmfm', 'gtmpi', 'gtmss'), 'wq')
  met_sites <- 'gtmpcmet'
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')


  # ----------------------------------------------------------------------------
  # MET
  # ----------------------------------------------------------------------------

  ### load, clean, and filter data

  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  ## convert select parameters, add precip intensity (in/hr)
  ls_par <- lapply(ls_par, function(x) {x$atemp <- x$atemp * 9 / 5 + 32; x}) # C to F
  ls_par <- lapply(ls_par, function(x) {x$wspd <- x$wspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$maxwspd <- x$maxwspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$totprcp <- x$totprcp / 25.4; x}) # mm to in
  ls_par <- lapply(ls_par, function(x) {x$intensprcp <- x$totprcp * 4; x}) # in/15-min to in/hr

  names(ls_par) <- met_sites


  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  parm <- parm %>% subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  names(ls_par) <- met_sites

  ## identify parameters
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))


  for(i in 1:length(ls_par)) {

    # select a station data frame & determine name
    dat <- ls_par[[i]]
    sta <- names(ls_par)[i]

    # tidy
    dat <- tidyr::pivot_longer(dat, cols = 2:12
                        , names_to = 'parameter'
                        , values_to = 'value')

    df_day <- dat %>%
      dplyr::filter(between(datetimestamp
                     , as.POSIXct(view_start)
                     , as.POSIXct(view_end))) %>%
      dplyr::mutate(datetimestamp_day = floor_date(datetimestamp, unit = 'hour')) %>%
      dplyr::group_by(datetimestamp_day, parameter) %>%
      dplyr::summarize(value = mean(value, na.rm = T))

    for(j in 1:length(parm)) {

      # Create a dummy data.frame for events and recovery
      df <- data.frame(xmin=as.POSIXct(c(storm_start)),
                       xmax=as.POSIXct(c(storm_end)),
                       ymin=c(-Inf),
                       ymax=c(Inf),
                       years=c('Event Onset'))

      x <-
        df_day %>% dplyr::filter(parameter == parm[j]) %>%
        ggplot2::ggplot(., ggplot2::aes(x = datetimestamp_day, y = value)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = sta)) +
        ggplot2::geom_line(aes(color = 'Hourly Avg'), lwd = 1) +# 'steelblue3') +
        ggplot2::geom_rect(data=df,ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                  alpha=0.1,inherit.aes=FALSE) +
        ggplot2::scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
        ggplot2::labs(x = '', y = SWMPrExtension::y_labeler(parm[j]))

      x <-
        x +
        ggplot2::scale_color_manual('', values = c('steelblue3')) +
        ggplot2::scale_fill_manual('', values = c('steelblue3', 'green'))

      x <- x +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(color = 'black', fill = NA)) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
        ggplot2::theme(text = ggplot2::ggplot2::element_text(size = 16)) +
        ggplot2::theme(legend.position = 'top')

      x_ttl <- paste('output/met/timeseries_event_hourly/timeseries_event_hourly_', sta, '_', parm[j], '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

    }
  }


  # ----------------------------------------------
  # Time series, precip                      -----
  # ----------------------------------------------

  precip <- ls_par[[1]]

  precip_day <- precip %>%
    dplyr::filter(dplyr::between(datetimestamp
                   , as.POSIXct(view_start)
                   , as.POSIXct(view_end))) %>%
    dplyr::mutate(datetimestamp_day = lubridate::floor_date(datetimestamp, unit = 'day')) %>%
    dplyr::group_by(datetimestamp_day) %>%
    dplyr::summarize(value = sum(totprcp, na.rm = T))

  precip_day$mo <- paste(month.abb[month(precip_day$datetimestamp_day)]
                         , day(precip_day$datetimestamp_day)
                         , sep = ' ') %>%
    factor

  # precip_day$date_fac <- paste(as.factor(precip_day$datetimestamp_day), sep = ' ')

  # basic plot
  x <- ggplot2::ggplot(precip_day, ggplot2::aes(x = mo, y = value)) +
    ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(levels(precip_day$mo)))

  # colors
  x <- x +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5), breaks = seq(0 , 1.6, 0.25)) +
    ggplot2::labs(x = NULL, y = 'Daily Precipitation (in)')

  x <-
    x +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(strip.background = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(color = 'black', fill = NA)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    ggplot2::theme(text = ggplot2::element_text(size = 16)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt')) +
    ggplot2::theme(legend.position = 'top')

  x_ttl <- paste('output/met/barplot/barplot_daily_', sta, '_', parm[j], '.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)



  # ----------------------------------------------------------------------------
  # WQ
  # ----------------------------------------------------------------------------


  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(view_start, view_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  ## convert select parameters
  ls_par <- lapply(ls_par, function(x) {x$temp <- x$temp * 9 / 5 + 32; x})
  ls_par <- lapply(ls_par, function(x) {x$depth <- x$depth * 3.28; x})

  names(ls_par) <- wq_sites

  ## identify parameters
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))

  # ----------------------------------------------
  # Time series, hourly smooth, event window -----
  # ----------------------------------------------

  for(i in 1:length(ls_par)) {

    # select a station data frame & determine name
    dat <- ls_par[[i]]
    sta <- names(ls_par)[i]

    # tidy
    dat <- tidyr::pivot_longer(dat, cols = 2:13
                        , names_to = 'parameter'
                        , values_to = 'value')

    df_day <- dat %>%
      dplyr::filter(dplyr::between(datetimestamp
                     , as.POSIXct(view_start)
                     , as.POSIXct(view_end))) %>%
      dplyr::mutate(datetimestamp_day = lubridate::floor_date(datetimestamp, unit = 'hour')) %>%
      dplyr::group_by(datetimestamp_day, parameter) %>%
      summarize(value = mean(value, na.rm = T))

    for(j in 1:length(parm)) {

      # Create a dummy data.frame for events and recovery
      df <- data.frame(xmin=as.POSIXct(c(storm_start)),
                       xmax=as.POSIXct(c(storm_end)),
                       ymin=c(-Inf),
                       ymax=c(Inf),
                       years=c('Event Onset'))

      x <-
        df_day %>%
        dplyr::filter(parameter == parm[j]) %>%
        ggplot2::ggplot(., ggplot2::aes(x = datetimestamp_day, y = value)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = sta)) +
        ggplot2::geom_line(ggplot2::aes(color = 'Hourly Avg'), lwd = 1) +
        ggplot2::geom_rect(data=df,ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                  alpha=0.1,inherit.aes=FALSE) +
        ggplot2::scale_x_datetime(date_breaks = '1 week', date_labels = '%b %d') +
        ggplot2::labs(x = '', y = SWMPrExtension::y_labeler(parm[j]))

      x <-
        x +
        ggplot2::scale_color_manual('', values = c('steelblue3')) +
        ggplot2::scale_fill_manual('', values = c('steelblue3', 'green'))

      x <- x +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
        ggplot2::theme(strip.background = ggplot2::element_blank(),
              panel.grid = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(color = 'black', fill = NA)) +
        ggplot2::theme(plot.margin = ggplot2::margin(5.5, 10, 5.5, 5.5, unit = 'pt')) +
        ggplot2::theme(axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) +
        ggplot2::theme(legend.position = 'top')

      x_ttl <- paste('output/wq/timeseries_event/timeseries_event_hourly_', sta, '_', parm[j], '_', storm_nm, '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

    }
  }


}
