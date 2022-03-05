#' event_timeseries_hourly
#'
#' @param var_in
#' @param data_path
#' @param storm_nm
#' @param onset_start
#' @param onset_end
#' @param view_start
#' @param view_end
#' @param recovery_start
#' @param recovery_end
#' @param wq_sites
#' @param met_sites
#' @param keep_flags
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
event_timeseries_hourly <- function(var_in,
                                    data_path = NULL,
                                    storm_nm = NULL,
                                    onset_start = NULL,
                                    onset_end = NULL,
                                    view_start = NULL,
                                    view_end = NULL,
                                    recovery_start = NULL,
                                    recovery_end = NULL,
                                    wq_sites = NULL,
                                    met_sites = NULL,
                                    keep_flags = NULL,
                                    ...){


  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "timeseries_hourly")


  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(is.null(storm_nm)) storm_nm <- input_Parameters[1,2]
  if(is.null(onset_start)) onset_start <- input_Parameters[2,2]
  if(is.null(onset_end)) onset_end <- input_Parameters[3,2]
  if(is.null(view_start)) view_start <- input_Parameters[4,2]
  if(is.null(view_end)) view_end <- input_Parameters[5,2]
  if(is.null(recovery_start)) recovery_start <- input_Parameters[6,2]
  if(is.null(recovery_end)) recovery_end <- input_Parameters[7,2]
  if(is.null(wq_sites)) wq_sites <- unlist(strsplit(input_Parameters[8,2],", "))
  if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[9,2],", "))
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[10,2],", "))
  if(is.null(data_path)) data_path <- 'data/cdmo'



  # ----------------------------------------------------------------------------
  # MET
  # ----------------------------------------------------------------------------

  ### load, clean, and filter data

  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(view_start, view_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  ## convert select parameters, add precip intensity (in/hr)
  ls_par <- lapply(ls_par, function(x) {x$atemp <- x$atemp * 9 / 5 + 32; x}) # C to F
  ls_par <- lapply(ls_par, function(x) {x$wspd <- x$wspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$maxwspd <- x$maxwspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$totprcp <- x$totprcp / 25.4; x}) # mm to in
  ls_par <- lapply(ls_par, function(x) {x$intensprcp <- x$totprcp * 4; x}) # in/15-min to in/hr

  ## list unit conversions for plot labels
  param <- c('atemp', 'wspd', 'maxwspd', 'totprcp', 'intensprcp')
  unit <- rep(TRUE, length(param))
  conversions <- data.frame(parameter = param, con = unit)

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
      dplyr::filter(dplyr::between(datetimestamp
                     , as.POSIXct(view_start)
                     , as.POSIXct(view_end))) %>%
      dplyr::mutate(datetimestamp_day = lubridate::floor_date(datetimestamp, unit = 'hour')) %>%
      dplyr::group_by(datetimestamp_day, parameter) %>%
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
      dplyr::left_join(conversions) %>%
      dplyr::mutate(con = tidyr::replace_na(con, FALSE))

    for(j in 1:length(parm)) {

      # Create a dummy data.frame for events and recovery
      df <- data.frame(xmin=as.POSIXct(c(onset_start)),
                       xmax=as.POSIXct(c(onset_end)),
                       ymin=c(-Inf),
                       ymax=c(Inf),
                       years=c('Event Onset'))

      converted <- df_day %>% filter(parameter == parm[j])
      converted <- converted$con[1]


      x <-
        df_day %>% dplyr::filter(parameter == parm[j]) %>%
        ggplot2::ggplot(., ggplot2::aes(x = datetimestamp_day, y = value)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = sta)) +
        ggplot2::geom_line(aes(color = 'Hourly Avg'), lwd = 1) +# 'steelblue3') +
        ggplot2::geom_rect(data=df,ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                  alpha=0.1,inherit.aes=FALSE) +
        ggplot2::labs(x = '', y = SWMPrStorm::y_labeler(parm[j], converted=converted))

      x <-
        x +
        ggplot2::scale_color_manual('', values = c('steelblue3')) +
        ggplot2::scale_fill_manual('', values = c('steelblue3', 'green')) +
        ggplot2::scale_x_datetime(date_breaks = 'day', date_labels = '%b\n%d', guide = guide_axis(check.overlap = TRUE)) +


      x <- x +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 8), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

      x_ttl <- paste('output/met/timeseries_event_hourly/timeseries_event_hourly_', sta, '_', parm[j], '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

    }
  }



  # ----------------------------------------------------------------------------
  # WQ
  # ----------------------------------------------------------------------------


  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(view_start, view_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  ## convert select parameters
  ls_par <- lapply(ls_par, function(x) {x$temp <- x$temp * 9 / 5 + 32; x})
  ls_par <- lapply(ls_par, function(x) {x$depth <- x$depth * 3.28; x})

  ## list unit conversions for plot labels
  param <- c('temp', 'depth')
  unit <- rep(TRUE, length(param))
  conversions <- data.frame(parameter = param, con = unit)

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
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
      dplyr::left_join(conversions) %>%
      dplyr::mutate(con = tidyr::replace_na(con, FALSE))

    for(j in 1:length(parm)) {

      # Create a dummy data.frame for events and recovery
      df <- data.frame(xmin=as.POSIXct(c(onset_start)),
                       xmax=as.POSIXct(c(onset_end)),
                       ymin=c(-Inf),
                       ymax=c(Inf),
                       years=c('Event Onset'))

      converted <- df_day %>% filter(parameter == parm[j])
      converted <- converted$con[1]


      #breaks <- df_day %>%
      #  dplyr::filter(parameter == parm[j]) %>%
      #  dplyr::ungroup() %>%
      #  dplyr::select(datetimestamp_day) %>%
      #  dplyr::summarize(min = min(datetimestamp_day), max = max(datetimestamp_day)) %>%
      #  dplyr::mutate(floor = lubridate::floor_date(min, unit = "day"),
      #                ceiling = lubridate::ceiling_date(max, unit = "day")) %>%
      #  dplyr::mutate(diff = difftime(ceiling, floor, unit = "day")) %>%
      #  dplyr::mutate(interval = diff/6) %>%
      #  dplyr::mutate(interval = floor(interval))



      x <-
        df_day %>%
        dplyr::filter(parameter == parm[j]) %>%
        ggplot2::ggplot(., ggplot2::aes(x = datetimestamp_day, y = value)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = sta)) +
        ggplot2::geom_line(ggplot2::aes(color = 'Hourly Avg'), lwd = 1) +
        ggplot2::geom_rect(data=df,ggplot2::aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax,fill=years),
                  alpha=0.1,inherit.aes=FALSE) +
        ggplot2::labs(x = '', y = SWMPrStorm::y_labeler(parm[j],converted=converted))

      x <-
        x +
        ggplot2::scale_color_manual('', values = c('steelblue3')) +
        ggplot2::scale_fill_manual('', values = c('steelblue3', 'green')) +
        ggplot2::scale_x_datetime(date_breaks = 'day', date_labels = '%b\n%d', guide = guide_axis(check.overlap = TRUE))
        #ggplot2::scale_x_datetime(date_breaks = paste0(breaks$interval, " ", units(breaks$interval)), date_labels = '%b %d')

      x <- x +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 8), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

      x_ttl <- paste('output/wq/timeseries_event_hourly/timeseries_event_hourly_', sta, '_', parm[j], '_', storm_nm, '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

    }
  }


}
