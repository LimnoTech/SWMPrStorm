#' Title
#'
#' @param var_in
#' @param data_path
#' @param storm_start
#' @param storm_end
#' @param view_start
#' @param view_end
#' @param stn_met
#' @param keep_flags
#' @param ...
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
event_timeseries_precip <- function(var_in,
                                    data_path,
                                    storm_start = NULL,
                                    storm_end = NULL,
                                    view_start = NULL,
                                    view_end = NULL,
                                    stn_met = NULL,
                                    keep_flags = NULL,
                                    flip = FALSE,
                                    ...) {



  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

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
  if(is.null(stn_met)) stn_met <- input_Parameters[10,2]
  if(is.null(keep_flags)) keep_flags <- input_Flags$keep_flags
  if(is.null(data_path)) data_path <- 'data/cdmo'


  # ----------------------------------------------
  # Load MET data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(stn_met, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end)) #, select = par) # Note: par <- wb_basic %>% .[[1]]

  ## convert select parameters, add precip intensity (in/hr)
  ls_par <- lapply(ls_par, function(x) {x$atemp <- x$atemp * 9 / 5 + 32; x}) # C to F
  ls_par <- lapply(ls_par, function(x) {x$wspd <- x$wspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$maxwspd <- x$maxwspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  ls_par <- lapply(ls_par, function(x) {x$totprcp <- x$totprcp / 25.4; x}) # mm to in
  ls_par <- lapply(ls_par, function(x) {x$intensprcp <- x$totprcp * 4; x}) # in/15-min to in/hr

  names(ls_par) <- stn_met

  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  names(ls_par) <- stn_met

  ## identify parameters
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))

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

  precip_day$mo <- paste(month.abb[lubridate::month(precip_day$datetimestamp_day)]
                         , lubridate::day(precip_day$datetimestamp_day)
                         , sep = ' ') %>%
    factor

  precip_day <- precip_day %>%
    dplyr::mutate(mo = factor(mo, levels = mo[order(datetimestamp_day)]))

  if(flip == FALSE) {
  # basic plot
  x <- ggplot2::ggplot(precip_day, ggplot2::aes(x = mo, y = value)) +
    ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(limits = rev(levels(precip_day$mo)))

  # colors
  x <- x +
    ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5), breaks = seq(0 , 1.6, 0.25)) +
    ggplot2::labs(x = NULL, y = 'Daily Precipitation (in)')

  x <-
    x +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                   text = ggplot2::element_text(size = 16),
                   plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                   legend.position = 'top')


  x_ttl <- paste('output/met/barplot/barplot_daily_', stn_met, '_', 'intensprcp', '.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)

  } else if(flip == TRUE) {
    x <- ggplot2::ggplot(precip_day, ggplot2::aes(x = mo, y = value)) +
      ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
      ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met))

    # colors
    x <- x +
      ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.5), breaks = seq(0 , 1.6, 0.25)) +
      ggplot2::labs(x = NULL, y = 'Daily Precipitation (in)')

    x <-
      x +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     strip.background = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                     legend.position = 'top')
  }



  # ----------------------------------------------
  # Cumulative Precip                        -----
  # ----------------------------------------------

  precip_all <- precip_day %>%
    dplyr::summarize(total_precip_in = sum(value)) %>%
    dplyr::mutate(Date = paste0(precip_day$mo[1], " - ", tail(precip_day$mo,n=1))) %>%
    dplyr::mutate(label = round(total_precip_in, 2))


  if(flip == TRUE) {
  x <- ggplot2::ggplot(precip_all, ggplot2::aes(x=Date, y = total_precip_in)) +
    ggplot2::geom_col(fill = "steelblue3", width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(x=Date, y = total_precip_in +0.2, label = label), color = "steelblue3", fontface="bold") +
    ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, ceiling(max(precip_all$total_precip_in)*1.25))) +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
    ggplot2::xlab("") +
    ggplot2::ylab("Total Precipitation (in)") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   strip.background = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                   axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                   text = ggplot2::element_text(size = 16),
                   plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                   legend.position = 'top')


  } else if(flip == FALSE) {
    x <- ggplot2::ggplot(precip_all, ggplot2::aes(x=Date, y = total_precip_in)) +
      ggplot2::geom_col(fill = "steelblue3", width = 0.5) +
      ggplot2::geom_text(ggplot2::aes(x=Date, y = total_precip_in +0.3, label = label), color = "steelblue3", fontface="bold",hjust=0) +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(expand = c(0,0), limits = c(0, ceiling(max(precip_all$total_precip_in)*1.25))) +
      ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
      ggplot2::xlab("") +
      ggplot2::ylab("Total Precipitation (in)") +
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     strip.background = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank(),
                     panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                     legend.position = 'top')


  }


  x_ttl <- paste('output/met/barplot/barplot_cumulative_', stn_met, '_', 'intensprcp', '.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)



}
