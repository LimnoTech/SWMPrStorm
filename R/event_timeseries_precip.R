event_timeseries_precip <- function() {

  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-10-02 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-07 00:00:00' # '2016-10-10 00:00:00'
  view_start <- '2016-10-01 00:00:00' # '2017-08-30 00:00:00'
  view_end <- '2016-10-07 00:00:00' # '2017-09-14 00:00:00'
  # recovery_start <- storm_end
  # recovery_end <- '2016-11-15 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'gtmfm', 'gtmpi', 'gtmss'), 'wq')
  met_sites <- 'gtmpcmet'
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')

  # ----------------------------------------------
  # Load MET data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
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
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  names(ls_par) <- met_sites

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

  precip_day$mo <- paste(month.abb[month(precip_day$datetimestamp_day)]
                         , day(precip_day$datetimestamp_day)
                         , sep = ' ') %>%
    factor ##### Note to revisit this line of code.

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
    theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    theme(text = ggplot2::element_text(size = 16)) +
    theme(plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt')) +
    theme(legend.position = 'top')

  x_ttl <- paste('output/met/barplot/barplot_daily_', sta, '_', parm[j], '.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)



}
