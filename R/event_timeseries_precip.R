#' event_timeseries_precip
#'
#' @param var_in .xlsx with all required input variables defined (string).
#' @param data_path pathway to cdmo data folder (string).
#' @param storm_start YYYY-MM-DD HH:MM:SS start of storm event (string).
#' @param storm_end YYYY-MM-DD HH:MM:SS end of storm event (string).
#' @param stn_met comma separated list of data quality flags that should be kept (string).
#' @param keep_flags comma separated list of data quality flags that should be kept (string).
#' @param flip TRUE/FALSE. If TRUE, the plot axes will flip.
#' @param reserve 3 digit reserve code (string).
#' @param skip TRUE/FALSE. If TRUE, function will be skipped (string).
#'
#' @return
#' @export
#'
#' @examples
event_timeseries_precip <- function(var_in,
                                    data_path,
                                    reserve = NULL,
                                    storm_start = NULL,
                                    storm_end = NULL,
                                    stn_met = NULL,
                                    keep_flags = NULL,
                                    flip = FALSE,
                                    skip = NULL) {

  # ----------------------------------------------------------------------------
  # Define global variables
  # ----------------------------------------------------------------------------
  NERR.Site.ID_ <- rlang::sym('NERR.Site.ID')
  Status_ <- rlang::sym('Status')
  Station.Type_ <- rlang::sym('Station.Type')

  parameter_ <- rlang::sym('parameter')
  value_ <- rlang::sym('value')

  datetimestamp_ <- rlang::sym('datetimestamp')
  datetimestamp_day_ <- rlang::sym('datetimestamp_day')
  totprcp_ <- rlang::sym('totprcp')
  mo_ <- rlang::sym('mo')
  total_precip_in_ <- rlang::sym('total_precip_in')
  Date_ <- rlang::sym('Date')
  label_ <- rlang::sym('label')
  intensprcp_ <- rlang::sym('intensprcp')


  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "precip_barplots")
  input_Master <- xlsx::read.xlsx(var_in, sheetName = "MASTER")

  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  # read in all available stations based on reserve (reserve is a required field)
  if(is.null(reserve)) reserve <- input_Master[1,2]

  stations <- get('sampling_stations') %>%
    dplyr::filter(!! NERR.Site.ID_ == reserve) %>%
    dplyr::filter(!! Status_ == "Active")

  met_stations <- stations %>%
    dplyr::filter(!! Station.Type_ == 0)

  if(is.null(storm_start)) storm_start <- input_Parameters[1,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[2,2]
  if(is.null(stn_met)) stn_met <- if(is.na(input_Parameters[3,2])) {met_stations$Station.Code} else {input_Parameters[3,2]}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[4,2],", "))
  if(is.null(flip)) flip <- input_Parameters[5,2]
  if(is.null(skip)) skip <- input_Parameters[6,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'

  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping event_timeseries_precip"))}



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
  parm <- unique(names(ls_par[[1]]))
  parm <- subset(parm, !(parm %in% c('datetimestamp', 'wdir', 'sdwdir', 'totpar', 'totsorad')))


  # ----------------------------------------------
  # Time series, precip                      -----
  # ----------------------------------------------

  for(i in 1:length(ls_par)) {

      precip <- ls_par[[i]]

      precip_day <- precip %>%
        dplyr::filter(dplyr::between(!! datetimestamp_
                       , as.POSIXct(storm_start)
                       , as.POSIXct(storm_end))) %>%
        dplyr::mutate("datetimestamp_day" = lubridate::floor_date(!! datetimestamp_, unit = 'day')) %>%
        dplyr::group_by(!! datetimestamp_day_) %>%
        dplyr::summarize(value = sum(!! totprcp_, na.rm = T))

      precip_day$mo <- paste(month.abb[lubridate::month(precip_day$datetimestamp_day)]
                             , lubridate::day(precip_day$datetimestamp_day)
                             , sep = ' ') %>%
        factor

      precip_day <- precip_day %>%
        dplyr::mutate("mo" = factor(!! mo_, levels = (!! mo_)[order((!! datetimestamp_day_))]))

      if(flip == FALSE) {
      # basic plot
      x <- ggplot2::ggplot(precip_day, ggplot2::aes(x = !! mo_, y = !! value_)) +
        ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
        ggplot2::labs(x = NULL, y = 'Daily Precipitation (in)') +
        ggplot2::coord_flip() +
        ggplot2::scale_x_discrete(limits = rev(levels(precip_day$mo)))

      x <- x +
        ggplot2::scale_y_continuous(limits = c(0, max(ceiling(precip_day$value))), expand = c(0,0))

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


      x_ttl <- paste('output/met/barplot/barplot_daily_', stn_met[i], '_', 'totprcp', '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)

      } else if(flip == TRUE) {
        x <- ggplot2::ggplot(precip_day, ggplot2::aes(x = !! mo_, y = !! value_)) +
          ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
          ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
          ggplot2::labs(x = NULL, y = 'Daily Precipitation (in)')

        # colors
        x <- x +
          ggplot2::scale_y_continuous(limits = c(0, max(ceiling(precip_day$value))), expand = c(0,0))

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

        x_ttl <- paste('output/met/barplot/barplot_daily_', stn_met[i], '_', 'totprcp', '.png', sep = '')

        ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)
      }



      # ----------------------------------------------
      # Cumulative Precip                        -----
      # ----------------------------------------------

      precip_all <- precip_day %>%
        dplyr::summarize("total_precip_in" = sum(!! value_)) %>%
        dplyr::mutate("Date" = paste0(precip_day$mo[1], " - ", utils::tail(precip_day$mo,n=1))) %>%
        dplyr::mutate("label" = round(!! total_precip_in_, 2))


      if(flip == TRUE) {
      x <- ggplot2::ggplot(precip_all, ggplot2::aes(x=!! Date_, y = !! total_precip_in_)) +
        ggplot2::geom_col(fill = "steelblue3", width = 0.5) +
        ggplot2::geom_text(ggplot2::aes(x=!! Date_, y = (!! total_precip_in_ + + ceiling(max(precip_all$total_precip_in)*1.25)*.05), label = !! label_), color = "steelblue3", fontface="bold") +
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
                       axis.text = ggplot2::element_blank(),
                       axis.ticks = ggplot2::element_blank(),
                       plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                       legend.position = 'top')


      } else if(flip == FALSE) {
        x <- ggplot2::ggplot(precip_all, ggplot2::aes(x=!! Date_, y = !! total_precip_in_)) +
          ggplot2::geom_col(fill = "steelblue3", width = 0.5) +
          ggplot2::geom_text(ggplot2::aes(x=!! Date_, y = (!! total_precip_in_ + ceiling(max(precip_all$total_precip_in)*1.25)*.05), label = !! label_), color = "steelblue3", fontface="bold",hjust=0) +
          ggplot2::coord_flip() +
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
                         axis.text = ggplot2::element_blank(),
                         axis.ticks = ggplot2::element_blank(),
                         plot.margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'),
                         legend.position = 'top')


      }


      x_ttl <- paste('output/met/barplot/barplot_cumulative_', stn_met[i], '_', 'totprcp', '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)



      # ----------------------------------------------
      # Daily Max Intensity Precip               -----
      # ----------------------------------------------


      precip <- ls_par[[i]]

      precip_int <- precip %>%
        dplyr::filter(dplyr::between(!! datetimestamp_
                                     , as.POSIXct(storm_start)
                                     , as.POSIXct(storm_end))) %>%
        dplyr::mutate("datetimestamp_day" = lubridate::floor_date(!! datetimestamp_, unit = 'day')) %>%
        dplyr::group_by(!! datetimestamp_day_) %>%
        dplyr::summarize("value" = max(!! intensprcp_, na.rm = T))

      precip_int$mo <- paste(month.abb[lubridate::month(precip_day$datetimestamp_day)]
                             , lubridate::day(precip_day$datetimestamp_day)
                             , sep = ' ') %>%
        factor

      precip_int <- precip_int %>%
        dplyr::mutate("mo" = factor(!! mo_, levels =(!! mo_)[order((!! datetimestamp_day_))]))

      if(flip == FALSE) {
        # basic plot
        x <- ggplot2::ggplot(precip_int, ggplot2::aes(x = !! mo_, y = !! value_)) +
          ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
          ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
          ggplot2::coord_flip() +
          ggplot2::scale_x_discrete(limits = rev(levels(precip_int$mo))) +
          ggplot2::labs(x = NULL, y = 'Max Precipitation Intensity (in/hour)')

        # colors
        x <- x +
          ggplot2::scale_y_continuous(limits = c(0, max(ceiling(precip_day$value))), expand = c(0,0))


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


        x_ttl <- paste('output/met/barplot/barplot_daily_', stn_met[i], '_', 'intensprcp ', '.png', sep = '')

        ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)

      } else if(flip == TRUE) {
        x <- ggplot2::ggplot(precip_int, ggplot2::aes(x = !! mo_, y = !! value_)) +
          ggplot2::geom_bar(stat = 'identity', fill = 'steelblue3') +
          ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_met)) +
          ggplot2::labs(x = NULL, y = 'Max Precipitation Intensity (in/hour)')

        # colors
        x <- x +
          ggplot2::scale_y_continuous(limits = c(0, max(ceiling(precip_day$value))), expand = c(0,0))


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

        x_ttl <- paste('output/met/barplot/barplot_daily_', stn_met[i], '_', 'intensprcp ', '.png', sep = '')

        ggplot2::ggsave(filename = x_ttl, plot = x, height = 6, width = 4, units = 'in', dpi = 300)
      }

  }

}
