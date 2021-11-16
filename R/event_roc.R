#' Title
#'
#' @param var_in
#' @param data_path
#' @param storm_nm
#' @param storm_start
#' @param storm_end
#' @param wq_sites
#' @param met_sites
#' @param keep_flags
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
event_roc <- function(var_in,
                      data_path,
                      storm_nm = NULL,
                      storm_start = NULL,
                      storm_end = NULL,
                      wq_sites = NULL,
                      met_sites = NULL,
                      keep_flags = NULL,
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

  if(is.null(storm_nm)) storm_nm <- input_Parameters[1,2]
  if(is.null(storm_start)) storm_start <- input_Parameters[2,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[3,2]
  if(is.null(wq_sites)) wq_sites <- input_Sites$wq_sites[!is.na(input_Sites$wq_sites)]
  if(is.null(met_sites)) met_sites <- input_Sites$met_sites[!is.na(input_Sites$met_sites)]
  if(is.null(keep_flags)) keep_flags <- input_Flags$keep_flags
  if(is.null(data_path)) data_path <- 'data/cdmo'


  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  names(ls_par) <- wq_sites

  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% dplyr::filter(parameter %in% parm)

  # add reserve name
  station_list <-sampling_stations
  add_reserve <- station_list %>% dplyr::select(station = Station.Code, Reserve.Name)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    dplyr::filter(Station.Code %in% unique(dat_tidy$station)) %>%
    dplyr::select(Reserve.Name, Latitude) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(Latitude)) %>%
    dplyr::mutate(factorid = rank(Latitude))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------

  unique(dat_tidy$parameter)

  p <- 'temp'
  s <- 'gtmpcwq'

  for(p in parm) {
    for(s in wq_sites) {

      roc <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        dplyr::mutate(diff_result = result - dplyr::lag(result))

      roc_smooth <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        dplyr::group_by(time_hr = lubridate::floor_date(datetimestamp, "hour")) %>%
        dplyr::summarise(result = mean(result, na.rm = T)) %>%
        dplyr::mutate(diff_result = result - dplyr::lag(result))


      p1 <- ggplot2::ggplot(roc, ggplot2::aes(x = datetimestamp, y = diff_result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(p) +
        ggplot2::ylab(SWMPrStorm::y_labeler_delta(p)) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/wq/event_roc/roc_", s, "_", p, "_stepwise.png"), height=4, width=6, dpi=300)


      p2 <- ggplot2::ggplot(roc_smooth, ggplot2::aes(x = time_hr, y = diff_result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_labeler_delta(p)) +
        ggplot2::xlab("Datetime, smoothed hourly") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/wq/event_roc/roc_", s, "_", p, "_hourly.png"), height=4, width=6, dpi=300)

      p3 <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        ggplot2::ggplot(., ggplot2::aes(x = datetimestamp, y = result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_labeler(p)) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/wq/event_roc/roc_", s, "_", p, "_raw.png"), height=4, width=6, dpi=300)



    }
  }








  # ----------------------------------------------
  # Load met data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
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
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totsorad')))
  #parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% dplyr::filter(parameter %in% parm)

  # add reserve name
  station_list <- sampling_stations
  add_reserve <- station_list %>% dplyr::select(station = Station.Code, Reserve.Name)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    dplyr::filter(Station.Code %in% unique(dat_tidy$station)) %>%
    dplyr::select(Reserve.Name, Latitude) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(Latitude)) %>%
    dplyr::mutate(factorid = rank(Latitude))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------

  p <- "atemp"
  s <- "gtmpcmet"

  for(p in parm) {
    for (s in met_sites) {

      roc <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        dplyr::mutate(diff_result = result - dplyr::lag(result))

      roc_smooth <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        dplyr::group_by(time_hr = lubridate::floor_date(datetimestamp, "hour")) %>%
        dplyr::summarise(result = mean(result, na.rm = T)) %>%
        dplyr::mutate(diff_result = result - dplyr::lag(result))

      p1 <- ggplot2::ggplot(roc, ggplot2::aes(x = datetimestamp, y = diff_result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_labeler_delta(p)) +
        ggplot2::xlab("Datetime") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/met/event_roc/roc_", s, "_", p, "_stepwise.png"), height=4, width=6, dpi=300)



      p2 <- ggplot2::ggplot(roc_smooth, ggplot2::aes(x = time_hr, y = diff_result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_labeler_delta(p)) +
        ggplot2::xlab("Datetime, smoothed hourly") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/met/event_roc/roc_", s, "_", p, "_hourly.png"), height=4, width=6, dpi=300)


      p3 <- dat_tidy %>%
        dplyr::filter(station == s, parameter == p) %>%
        ggplot2::ggplot(., aes(x = datetimestamp, y = result)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = guide_axis(check.overlap = TRUE)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_labeler(p)) +
        ggplot2::xlab("Datetime") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top') +
        ggplot2::ggsave(paste0("output/met/event_roc/roc_", s, "_", p, "_raw.png"), height=4, width=6, dpi=300)


    }
  }

}
