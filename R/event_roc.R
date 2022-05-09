#' event_roc
#'
#' @param var_in .xlsx with all required input variables defined (string).
#' @param data_path pathway to cdmo data folder (string).
#' @param storm_nm name of storm event (string).
#' @param storm_start YYYY-MM-DD HH:MM:SS start of storm event (string).
#' @param storm_end YYYY-MM-DD HH:MM:SS end of storm event (string).
#' @param wq_sites comma separated list of wq stations to plot. if left blank, all active stations from reserve will be plotted. (string).
#' @param met_sites comma separated list of met stations to plot. if left blank, all active stations from reserve will be plotted. (string).
#' @param keep_flags comma separated list of data quality flags that should be kept (string).
#' @param reserve 3 digit reserve code (string).
#' @param skip TRUE/FALSE. If TRUE, function will be skipped (string).
#' @param user_units User defined units. Set to "English" or "SI". Default CDMO data is in SI units. Not all parameters have common equivalent English units (e.g. concentrations), and therefore not all will be converted.
#'
#' @return plots are generated and saved in /output/wq/event_roc/ and /output/met/event_roc/
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #StormVariables.xlsx is a template variable input file saved in data/
#' vars_in <- 'data/StormTrackVariables.xlsx'
#' single_storm_track(var_in = vars_in)
#' }
event_roc <- function(var_in,
                      data_path = NULL,
                      reserve = NULL,
                      storm_nm = NULL,
                      storm_start = NULL,
                      storm_end = NULL,
                      wq_sites = NULL,
                      met_sites = NULL,
                      keep_flags = NULL,
                      skip = NULL,
                      user_units = NULL) {

  # ----------------------------------------------------------------------------
  # Define global variables
  # ----------------------------------------------------------------------------
  NERR.Site.ID_ <- rlang::sym('NERR.Site.ID')
  Status_ <- rlang::sym('Status')
  Station.Type_ <- rlang::sym('Station.Type')
  Station.Code_ <- rlang::sym('Station.Code')
  Reserve.Name_ <- rlang::sym('Reserve.Name')
  Latitude_ <- rlang::sym('Latitude')


  station_ <- rlang::sym('station')
  event_ <- rlang::sym('event')
  parameter_ <- rlang::sym('parameter')
  result_ <- rlang::sym('result')
  datetimestamp_ <- rlang::sym('datetimestamp')
  datetime_floor_ <- rlang::sym('datetime_floor')
  avg_ <- rlang::sym('avg')
  diff_result_ <- rlang::sym('diff_result')
  legend_ <- rlang::sym('legend')
  time_hr_ <- rlang::sym('time_hr')


  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in


  input_Parameters <- openxlsx::read.xlsx(var_in, sheet = "rate_of_change")
  input_Master <- openxlsx::read.xlsx(var_in, sheet = "MASTER")

  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(is.null(reserve)) reserve <- input_Master[1,2]


  stations <- get('sampling_stations') %>%
    dplyr::filter(!! NERR.Site.ID_ == reserve) %>%
    dplyr::filter(!! Status_ == "Active")

  wq_stations <- stations %>%
    dplyr::filter(!! Station.Type_ == 1)

  met_stations <- stations %>%
    dplyr::filter(!! Station.Type_ == 0)

  if(is.null(storm_nm)) storm_nm <- input_Parameters[1,2]
  if(is.null(storm_start)) storm_start <- input_Parameters[2,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[3,2]
  #if(is.null(wq_sites)) wq_sites <- unlist(strsplit(input_Parameters[4,2],", "))
  if(is.null(wq_sites)) wq_sites <- if(is.na(input_Parameters[4,2])) {wq_stations$Station.Code} else {unlist(strsplit(input_Parameters[4,2],", "))}
  #if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[5,2],", "))
  if(is.null(met_sites)) met_sites <- if(is.na(input_Parameters[5,2])) {met_stations$Station.Code} else {unlist(strsplit(input_Parameters[5,2],", "))}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[6,2],", "))
  if(is.null(skip)) skip <- input_Parameters[7,2]
  if(is.null(user_units)) user_units <- input_Parameters[8,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'



  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping event_roc"))}



  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  names(ls_par) <- wq_sites

  ## convert dataset to user defined units (if "SI", no conversion will take place)
  ls_par <- SWMPrStorm::convert_units(ls_par, user_units)


  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]]))
  parm <- subset(parm, !(parm %in% c('datetimestamp', 'wdir', 'sdwdir', 'totpar', 'totsorad')))


  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- tidyr::pivot_longer(dat, 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% dplyr::filter(!! parameter_ %in% parm)

  # add reserve name
  station_list <- get('sampling_stations')
  add_reserve <- station_list %>% dplyr::select("station" = !! Station.Code_, !! Reserve.Name_)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    dplyr::filter(!! Station.Code_ %in% unique(dat_tidy$station)) %>%
    dplyr::select(!! Reserve.Name_, !! Latitude_) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(!! Latitude_)) %>%
    dplyr::mutate(factorid = rank(!! Latitude_))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------


  for(p in parm) {
    for(s in wq_sites) {

      roc <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::mutate("diff_result" = !! result_ - dplyr::lag(!! result_),
                      "legend" = "Raw Data")

      roc_smooth <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::group_by("time_hr" = lubridate::floor_date(!! datetimestamp_, "hour")) %>%
        dplyr::summarise("result" = mean(!! result_, na.rm = T)) %>%
        dplyr::mutate("diff_result" = !! result_ - dplyr::lag(!! result_),
                      "legend" = "Hourly Avg")

      roc_raw <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::mutate("legend" = "Raw Data")


      p1 <- ggplot2::ggplot(roc, ggplot2::aes(x = !! datetimestamp_, y = !! diff_result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(p) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

        ggplot2::ggsave(filename = paste0("output/wq/event_roc/roc_", s, "_", p, "_stepwise.png"), plot = p1, height=4, width=6, dpi=300)




      p2 <- ggplot2::ggplot(roc_smooth, ggplot2::aes(x = !! time_hr_, y = !! diff_result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

        ggplot2::ggsave(filename = paste0("output/wq/event_roc/roc_", s, "_", p, "_hourly.png"), plot = p2, height=4, width=6, dpi=300)

      p3 <- ggplot2::ggplot(roc_raw, ggplot2::aes(x = !! datetimestamp_, y = !! result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')


        ggplot2::ggsave(filename = paste0("output/wq/event_roc/roc_", s, "_", p, "_raw.png"), plot = p3, height=4, width=6, dpi=300)



    }
  }








  # ----------------------------------------------
  # Load met data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

  names(ls_par) <- met_sites


  ## convert dataset to user defined units (if "SI", no conversion will take place)
  ls_par <- SWMPrStorm::convert_units(ls_par, user_units)

  ## add in precip intensity
  ls_par <- lapply(ls_par, function(x) {x$intensprcp <- x$totprcp * 4; x}) # unit/15-min to unit/hr


  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]]))
  parm <- subset(parm, !(parm %in% c('datetimestamp', 'wdir', 'sdwdir', 'totsorad')))


  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- tidyr::pivot_longer(dat, 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% dplyr::filter(!! parameter_ %in% parm)

  # add reserve name
  station_list <- get('sampling_stations')
  add_reserve <- station_list %>% dplyr::select("station" = !! Station.Code_, !! Reserve.Name_)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    dplyr::filter(!! Station.Code_ %in% unique(dat_tidy$station)) %>%
    dplyr::select(!! Reserve.Name_, !! Latitude_) %>%
    dplyr::distinct() %>%
    dplyr::arrange(dplyr::desc(!! Latitude_)) %>%
    dplyr::mutate(factorid = rank(!! Latitude_))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------



  for(p in parm) {
    for (s in met_sites) {

      roc <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::mutate("diff_result" = !! result_ - dplyr::lag(!! result_),
                      "legend" = "Raw Data")

      roc_smooth <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::group_by("time_hr" = lubridate::floor_date(!! datetimestamp_, "hour")) %>%
        dplyr::summarise("result" = mean(!! result_, na.rm = T)) %>%
        dplyr::mutate("diff_result" = !! result_ - dplyr::lag(!! result_),
                      "legend" = "Hourly Avg")

      roc_raw <- dat_tidy %>%
        dplyr::filter(!! station_ == s, !! parameter_ == p) %>%
        dplyr::mutate("legend" = "Raw Data")


      p1 <- ggplot2::ggplot(roc, ggplot2::aes(x = !! datetimestamp_, y = !! diff_result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

        ggplot2::ggsave(filename = paste0("output/met/event_roc/roc_", s, "_", p, "_stepwise.png"), plot = p1, height=4, width=6, dpi=300)



      p2 <- ggplot2::ggplot(roc_smooth, ggplot2::aes(x = !! time_hr_, y = !! diff_result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

        ggplot2::ggsave(filename = paste0("output/met/event_roc/roc_", s, "_", p, "_hourly.png"), plot = p2, height=4, width=6, dpi=300)


      p3 <-  ggplot2::ggplot(roc_raw, ggplot2::aes(x = !! datetimestamp_, y = !! result_)) +
        ggplot2::geom_line(ggplot2::aes(color = !! legend_), lwd = 1) +
        ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                                  , date_breaks = '1 day'
                                  , labels = scales::date_format('%b %d')
                                  , guide = ggplot2::guide_axis(check.overlap = TRUE)) +
        ggplot2::scale_color_manual(values = c("steelblue3"), name = "") +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = s)) +
        ggplot2::ylab(SWMPrStorm::y_axis_unit_labeler_delta(p, user_units)) +
        ggplot2::xlab("") +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 10, 5.5, 15, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')

        ggplot2::ggsave(filename = paste0("output/met/event_roc/roc_", s, "_", p, "_raw.png"), plot=p3, height=4, width=6, dpi=300)


    }
  }

}
