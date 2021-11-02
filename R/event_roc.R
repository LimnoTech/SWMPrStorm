event_roc <- function(var_in,
                      data_path,
                      storm_nm = NULL,
                      storm_start = NULL,
                      storm_end = NULL,
                      view_start = NULL,
                      view_end = NULL,
                      recovery_start = NULL,
                      recovery_end = NULL,
                      reserve = NULL,
                      stn_wq = NULL,
                      wq_sites = NULL,
                      stn_met = NULL,
                      met_sites = NULL,
                      stn_target = NULL,
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
  if(is.null(view_start)) view_start <- input_Parameters[4,2]
  if(is.null(view_end)) view_end <- input_Parameters[5,2]
  if(is.null(recovery_start)) recovery_start <- storm_end
  if(is.null(recovery_end)) recovery_end <- input_Parameters[6,2]
  if(is.null(reserve)) reserve <- input_Parameters[7,2]
  if(is.null(stn_wq)) stn_wq <- input_Parameters[9,2]
  if(is.null(wq_sites)) wq_sites <- input_Sites$wq_sites[!is.na(input_Sites$wq_sites)]
  if(is.null(stn_met)) stn_met <- input_Parameters[10,2]
  if(is.null(met_sites)) met_sites <- input_Sites$met_sites[!is.na(input_Sites$met_sites)]
  if(is.null(stn_target)) stn_target <- input_Parameters[8,2]
  if(is.null(keep_flags)) keep_flags <- input_Flags$keep_flags
  if(is.null(data_path)) data_path <- 'data/cdmo'


  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = 'data/cdmo')
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

  param <- 'temp'
  stn <- 'gtmpcwq'

  roc <- dat_tidy %>%
    dplyr::filter(station == stn, parameter == param) %>%
    dplyr::mutate(diff_result = result - lag(result))

  roc_smooth <- dat_tidy %>%
    dplyr::filter(station == stn, parameter == param) %>%
    dplyr::group_by(time_hr = lubridate::floor_date(datetimestamp, "hour")) %>%
    dplyr::summarise(result = mean(result, na.rm = T)) %>%
    dplyr::mutate(diff_result = result - lag(result))

  roc %>%
    ggplot2::ggplot(., ggplot2::aes(x = datetimestamp, y = diff_result)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(date_breaks = '1 day', labels = date_format('%b %d')) +
    ggplot2::ggtitle(param)

  roc_smooth %>%
    ggplot2::ggplot(., ggplot2::aes(x = time_hr, y = diff_result)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                     , date_breaks = '1 day', labels = date_format('%b %d')) +
    ggplot2::ggtitle(param)

  dat_tidy %>%
    dplyr::filter(station == stn, parameter == param) %>%
    ggplot2::ggplot(., ggplot2::aes(x = datetimestamp, y = result)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                     , date_breaks = '1 day', labels = date_format('%b %d')) +
    ggplot2::ggtitle(param)

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------
  param <- 'depth'

  roc <- dat_tidy %>%
    dplyr::filter(station == stn, parameter == param)


  ### i think these next few lines of code can be removed... sealevel is a default dataset with 'oce' package?
  tmp <- oce::tidem(t = dat_tidy$datetimestamp, x = dat_tidy$result)
  plot(tmp)

  data(sealevel)
  tide <- oce::tidem(sealevel)
  plot(tide)
  ###



  # ----------------------------------------------
  # Load met data ----------------------
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

  unique(dat_tidy$parameter)

  param <- 'bp'
  stn <- 'gtmpcmet'

  roc <- dat_tidy %>%
    dplyr::filter(station == stn, parameter == param) %>%
    dplyr::mutate(diff_result = result - lag(result))

  roc_smooth <- dat_tidy %>%
    dplyr::filter(station == 'gtmpcmet', parameter == param) %>%
    dplyr::group_by(time_hr = lubridate::floor_date(datetimestamp, "hour")) %>%
    dplyr::summarise(result = mean(result, na.rm = T)) %>%
    dplyr::mutate(diff_result = result - stats::lag(result))

  roc %>%
    ggplot2::ggplot(., ggplot2::aes(x = datetimestamp, y = diff_result)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(param)

  roc_smooth %>%
    ggplot2::ggplot(., ggplot2::aes(x = time_hr, y = diff_result)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(param)

  dat_tidy %>%
    ggplot2::filter(station == 'gtmpcmet', parameter == param) %>%
    ggplot2::ggplot(., aes(x = datetimestamp, y = result)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(param)


}
