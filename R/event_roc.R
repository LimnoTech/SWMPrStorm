event_roc <- function() {

  ########## WATER QUALITY #####################################################

  # a few notes -------------------
  ## Kim Cressman's 2017 eclipse plot: https://github.com/swmpkim/2017_eclipse_viz/blob/master/Eclipse_visualization.Rmd
  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-10-04 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-17 00:00:00' # '2016-10-10 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'acemc', 'sapld', 'niwol', 'nocrc'), 'wq')
  met_sites <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
  met_sites_lvl <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')

  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))#, select = par) # Note: par <- wb_basic %>% .[[1]]

  # ## convert select parameters, add precip intensity (in/hr)
  # ls_par <- lapply(ls_par, function(x) {x$atemp <- x$atemp * 9 / 5 + 32; x}) # C to F
  # ls_par <- lapply(ls_par, function(x) {x$wspd <- x$wspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  # ls_par <- lapply(ls_par, function(x) {x$maxwspd <- x$maxwspd * 3600 * 1 / 1609.34; x}) # m/s to mph
  # ls_par <- lapply(ls_par, function(x) {x$totprcp <- x$totprcp / 25.4; x}) # mm to in
  # ls_par <- lapply(ls_par, function(x) {x$intensprcp <- x$totprcp * 4; x}) # in/15-min to in/hr

  names(ls_par) <- wq_sites

  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% filter(parameter %in% parm)

  # add reserve name
  station_list <-sampling_stations
  add_reserve <- station_list %>% select(station = Station.Code, Reserve.Name)
  dat_tidy <- left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    filter(Station.Code %in% unique(dat_tidy$station)) %>%
    select(Reserve.Name, Latitude) %>%
    distinct() %>%
    arrange(desc(Latitude)) %>%
    mutate(factorid = rank(Latitude))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------
  library(ggridges)
  library(scales)

  unique(dat_tidy$parameter)

  param <- 'temp'
  stn <- 'gtmpcwq'

  roc <- dat_tidy %>%
    filter(station == stn, parameter == param) %>%
    mutate(diff_result = result - lag(result))

  roc_smooth <- dat_tidy %>%
    filter(station == stn, parameter == param) %>%
    group_by(time_hr = floor_date(datetimestamp, "hour")) %>%
    summarise(result = mean(result, na.rm = T)) %>%
    mutate(diff_result = result - lag(result))

  roc %>%
    ggplot(., aes(x = datetimestamp, y = diff_result)) +
    geom_line() +
    scale_x_datetime(date_breaks = '1 day', labels = date_format('%b %d')) +
    ggtitle(param)

  roc_smooth %>%
    ggplot(., aes(x = time_hr, y = diff_result)) +
    geom_line() +
    scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                     , date_breaks = '1 day', labels = date_format('%b %d')) +
    ggtitle(param)

  dat_tidy %>%
    filter(station == stn, parameter == param) %>%
    ggplot(., aes(x = datetimestamp, y = result)) +
    geom_line() +
    scale_x_datetime(limits = c(as.POSIXct(storm_start), as.POSIXct(storm_end))
                     , date_breaks = '1 day', labels = date_format('%b %d')) +
    ggtitle(param)

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------
  param <- 'depth'

  roc <- dat_tidy %>%
    filter(station == stn, parameter == param)

  library(oce)
  tmp <- tidem(t = dat_tidy$datetimestamp, x = dat_tidy$result)
  plot(tmp)

  library(oce)
  data(sealevel)
  tide <- tidem(sealevel)
  plot(tide)


  ########## Meteorological #####################################################
  # a few notes -------------------
  ## Kim Cressman's 2017 eclipse plot: https://github.com/swmpkim/2017_eclipse_viz/blob/master/Eclipse_visualization.Rmd
  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-10-04 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-17 00:00:00' # '2016-10-10 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'acemc', 'sapld', 'niwol', 'nocrc'), 'wq')
  met_sites <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
  met_sites_lvl <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')

  # ----------------------------------------------
  # Load water quality data ----------------------
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
  dat <- bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% filter(parameter %in% parm)

  # add reserve name
  station_list <- sampling_stations
  add_reserve <- station_list %>% select(station = Station.Code, Reserve.Name)
  dat_tidy <- left_join(dat_tidy, add_reserve)

  # assign factors for plotting
  ## turn this into a function

  f_reservename <- station_list %>%
    filter(Station.Code %in% unique(dat_tidy$station)) %>%
    select(Reserve.Name, Latitude) %>%
    distinct() %>%
    arrange(desc(Latitude)) %>%
    mutate(factorid = rank(Latitude))

  # use those factor levels to turn Reserve.Name in the main data frame into a factor, ordered thusly
  dat_tidy$Reserve.Name <- factor(dat_tidy$Reserve.Name
                                  , levels = unique(f_reservename$Reserve.Name[order(f_reservename$factorid)]))

  # ----------------------------------------------
  # Rate of change plot                        ---
  # ----------------------------------------------
  library(ggridges)
  library(scales)

  unique(dat_tidy$parameter)

  param <- 'bp'
  stn <- 'gtmpcmet'

  roc <- dat_tidy %>%
    filter(station == stn, parameter == param) %>%
    mutate(diff_result = result - lag(result))

  roc_smooth <- dat_tidy %>%
    filter(station == 'gtmpcmet', parameter == param) %>%
    group_by(time_hr = floor_date(datetimestamp, "hour")) %>%
    summarise(result = mean(result, na.rm = T)) %>%
    mutate(diff_result = result - lag(result))

  roc %>%
    ggplot(., aes(x = datetimestamp, y = diff_result)) +
    geom_line() +
    ggtitle(param)

  roc_smooth %>%
    ggplot(., aes(x = time_hr, y = diff_result)) +
    geom_line() +
    ggtitle(param)

  dat_tidy %>%
    filter(station == 'gtmpcmet', parameter == param) %>%
    ggplot(., aes(x = datetimestamp, y = result)) +
    geom_line() +
    ggtitle(param)



}
