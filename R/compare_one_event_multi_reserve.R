compare_one_event_multi_reserve <- function() {

  ########## WATER QUALITY #####################################################

  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-09-28 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-09 23:45:00' # '2016-10-10 00:00:00'
  view_start <- '2016-09-01 00:00:00' # '2017-08-30 00:00:00'
  view_end <- '2016-12-01 00:00:00' # '2017-09-14 00:00:00'
  recovery_start <- storm_end
  recovery_end <- '2016-11-15 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'acemc', 'sapld', 'niwol', 'nocrc'), 'wq')
  met_sites <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
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

  ## convert select parameters
  ls_par <- lapply(ls_par, function(x) {x$temp <- x$temp * 9 / 5 + 32; x})
  ls_par <- lapply(ls_par, function(x) {x$depth <- x$depth * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$level <- x$level * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$cdepth <- x$cdepth * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$clevel <- x$clevel * 3.28; x})

  names(ls_par) <- wq_sites

  ## identify parameters
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))

  # combine data.frames into one and tidy
  dat <- bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    dplyr::group_by(event, evt_start, evt_end, parameter, station) %>%
    dplyr::summarise(min = min(result, na.rm = T)
              , max = max(result, na.rm = T)
              , mean = mean(result, na.rm = T)
              , median = median(result, na.rm = T))

  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # add factors for spatial filtering
  summary$station_fac <- factor(summary$station, levels = wq_sites)

  # re-sort the table using factors
  summary <- summary %>% dplyr::arrange(., parameter, station_fac)

  # write table
  tbl_ttl <- paste('output/wq/comparison_one_evt_multi_reserve/comparison_', storm_nm, '_multireserve.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)

  ########## Meterological #####################################################
  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-09-28 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-09 23:45:00' # '2016-10-10 00:00:00'
  view_start <- '2016-09-01 00:00:00' # '2017-08-30 00:00:00'
  view_end <- '2016-12-01 00:00:00' # '2017-09-14 00:00:00'
  recovery_start <- storm_end
  recovery_end <- '2016-11-15 00:00:00'

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'acemc', 'sapld', 'niwol', 'nocrc'), 'wq')
  met_sites <- paste0(c('gtmpc', 'acebp', 'sapml', 'niwol', 'nocrc'), 'met')
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
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>% dplyr::filter(parameter %in% parm)

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    dplyr::group_by(event, evt_start, evt_end, parameter, station) %>%
    dplyr::summarise(min = min(result, na.rm = T)
              , max = max(result, na.rm = T)
              , mean = mean(result, na.rm = T)
              , median = median(result, na.rm = T)
              , total = sum(result, na.rm = T))

  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # add factors for spatial filtering
  summary$station_fac <- factor(summary$station, levels = met_sites)

  # re-sort the table using factors
  summary <- summary %>% dplyr::arrange(., parameter, station_fac)

  # write table
  tbl_ttl <- paste('output/met/comparison_one_evt_multi_reserve/comparison_', storm_nm, '_multireserve.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


}


