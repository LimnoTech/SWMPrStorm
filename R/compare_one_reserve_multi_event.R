compare_one_reserve_multi_event <- function() {

  ########## WATER QUALITY #####################################################

  # Define storm ------------------
  # Events, all category 5
  storm_nm <- c('Matthew', 'Irma', 'Maria', 'Michael', 'Dorian')
  storm_start <- c('2016-09-28 00:00:00', '2017-08-30 00:00:00', '2017-09-16 00:00:00', '2018-10-07 00:00:00', '2019-08-24 00:00:00') # '2016-9-28 00:00:00'
  storm_end <- c('2016-10-09 23:45:00', '2017-09-12 23:45:00', '2017-09-30 23:45:00', '2018-10-11 23:45:00', '2019-09-07 23:45:00') # '2016-10-10 00:00:00'

  evt <- read.csv('ext_data/NOAA_NHC_hurricane_events.csv')
  evt$start_event <- ymd_hm(evt$start_event)

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'gtmfm', 'gtmpi', 'gtmss'), 'wq')
  met_sites <- paste0(c('gtmpc'), 'met')
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')

  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  names(ls_par) <- wq_sites

  # filter
  matthew <- lapply(ls_par, subset, subset = c(storm_start[1], storm_end[1]))
  irma <- lapply(ls_par, subset, subset = c(storm_start[2], storm_end[2]))
  maria <- lapply(ls_par, subset, subset = c(storm_start[3], storm_end[3]))
  michael <- lapply(ls_par, subset, subset = c(storm_start[4], storm_end[4]))
  dorian <- lapply(ls_par, subset, subset = c(storm_start[5], storm_end[5]))

  # bind rows
  matthew <- bind_rows(matthew, .id = 'station')
  irma <- bind_rows(irma, .id = 'station')
  maria <- bind_rows(maria, .id = 'station')
  michael <- bind_rows(michael, .id = 'station')
  dorian <- bind_rows(dorian, .id = 'station')

  ls_evts <- list(matthew, irma, maria, michael, dorian)
  names(ls_evts) <- c('matthew', 'irma', 'maria', 'michael', 'dorian')
  evts <- bind_rows(ls_evts, .id = 'event')

  ## convert select parameters
  evts$temp <- evts$temp * 9 / 5 + 3
  evts$level <- evts$level * 3.28
  evts$depth <- evts$depth * 3.28
  evts$clevel <- evts$clevel * 3.28
  evts$cdepth <- evts$cdepth * 3.28

  dat <- evts


  # combine data.frames into one and tidy
  dat_tidy <- dat %>% pivot_longer(., 4:length(names(dat)), names_to = 'parameter', values_to = 'result')

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    group_by(event, parameter, station) %>%
    summarise(min = min(result, na.rm = T)
              , max = max(result, na.rm = T)
              , mean = mean(result, na.rm = T)
              , median = median(result, na.rm = T))

  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # # add factors for spatial filtering
  # summary$station_fac <- factor(summary$station, levels = wq_sites)
  #
  # # re-sort the table using factors
  # summary <- summary %>% arrange(., parameter, station_fac)

  # write table
  tbl_ttl <- paste('output/wq/comparison_one_reserve_multi_event/comparison_', reserve, '_multievent.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


  ########## Meteorological #####################################################

  # Define storm ------------------
  # Events, all category 5
  storm_nm <- c('Matthew', 'Irma', 'Maria', 'Michael', 'Dorian')
  storm_start <- c('2016-09-28 00:00:00', '2017-08-30 00:00:00', '2017-09-16 00:00:00', '2018-10-07 00:00:00', '2019-08-24 00:00:00') # '2016-9-28 00:00:00'
  storm_end <- c('2016-10-09 23:45:00', '2017-09-12 23:45:00', '2017-09-30 23:45:00', '2018-10-11 23:45:00', '2019-09-07 23:45:00') # '2016-10-10 00:00:00'

  evt <- read.csv('ext_data/NOAA_NHC_hurricane_events.csv')
  evt$start_event <- ymd_hm(evt$start_event)

  reserve <- 'gtm'
  wq_sites <- paste0(c('gtmpc', 'gtmfm', 'gtmpi', 'gtmss'), 'wq')
  met_sites <- paste0(c('gtmpc'), 'met')
  stn_target <- 'gtmpc'

  keep_flags <- c('0', '3', '5', '<-4> [SBL]', '1')

  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = 'data/cdmo')
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  names(ls_par) <- met_sites

  # filter
  matthew <- lapply(ls_par, subset, subset = c(storm_start[1], storm_end[1]))
  irma <- lapply(ls_par, subset, subset = c(storm_start[2], storm_end[2]))
  maria <- lapply(ls_par, subset, subset = c(storm_start[3], storm_end[3]))
  michael <- lapply(ls_par, subset, subset = c(storm_start[4], storm_end[4]))
  dorian <- lapply(ls_par, subset, subset = c(storm_start[5], storm_end[5]))

  # bind rows
  matthew <- bind_rows(matthew, .id = 'station')
  irma <- bind_rows(irma, .id = 'station')
  maria <- bind_rows(maria, .id = 'station')
  michael <- bind_rows(michael, .id = 'station')
  dorian <- bind_rows(dorian, .id = 'station')

  ls_evts <- list(matthew, irma, maria, michael, dorian)
  names(ls_evts) <- c('matthew', 'irma', 'maria', 'michael', 'dorian')
  evts <- bind_rows(ls_evts, .id = 'event')

  ## convert select parameters
  evts$atemp <- evts$atemp * 9 / 5 + 3
  evts$wspd <- evts$wspd * 3600 * 1 / 1609.34
  evts$wspd <- evts$maxwspd * 3600 * 1 / 1609.34
  evts$totprcp <- evts$totprcp / 25.4
  evts$intensprcp <- evts$totprcp * 4

  tmp <- evts %>% filter(event == 'matthew')
  sum(tmp$totprcp, na.rm = T)

  dat <- evts

  # combine data.frames into one and tidy
  dat_tidy <- dat %>% pivot_longer(., 4:length(names(dat)), names_to = 'parameter', values_to = 'result')

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    group_by(event, parameter, station) %>%
    summarise(min = min(result, na.rm = T)
              , max = max(result, na.rm = T)
              , mean = mean(result, na.rm = T)
              , median = median(result, na.rm = T)
              , total = sum(result, na.rm = T))

  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # # add factors for spatial filtering
  # summary$station_fac <- factor(summary$station, levels = wq_sites)
  #
  # # re-sort the table using factors
  # summary <- summary %>% arrange(., parameter, station_fac)

  # write table
  tbl_ttl <- paste('output/met/comparison_one_reserve_multi_event/comparison_', reserve, '_multievent.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


}
