#' produce daily stat time series for single event at one reserve
#'
#' @param var_in
#' @param data_path
#' @param storm_nm
#' @param storm_start
#' @param storm_end
#' @param reserve
#' @param wq_sites
#' @param met_sites
#' @param keep_flags
#' @param ...
#' @param skip
#'
#' @return
#' @export
#'
#' @examples
compare_one_reserve_one_event <- function(var_in,
                                  data_path,
                                  storm_nm = NULL,
                                  storm_start = NULL,
                                  storm_end = NULL,
                                  reserve = NULL,
                                  wq_sites = NULL,
                                  met_sites = NULL,
                                  keep_flags = NULL,
                                  skip = NULL,
                                  ...) {





  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "stats_daily")
  input_Master <- xlsx::read.xlsx(var_in, sheetName = "MASTER")

  if(is.null(reserve)) reserve <- input_Master[1,2]


  stations <- sampling_stations %>%
    filter(NERR.Site.ID == reserve) %>%
    filter(Status == "Active")

  wq_stations <- stations %>%
    filter(Station.Type == 1)

  met_stations <- stations %>%
    filter(Station.Type == 0)


  if(is.null(storm_nm)) storm_nm <- unlist(strsplit(input_Parameters[1,2],", "))
  if(is.null(storm_start)) storm_start <- unlist(strsplit(input_Parameters[2,2],", "))
  if(is.null(storm_end)) storm_end <- unlist(strsplit(input_Parameters[3,2],", "))
  #if(is.null(wq_sites)) wq_sites <- unlist(strsplit(input_Parameters[5,2],", "))
  if(is.null(wq_sites)) wq_sites <- if(is.na(input_Parameters[4,2])) {wq_stations$Station.Code} else {input_Parameters[4,2]}
  #if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[6,2],", "))
  if(is.null(met_sites)) met_sites <- if(is.na(input_Parameters[5,2])) {met_stations$Station.Code} else {input_Parameters[5,2]}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[6,2],", "))
  if(is.null(skip)) skip <- input_Parameters[7,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'



  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping compare_one_reserve_one_event"))}



  ########## WATER QUALITY #####################################################

  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  names(ls_par) <- wq_sites

  # filter

  evts <- data.frame()
  for(i in 1:length(storm_nm)) {

    evt <- lapply(ls_par, subset, subset = c(as.POSIXct(storm_start[i]), as.POSIXct(storm_end[i])))
    evt <- dplyr::bind_rows(evt, .id = 'station')
    evt$event <- storm_nm[i]

    evts <- dplyr::bind_rows(evts, evt)

  }


  dat <- evts %>% dplyr::relocate(event)


  # combine data.frames into one and tidy
  dat_tidy <- dat %>%
    tidyr::pivot_longer(., 4:length(names(dat)), names_to = 'parameter', values_to = 'result') %>%
    dplyr::mutate(date = as.Date(datetimestamp))

  # ----------------------------------------------
  # Single Event Comparison, Single Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    dplyr::group_by(event, parameter, station, date) %>%
    tidyr::drop_na(result) %>%
    dplyr::summarise(min = min(result, na.rm = T)
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
  tbl_ttl <- paste('output/wq/comparison_one_reserve_one_event/comparison_wq_', storm_nm, '_', reserve, '.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


  ########## Meteorological #####################################################


  # ----------------------------------------------
  # Load meteorological data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  names(ls_par) <- met_sites

  # filter

  evts <- data.frame()
  for(i in 1:length(storm_nm)) {

    evt <- lapply(ls_par, subset, subset = c(as.POSIXct(storm_start[i]), as.POSIXct(storm_end[i])))
    evt <- dplyr::bind_rows(evt, .id = 'station')
    evt$event <- storm_nm[i]

    evts <- dplyr::bind_rows(evts, evt)

  }


  ## convert select parameters
  evts$atemp <- evts$atemp * 9 / 5 + 3
  evts$wspd <- evts$wspd * 3600 * 1 / 1609.34
  evts$maxwspd <- evts$maxwspd * 3600 * 1 / 1609.34
  evts$totprcp <- evts$totprcp / 25.4
  evts$intensprcp <- evts$totprcp * 4

  dat <- evts %>% dplyr::relocate(event)

  # combine data.frames into one and tidy
  dat_tidy <- dat %>%
    tidyr::pivot_longer(., 4:length(names(dat)), names_to = 'parameter', values_to = 'result') %>%
    dplyr::mutate(date = as.Date(datetimestamp))

  # ----------------------------------------------
  # Single Event Comparison, One Reserves ---
  # ----------------------------------------------


  total_nalist <- c("atemp", "bp", "intensprcp", "maxwspd", "rh", "sdwdir", "wdir", "wspd")

  summary <- dat_tidy %>%
    dplyr::group_by(event, parameter, station, date) %>%
    tidyr::drop_na(result) %>%
    dplyr::summarise(min = min(result, na.rm = T)
                     , max = max(result, na.rm = T)
                     , mean = mean(result, na.rm = T)
                     , median = median(result, na.rm = T)
                     , total = sum(result, na.rm = T)) %>%
    dplyr::mutate(total = dplyr::case_when(parameter %in% total_nalist == FALSE ~ total))



  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)



  # write table
  tbl_ttl <- paste('output/met/comparison_one_reserve_one_event/comparison_met_', storm_nm, '_', reserve, '.csv', sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


}
