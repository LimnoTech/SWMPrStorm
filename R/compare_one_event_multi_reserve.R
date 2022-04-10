#' compare_one_event_multi_reserve
#'
#' @param var_in .xlsx with all required input variables defined (string).
#' @param data_path pathway to cdmo data folder (string).
#' @param storm_nm name of storm event (string).
#' @param storm_start YYYY-MM-DD HH:MM:SS (string).
#' @param storm_end YYYY-MM-DD HH:MM:SS (string).
#' @param keep_flags comma separated list of data quality flags that should be kept (string).
#' @param reserve 3 digit reserve code (string).
#' @param skip TRUE/FALSE. If TRUE, function will be skipped (string).
#'
#'
#'
#' @return
#' @export
#'
#' @examples
compare_one_event_multi_reserve <- function(var_in,
                                            data_path,
                                            reserve = NULL,
                                            storm_nm = NULL,
                                            storm_start = NULL,
                                            storm_end = NULL,
                                            keep_flags = NULL,
                                            skip = NULL) {

  # ----------------------------------------------------------------------------
  # Define global variables
  # ----------------------------------------------------------------------------
  NERR.Site.ID_ <- rlang::sym('NERR.Site.ID')
  Status_ <- rlang::sym('Status')
  Station.Type_ <- rlang::sym('Station.Type')

  station_ <- rlang::sym('station')
  event_ <- rlang::sym('event')
  evt_start_ <- rlang::sym('evt_start')
  evt_end_ <- rlang::sym('evt_end')
  parameter_ <- rlang::sym('parameter')
  result_ <- rlang::sym('result')

  station_fac_ <- rlang::sym('station_fac')

  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "stats_one_event")
  input_Master <- xlsx::read.xlsx(var_in, sheetName = "MASTER")


  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(is.null(storm_nm)) storm_nm <- unlist(strsplit(input_Parameters[1,2],", "))
  if(is.null(storm_start)) storm_start <- unlist(strsplit(input_Parameters[2,2],", "))
  if(is.null(storm_end)) storm_end <- unlist(strsplit(input_Parameters[3,2],", "))
  #if(is.null(wq_sites)) wq_sites <- unlist(strsplit(input_Parameters[4,2],", "))
  #if(is.null(wq_sites)) wq_sites <- if(is.na(input_Parameters[4,2])) {wq_stations$Station.Code} else {input_Parameters[4,2]}
  #if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[5,2],", "))
  #if(is.null(met_sites)) met_sites <- if(is.na(input_Parameters[5,2])) {met_stations$Station.Code} else {input_Parameters[5,2]}
  if(is.null(reserve)) reserve <- if(is.na(input_Parameters[4,2])) {input_Master[1,2]} else { unlist(strsplit(input_Parameters[4,2],", "))}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[5,2],", "))
  if(is.null(skip)) skip <- input_Parameters[6,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'


  stations <- get('sampling_stations') %>%
    dplyr::filter(!! NERR.Site.ID_ == reserve) %>%
    dplyr::filter(!! Status_ == "Active")

  wq_sites <- stations %>%
    dplyr::filter(!! Station.Type_ == 1)
  wq_sites <- wq_sites$Station.Code

  met_sites <- stations %>%
    dplyr::filter(!! Station.Type_ == 0)
  met_sites <- met_sites$Station.Code


  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping compare_one_event_multi_reserve"))}



  ########## WATER QUALITY #####################################################


  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

  ## convert select parameters
  ls_par <- lapply(ls_par, function(x) {x$temp <- x$temp * 9 / 5 + 32; x})
  ls_par <- lapply(ls_par, function(x) {x$depth <- x$depth * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$level <- x$level * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$cdepth <- x$cdepth * 3.28; x})
  ls_par <- lapply(ls_par, function(x) {x$clevel <- x$clevel * 3.28; x})

  names(ls_par) <- wq_sites

  ## identify parameters
  parm <- unique(names(ls_par[[1]]))
  parm <- subset(parm, !(parm %in% c('datetimestamp')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  summary <- dat_tidy %>%
    dplyr::group_by(!! event_, !! evt_start_, !! evt_end_, !! parameter_, !! station_) %>%
    tidyr::drop_na(!! result_) %>%
    dplyr::summarise(min = min(!! result_, na.rm = T)
              , max = max(!! result_, na.rm = T)
              , mean = mean(!! result_, na.rm = T)
              , median = stats::median(!! result_, na.rm = T))

  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # add factors for spatial filtering
  summary$station_fac <- factor(summary$station, levels = wq_sites)

  # re-sort the table using factors
  summary <- summary %>% dplyr::arrange(., !! parameter_, !! station_fac_)

  # write table
  tbl_ttl <- paste('output/wq/data_one_event_multi_reserve_table/data_table_wq_', storm_nm, '_multireserve.csv', sep = '')
  utils::write.csv(summary, file = tbl_ttl, quote = F, row.names = F)

  ########## Meteorological #####################################################

  # ----------------------------------------------
  # Load meteorological data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

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

  dat_tidy <- dat_tidy %>% dplyr::filter(!! parameter_ %in% parm)

  # ----------------------------------------------
  # Single Event Comparison, Multiple Reserves ---
  # ----------------------------------------------

  total_nalist <- c("atemp", "bp", "intensprcp", "maxwspd", "rh", "sdwdir", "wdir", "wspd")

  summary <- dat_tidy %>%
    dplyr::group_by(!! event_, !! evt_start_, !! evt_end_, !! parameter_, !! station_) %>%
    tidyr::drop_na(!! result_) %>%
    dplyr::summarise(min = min(!! result_, na.rm = T)
              , max = max(!! result_, na.rm = T)
              , mean = mean(!! result_, na.rm = T)
              , median = stats::median(!! result_, na.rm = T)
              , total = sum(!! result_, na.rm = T)) %>%
    dplyr::mutate(total = dplyr::case_when(!! parameter_ %in% total_nalist == FALSE ~ total))


  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # add factors for spatial filtering
  summary$station_fac <- factor(summary$station, levels = met_sites)

  # re-sort the table using factors
  summary <- summary %>% dplyr::arrange(., !! parameter_, !! station_fac_)

  # write table
  tbl_ttl <- paste('output/met/data_one_event_multi_reserve_table/data_table_met_', storm_nm, '_multireserve.csv', sep = '')
  utils::write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


}


