#' summary_data_table
#'
#' @param var_in
#' @param data_path
#' @param storm_nm
#' @param storm_start
#' @param storm_end
#' @param reserve
#' @param keep_flags
#' @param skip
#'
#' @return
#' @export
#'
#' @examples
summary_data_table <- function(var_in,
                               data_path,
                               storm_nm = NULL,
                               storm_start = NULL,
                               storm_end = NULL,
                               reserve = NULL,
                               keep_flags = NULL,
                               skip = NULL) {




  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "table_summary")
  input_Master <- xlsx::read.xlsx(var_in, sheetName = "MASTER")



  if(is.null(storm_nm)) storm_nm <- unlist(strsplit(input_Parameters[1,2],", "))
  if(is.null(storm_start)) storm_start <- unlist(strsplit(input_Parameters[2,2],", "))
  if(is.null(storm_end)) storm_end <- unlist(strsplit(input_Parameters[3,2],", "))
  if(is.null(reserve)) reserve <- if(is.na(input_Parameters[4,2])) {input_Master[1,2]} else { unlist(strsplit(input_Parameters[4,2],", "))}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[5,2],", "))
  if(is.null(skip)) skip <- input_Parameters[6,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'

  stations <- sampling_stations %>%
    filter(NERR.Site.ID == reserve) %>%
    filter(Status == "Active")

  wq_sites <- stations %>%
    filter(Station.Type == 1)
  wq_sites <- wq_sites$Station.Code

  met_sites <- stations %>%
    filter(Station.Type == 0)
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
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

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
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end


  # -------------------------------------------------------------------
  #  Event Comparison, All Active Stations within Selected Reserves ---
  # -------------------------------------------------------------------

  summary <- dat_tidy %>%
    dplyr::group_by(event, evt_start, evt_end, parameter, station) %>%
    tidyr::drop_na(result) %>%
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
  tbl_ttl <- paste('output/wq/data_table/summary_data_table_wq_', paste(reserve, collapse = "_"), ".csv", sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)


  ########## Meteorological #####################################################

  # ----------------------------------------------
  # Load meteorological data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, qaqc, qaqc_keep = keep_flags)
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

  dat_tidy <- dat_tidy %>% dplyr::filter(parameter %in% parm)

  # ---------------------------------------------------------------
  #  Event Comparison, All Active Stations at Selected Reserves ---
  # ---------------------------------------------------------------

  total_nalist <- c("atemp", "bp", "intensprcp", "maxwspd", "rh", "sdwdir", "wdir", "wspd")

  summary <- dat_tidy %>%
    dplyr::group_by(event, evt_start, evt_end, parameter, station) %>%
    tidyr::drop_na(result) %>%
    dplyr::summarise(min = min(result, na.rm = T)
                     , max = max(result, na.rm = T)
                     , mean = mean(result, na.rm = T)
                     , median = median(result, na.rm = T)
                     , total = sum(result, na.rm = T)) %>%
    dplyr::mutate(total = dplyr::case_when(parameter %in% total_nalist == FALSE ~ total))


  # add readable station names
  summary$station_name <- sapply(summary$station, SWMPrExtension::title_labeler)

  # add factors for spatial filtering
  summary$station_fac <- factor(summary$station, levels = met_sites)

  # re-sort the table using factors
  summary <- summary %>% dplyr::arrange(., parameter, station_fac)

  # write table
  tbl_ttl <- paste('output/met/data_table/summary_data_table_met_', paste(reserve, collapse = "_"), ".csv", sep = '')
  write.csv(summary, file = tbl_ttl, quote = F, row.names = F)






}
