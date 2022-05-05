#' event_ridgeline
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
#'
#' @return plots are generated and saved in /output/wq/ridgelines/ and /output/met/ridgelines/
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #StormVariables.xlsx is a template variable input file saved in data/
#' vars_in <- 'data/StormTrackVariables.xlsx'
#' single_storm_track(var_in = vars_in)
#' }
event_ridgeline <- function(var_in,
                            data_path,
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


  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------


  #a.  Read in the variable input template, var_in

  input_Parameters <- openxlsx::read.xlsx(var_in, sheet = "ridgeline")
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
  if(is.null(wq_sites)) wq_sites <- if(is.na(input_Parameters[4,2])) {wq_stations$Station.Code[1]} else {unlist(strsplit(input_Parameters[4,2],", "))}
  #if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[5,2],", ")),
  if(is.null(met_sites)) met_sites <- if(is.na(input_Parameters[5,2])) {met_stations$Station.Code[1]} else {unlist(strsplit(input_Parameters[5,2],", "))}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[6,2],", "))
  if(is.null(skip)) skip <- input_Parameters[7,2]
  if(is.null(user_units)) user_units <- input_Parameters[8,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'


  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping event_ridgeline"))}



  ########## WATER QUALITY #####################################################

  # ----------------------------------------------
  # Load water quality data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'wq'

  ls_par <- lapply(wq_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

  names(ls_par) <- wq_sites

  ## convert dataset to user defined units (if "SI", no conversion will take place)
  ls_par <- SWMPrStorm::convert_units(ls_par, user_units)


  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- tidyr::pivot_longer(dat, 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end


  # add reserve name
  station_list <- get('sampling_stations')
  add_reserve <- station_list %>% dplyr::select("station" = !! Station.Code_, !! Reserve.Name_)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
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
  # Ridgeline plot                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(!! parameter_ == param)

    plt_ttl <- paste('output/wq/ridgelines/ridgeline_', '_', param, '.png', sep = '')

    p1 <- ggplot2::ggplot(df, ggplot2::aes(x=!! datetimestamp_, y = !! Reserve.Name_,
                                     height = !! result_, group = !! Reserve.Name_)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      ggplot2::ggtitle(SWMPrStorm::y_axis_unit_labeler(param, user_units)) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::unit(c(0,0,16,0),'pt')),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt'),
                     legend.position = 'top')

      ggplot2::ggsave(filename = plt_ttl, plot = p1, height=8, width =8)


  }


  # ----------------------------------------------
  # Ridgeline plot, hourly smoothing                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(!! parameter_ == param)
    df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')

    df_smooth <- df %>%
      dplyr::group_by(!! station_, !! Reserve.Name_, !! parameter_, !! datetime_floor_) %>%
      dplyr::summarise(avg = mean(!! result_, na.rm = T))


    plt_ttl <- paste('output/wq/ridgelines/ridgeline_', '_', param, '_smoothed.png', sep = '')

    p2 <- ggplot2::ggplot(df_smooth, ggplot2::aes(x=!! datetime_floor_, y = !! Reserve.Name_,
                                     height = !! avg_, group = !! Reserve.Name_)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      #ggridges::theme_ridges() +
      ggplot2::ggtitle(SWMPrStorm::y_axis_unit_labeler(param, user_units)) + #REPLACE WITH NEW UNIT SCHEME
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::unit(c(0,0,16,0),'pt')),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt'),
                     legend.position = 'top')

      ggplot2::ggsave(filename = plt_ttl, plot = p2, height = 8, width = 8)


  }



  # ----------------------------------------------
  # Load meteorological data ----------------------
  # ----------------------------------------------

  ## load, clean, and filter data
  data_type <- 'met'

  ls_par <- lapply(met_sites, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  ls_par <- lapply(ls_par, subset, subset = c(storm_start, storm_end))

  names(ls_par) <- met_sites


  ## identify parameters, remove a few
  parm <- unique(names(ls_par[[1]]))
  parm <- subset(parm, !(parm %in% c('datetimestamp', 'wdir', 'sdwdir', 'totpar', 'totsorad')))

  ## convert dataset to user defined units (if "SI", no conversion will take place)
  ls_par <- SWMPrStorm::convert_units(ls_par, user_units)


  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- tidyr::pivot_longer(dat, 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end


  # add reserve name
  station_list <- get('sampling_stations')
  add_reserve <- station_list %>% dplyr::select("station" = !! Station.Code_, !! Reserve.Name_)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
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
  # Ridgeline plot                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(!! parameter_ == param)

    plt_ttl <- paste('output/met/ridgelines/ridgeline_', '_', param, '.png', sep = '')

    p3 <- ggplot2::ggplot(df, ggplot2::aes(x=!! datetimestamp_, y = !! Reserve.Name_,
                                     height = !! result_, group = !! Reserve.Name_)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      #ggridges::theme_ridges() +
      ggplot2::ggtitle(SWMPrStorm::y_axis_unit_labeler(param, user_units)) + #REPLACE WITH NEW UNIT SCHEME
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::unit(c(0,0,16,0),'pt')),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt'),
                     legend.position = 'top')

      ggplot2::ggsave(filename = plt_ttl, plot = p3, height=8, width =8)


  }


  # ----------------------------------------------
  # Ridgeline plot, hourly smoothing                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(!! parameter_ == param)
    df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')

    df_smooth <- df %>%
      dplyr::group_by(!! station_, !! Reserve.Name_, !! parameter_, !! datetime_floor_) %>%
      dplyr::summarise(avg = mean(!! result_, na.rm = T))


    plt_ttl <- paste('output/met/ridgelines/ridgeline_', '_', param, '_smoothed.png', sep = '')

    p4 <- ggplot2::ggplot(df_smooth, ggplot2::aes(x=!! datetime_floor_, y = !! Reserve.Name_,
                                            height = !! avg_, group = !! Reserve.Name_)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      #ggridges::theme_ridges() +
      ggplot2::ggtitle(SWMPrStorm::y_axis_unit_labeler(param, user_units)) +
      ggplot2::ylab("") +
      ggplot2::xlab("") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, margin = ggplot2::unit(c(0,0,16,0),'pt')),
                     panel.background = ggplot2::element_rect(fill = "white"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.x = ggplot2::element_line(color = "grey90"),
                     axis.ticks.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt'),
                     legend.position = 'top')

      ggplot2::ggsave(filename = plt_ttl, plot = p4, height=8, width =8)


  }




}
