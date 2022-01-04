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
event_ridgeline <- function(var_in,
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

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "ridgeline")


  if(is.null(storm_nm)) storm_nm <- input_Parameters[1,2]
  if(is.null(storm_start)) storm_start <- input_Parameters[2,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[3,2]
  if(is.null(wq_sites)) wq_sites <- unlist(strsplit(input_Parameters[4,2],", "))
  if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[5,2],", "))
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[6,2],", "))
  if(is.null(data_path)) data_path <- 'data/cdmo'


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

  ## identify parameters, remove a few
  #parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  #parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end


  # add reserve name
  station_list <-sampling_stations
  add_reserve <- station_list %>% dplyr::select(station = Station.Code, Reserve.Name)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
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
  # Ridgeline plot                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(parameter == param)

    plt_ttl <- paste('output/wq/ridgeline_one_reserve_one_event/ridgeline_', '_', param, '.png', sep = '')

    p1 <- ggplot2::ggplot(df, ggplot2::aes(x=datetimestamp, y = Reserve.Name,
                                     height = result, group = Reserve.Name)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      ggridges::theme_ridges() +
      ggplot2::ylab(SWMPrStorm::y_labeler(param)) +
      ggplot2::xlab("Date") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
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
      dplyr::filter(parameter == param)
    df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')

    df_smooth <- df %>%
      dplyr::group_by(station, Reserve.Name, parameter, datetime_floor) %>%
      dplyr::summarise(avg = mean(result, na.rm = T))


    plt_ttl <- paste('output/wq/ridgeline_one_reserve_one_event/ridgeline_', '_', param, '_smoothed.png', sep = '')

    p2 <- ggplot2::ggplot(df_smooth, ggplot2::aes(x=datetime_floor, y = Reserve.Name,
                                     height = avg, group = Reserve.Name)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      ggridges::theme_ridges() +
      ggplot2::ylab(SWMPrStorm::y_labeler(param)) +
      ggplot2::xlab("Date") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
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
  parm <- unique(names(ls_par[[1]])) %>% subset(!(. %in% c('datetimestamp')))
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end


  # add reserve name
  station_list <-sampling_stations
  add_reserve <- station_list %>% dplyr::select(station = Station.Code, Reserve.Name)
  dat_tidy <- dplyr::left_join(dat_tidy, add_reserve)

  # assign factors for plotting
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
  # Ridgeline plot                             ---
  # ----------------------------------------------


  params <- levels(as.factor(dat_tidy$parameter))


  for(param in params) {

    df <- dat_tidy %>%
      dplyr::filter(parameter == param)

    plt_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', '_', param, '.png', sep = '')

    p3 <- ggplot2::ggplot(df, ggplot2::aes(x=datetimestamp, y = Reserve.Name,
                                     height = result, group = Reserve.Name)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      ggridges::theme_ridges() +
      ggplot2::ylab(SWMPrStorm::y_labeler(param)) +
      ggplot2::xlab("Date") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
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
      dplyr::filter(parameter == param)
    df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')

    df_smooth <- df %>%
      dplyr::group_by(station, Reserve.Name, parameter, datetime_floor) %>%
      dplyr::summarise(avg = mean(result, na.rm = T))


    plt_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', '_', param, '_smoothed.png', sep = '')

    p4 <- ggplot2::ggplot(df_smooth, ggplot2::aes(x=datetime_floor, y = Reserve.Name,
                                            height = avg, group = Reserve.Name)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1, fill = "lightblue") +
      ggridges::theme_ridges() +
      ggplot2::ylab(SWMPrStorm::y_labeler(param)) +
      ggplot2::xlab("Date") +
      ggplot2::scale_x_datetime(expand = c(0, 0)) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                     axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 16, 0, 0), 'pt'), angle = 90),
                     text = ggplot2::element_text(size = 16),
                     plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt'),
                     legend.position = 'top')

      ggplot2::ggsave(filename = plt_ttl, plot = p4, height=8, width =8)


  }




}
