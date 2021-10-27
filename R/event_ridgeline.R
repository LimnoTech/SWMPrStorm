event_ridgeline <- function() {


  ########## WATER QUALITY #####################################################
  #a few notes -------------------
    ## Kim Cressman's 2017 eclipse plot: https://github.com/swmpkim/2017_eclipse_viz/blob/master/Eclipse_visualization.Rmd
    # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-10-05 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-10 00:00:00' # '2016-10-10 00:00:00'

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
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% dplyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
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
  # Ridgeline plot                             ---
  # ----------------------------------------------

  # spcond
  df <- dat_tidy %>%
    dplyr::filter(parameter == 'spcond')

    ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
      ggridges::geom_density_ridges(stat = "identity", scale = 1) +
      ggridges::theme_ridges()

  # wind speed
  df <- dat_tidy %>%
    dplyr::filter(parameter == 'cdepth')

  ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1)

  # max wind speed
  param <- 'sal'
  df <- dat_tidy %>%
    dplyr::filter(parameter == param)

  ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1)


  df2 <- dat_tidy %>%
    dplyr::filter(parameter == param) %>%
    dplyr::group_by(time_hr = floor_date(datetimestamp, "hour"), Reserve.Name) %>%
    dplyr::summarise(result = mean(result, na.rm = TRUE))

  ggplot2::ggplot(df2, ggplot2::aes(x = time_hr, y = Reserve.Name
                  , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1)

  # # max wind speed
  # ## multiple reserves
  # df <- dat_tidy %>% filter(parameter == 'maxwspd')
  # x <- ggplot(df, aes(x = datetimestamp, y = result, group = station)) +
  #   geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  #   geom_area(fill="lightblue") +
  #   theme_bw()


  # wind speed
  ## Just GTM
  df <- dat_tidy %>%
    dplyr::filter(station == 'gtmpcmet', parameter == 'wspd')
  df$datetime_floor <- lubrdiate::floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    dplyr::group_by(station, parameter, datetime_floor) %>%
    dplyr::summarise(avg = mean(result, na.rm = T))

  x <- ggplot2::ggplot(df_smooth, ggplot2::aes(x = datetime_floor, y = avg)) +
    ggplot2::geom_area(fill="lightblue") +
    ggplot2::theme_bw()

  #can clean up code below to only call ggplot2::theme() once with multiple arguments.
  x <-
    x +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    ggplot2::labs(x = '', y = 'Wind Speed (mph)') +
    ggplot2::scale_x_datetime(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    ggplot2::theme(text = ggplot2::element_text(size = 16)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt')) +
    ggplot2::theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_wspd.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)




  # precip
  ## Just GTM
  df <- dat_tidy %>% dplyr::
    filter(station == 'gtmpcmet', parameter == 'totprcp')
  df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    dplyr::group_by(station, parameter, datetime_floor) %>%
    dplyr::summarise(avg = sum(result, na.rm = T))

  x <- ggplot2::ggplot(df_smooth, ggplot2::aes(x = datetime_floor, y = avg)) +
    ggplot2::geom_area(fill="lightblue") +
    ggplot2::theme_bw()

  x <-
    x +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    ggplot2::labs(x = '', y = 'Precipitation (in)') +
    ggplot2::scale_x_datetime(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    ggplot2::theme(text = ggplot2::element_text(size = 16)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt')) +
    ggplot2::theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_totprcp.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)


  ########## Meteorological #####################################################

  # a few notes -------------------
  ## Kim Cressman's 2017 eclipse plot: https://github.com/swmpkim/2017_eclipse_viz/blob/master/Eclipse_visualization.Rmd
  # Define storm ------------------
  # Matthew
  storm_nm <- 'Matthew'
  storm_start <- '2016-10-05 00:00:00' # '2016-9-28 00:00:00'
  storm_end <- '2016-10-10 00:00:00' # '2016-10-10 00:00:00'

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
  parm <- parm %>%  subset(!(. %in% c('wdir', 'sdwdir', 'totpar', 'totsorad')))

  # combine data.frames into one and tidy
  dat <- dplyr::bind_rows(ls_par, .id = 'station')
  dat_tidy <- dat %>% tidyr::pivot_longer(., 3:length(names(dat)), names_to = 'parameter', values_to = 'result')
  dat_tidy$event <- storm_nm
  dat_tidy$evt_start <- storm_start
  dat_tidy$evt_end <- storm_end

  dat_tidy <- dat_tidy %>%
    dplyr::filter(parameter %in% parm)

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
  # Ridgeline plot                             ---
  # ----------------------------------------------

  # total precipitation
  df <- dat_tidy %>%
    dplyr::filter(parameter == 'totprcp')

  ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1) +
    ggridges::theme_ridges()

  # wind speed
  df <- dat_tidy %>%
    dplyr::filter(parameter == 'wspd')

  ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1)

  # max wind speed
  df <- dat_tidy %>%
    dplyr::filter(parameter == 'maxwspd')

  ggplot2::ggplot(df, ggplot2::aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    ggridges::geom_density_ridges(stat = "identity", scale = 1)

  # # max wind speed
  # ## multiple reserves
  # df <- dat_tidy %>% filter(parameter == 'maxwspd')
  # x <- ggplot(df, aes(x = datetimestamp, y = result, group = station)) +
  #   geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  #   geom_area(fill="lightblue") +
  #   theme_bw()



  # wind speed
  ## Just GTM
  df <- dat_tidy %>%
    dplyr::filter(station == 'gtmpcmet', parameter == 'wspd')
  df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    dplyr::group_by(station, parameter, datetime_floor) %>%
    dplyr::summarise(avg = mean(result, na.rm = T))

  x <- ggplot2::ggplot(df_smooth, ggplot2::aes(x = datetime_floor, y = avg)) +
    ggplot2::geom_area(fill="lightblue") +
    ggplot2::theme_bw()

  x <-
    x +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    ggplot2::labs(x = '', y = 'Wind Speed (mph)') +
    ggplot2::scale_x_datetime(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    ggplot2::theme(text = ggplot2::element_text(size = 16)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt')) +
    ggplot2::theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_wspd.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)




  # precip
  ## Just GTM
  df <- dat_tidy %>%
    dplyr::filter(station == 'gtmpcmet', parameter == 'totprcp')
  df$datetime_floor <- lubridate::floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    dplyr::group_by(station, parameter, datetime_floor) %>%
    dplyr::summarise(avg = sum(result, na.rm = T))

  x <- ggplot2::ggplot(df_smooth, ggplot2::aes(x = datetime_floor, y = avg)) +
    ggplot2::geom_area(fill="lightblue") +
    ggplot2::theme_bw()

  x <-
    x +
    ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    ggplot2::labs(x = '', y = 'Precipitation (in)') +
    ggplot2::scale_x_datetime(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    ggplot2::theme(text = ggplot2::element_text(size = 16)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0, 20, 0, 0), 'pt')) +
    ggplot2::theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_totprcp.png', sep = '')

  ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)



}
