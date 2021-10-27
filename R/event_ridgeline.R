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
  # Ridgeline plot                             ---
  # ----------------------------------------------
  library(ggridges)

  # spcond
  df <- dat_tidy %>% filter(parameter == 'spcond')
  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1) +
    theme_ridges()

  # wind speed
  df <- dat_tidy %>% filter(parameter == 'cdepth')
  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1)

  # max wind speed
  param <- 'sal'
  df <- dat_tidy %>% filter(parameter == param)

  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1)


  df2 <- dat_tidy %>% filter(parameter == param) %>%
    group_by(time_hr = floor_date(datetimestamp, "hour"), Reserve.Name) %>%
    summarise(result = mean(result, na.rm = TRUE))

  ggplot(df2, aes(x = time_hr, y = Reserve.Name
                  , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1)

  # # max wind speed
  # ## multiple reserves
  # df <- dat_tidy %>% filter(parameter == 'maxwspd')
  # x <- ggplot(df, aes(x = datetimestamp, y = result, group = station)) +
  #   geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  #   geom_area(fill="lightblue") +
  #   theme_bw()



  # wind speed
  ## Just GTM
  df <- dat_tidy %>% filter(station == 'gtmpcmet', parameter == 'wspd')
  df$datetime_floor <- floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    group_by(station, parameter, datetime_floor) %>%
    summarise(avg = mean(result, na.rm = T))

  x <- ggplot(df_smooth, aes(x = datetime_floor, y = avg)) +
    geom_area(fill="lightblue") +
    theme_bw()

  x <-
    x +
    ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    labs(x = '', y = 'Wind Speed (mph)') +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    theme(text = element_text(size = 16)) +
    theme(plot.margin = unit(c(0, 20, 0, 0), 'pt')) +
    theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_wspd.png', sep = '')

  ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)




  # precip
  ## Just GTM
  df <- dat_tidy %>% filter(station == 'gtmpcmet', parameter == 'totprcp')
  df$datetime_floor <- floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    group_by(station, parameter, datetime_floor) %>%
    summarise(avg = sum(result, na.rm = T))

  x <- ggplot(df_smooth, aes(x = datetime_floor, y = avg)) +
    geom_area(fill="lightblue") +
    theme_bw()

  x <-
    x +
    ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    labs(x = '', y = 'Precipitation (in)') +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    theme(text = element_text(size = 16)) +
    theme(plot.margin = unit(c(0, 20, 0, 0), 'pt')) +
    theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_totprcp.png', sep = '')

  ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)


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
  # Ridgeline plot                             ---
  # ----------------------------------------------
  library(ggridges)

  # total precipitation
  df <- dat_tidy %>% filter(parameter == 'totprcp')
  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1) +
    theme_ridges()

  # wind speed
  df <- dat_tidy %>% filter(parameter == 'wspd')
  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1)

  # max wind speed
  df <- dat_tidy %>% filter(parameter == 'maxwspd')
  ggplot(df, aes(x = datetimestamp, y = Reserve.Name
                 , height = result, group = Reserve.Name)) +
    geom_density_ridges(stat = "identity", scale = 1)

  # # max wind speed
  # ## multiple reserves
  # df <- dat_tidy %>% filter(parameter == 'maxwspd')
  # x <- ggplot(df, aes(x = datetimestamp, y = result, group = station)) +
  #   geom_density_ridges(scale = 10, size = 0.25, rel_min_height = 0.03) +
  #   geom_area(fill="lightblue") +
  #   theme_bw()



  # wind speed
  ## Just GTM
  df <- dat_tidy %>% filter(station == 'gtmpcmet', parameter == 'wspd')
  df$datetime_floor <- floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    group_by(station, parameter, datetime_floor) %>%
    summarise(avg = mean(result, na.rm = T))

  x <- ggplot(df_smooth, aes(x = datetime_floor, y = avg)) +
    geom_area(fill="lightblue") +
    theme_bw()

  x <-
    x +
    ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    labs(x = '', y = 'Wind Speed (mph)') +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    theme(text = element_text(size = 16)) +
    theme(plot.margin = unit(c(0, 20, 0, 0), 'pt')) +
    theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_wspd.png', sep = '')

  ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)




  # precip
  ## Just GTM
  df <- dat_tidy %>% filter(station == 'gtmpcmet', parameter == 'totprcp')
  df$datetime_floor <- floor_date(df$datetimestamp, unit = 'hour')
  df_smooth <- df %>%
    group_by(station, parameter, datetime_floor) %>%
    summarise(avg = sum(result, na.rm = T))

  x <- ggplot(df_smooth, aes(x = datetime_floor, y = avg)) +
    geom_area(fill="lightblue") +
    theme_bw()

  x <-
    x +
    ggtitle(SWMPrExtension::title_labeler(nerr_site_id = met_sites[1])) +
    # scale_x_datetime(date_breaks = 'day', date_labels = '%b %d') +
    labs(x = '', y = 'Precipitation (in)') +
    scale_x_datetime(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = unit(c(0, 8, 0, 0), 'pt'), angle = 90)) +
    theme(text = element_text(size = 16)) +
    theme(plot.margin = unit(c(0, 20, 0, 0), 'pt')) +
    theme(legend.position = 'top')

  x_ttl <- paste('output/met/ridgeline_one_reserve_one_event/ridgeline_', met_sites[1], '_totprcp.png', sep = '')

  ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)



}
