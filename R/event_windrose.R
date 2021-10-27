event_windrose <- function() {

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


  # ----------------------------------------------
  # Wind rose                                  ---
  # ----------------------------------------------
  tmp <- ls_par[[1]]
  angle = 45
  width = 1.5
  breaks = 10
  paddle = FALSE
  grid.line = 20
  max.freq = 90
  cols = 'GnBu'
  annotate = FALSE
  main = NULL
  type = 'default'
  between = list(x = 1, y = 1)
  par.settings = NULL
  strip = NULL

  tmp$date_char <- as.character(as.Date(tmp$datetimestamp))

  openair::windRose(tmp, ws = 'wspd', wd = 'wdir', #type = 'date_char',
                    angle = angle,
                    width = width,
                    breaks = breaks,
                    paddle = paddle,
                    grid.line = grid.line,
                    max.freq = 60, #max.freq,
                    cols = cols,
                    annotate = annotate,
                    main = main,
                    # type = type,
                    between = between,
                    par.settings = par.settings,
                    strip = strip)


  openair::windRose(tmp, ws = 'wspd', wd = 'wdir', type = 'date_char',
                    angle = angle,
                    width = width,
                    breaks = breaks,
                    paddle = paddle,
                    grid.line = grid.line,
                    max.freq = max.freq,
                    cols = cols,
                    annotate = annotate,
                    main = main,
                    # type = type,
                    between = between,
                    par.settings = par.settings,
                    strip = strip)



}
