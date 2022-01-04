#' event_windrose
#'
#' @param var_in
#' @param data_path
#' @param storm_start
#' @param storm_end
#' @param met_sites
#' @param keep_flags
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
event_windrose <- function(var_in,
                           data_path = NULL,
                           storm_start = NULL,
                           storm_end = NULL,
                           met_sites = NULL,
                           keep_flags = NULL,
                           ...) {



  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(var_in, sheetName = "windrose")

  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  if(is.null(storm_start)) storm_start <- input_Parameters[1,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[2,2]
  if(is.null(met_sites)) met_sites <- unlist(strsplit(input_Parameters[3,2],", "))
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[4,2],", "))
  if(is.null(data_path)) data_path <- 'data/cdmo'



  # ----------------------------------------------
  # Load water quality data ----------------------
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


  # ----------------------------------------------
  # Wind rose                                  ---
  # ----------------------------------------------

  angle = 45
  width = 1.5
  breaks = 8
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


  for (i in 1:length(names(ls_par))) {
  tmp <- ls_par[[i]]
  tmp$date_char <- as.character(as.Date(tmp$datetimestamp))


  plt_ttl <- paste0("output/met/windrose/",attributes(tmp)$station, "_wspd.png")
  png(plt_ttl, width = 1000, height = 1000)
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
  dev.off()


  plt_ttl <- paste0("output/met/windrose/",attributes(tmp)$station, "_wspd_bydate.png")
  png(plt_ttl, width = 1000, height = 1000)
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
  dev.off()

  }

}
