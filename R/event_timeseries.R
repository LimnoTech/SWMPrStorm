#' event_timeseries
#'
#' @param var_in .xlsx with all required input variables defined (string).
#' @param data_path pathway to cdmo data folder (string).
#' @param storm_start YYYY-MM-DD HH:MM:SS start of storm event (string).
#' @param storm_end YYYY-MM-DD HH:MM:SS end of storm event (string).
#' @param view_start YYYY-MM-DD HH:MM:SS first datetime of data to plot (string).
#' @param view_end YYYY-MM-DD HH:MM:SS last datetime of data to plot (string).
#' @param recovery_start YYYY-MM-DD HH:MM:SS start of storm recovery period (string).
#' @param recovery_end YYYY-MM-DD HH:MM:SS end of storm recovery period (string).
#' @param stn_wq station code to analyze. if blank all active stations within reserve will be analyzed (string)
#' @param keep_flags comma separated list of data quality flags that should be kept (string).
#' @param reserve 3 digit reserve code (string).
#' @param skip TRUE/FALSE. If TRUE, function will be skipped (string).
#' @param user_units User defined units. Set to "English" or "SI". Default CDMO data is in SI units. Not all parameters have common equivalent English units (e.g. concentrations), and therefore not all will be converted.
#'
#' @return plots are generated and saved in /output/wq/timeseries_event_recovery/ and /output/met/timeseries_event_recovery/
#' @export
#'
#' @examples
#'
#' \dontrun{
#' #StormVariables.xlsx is a template variable input file saved in data/
#' vars_in <- 'data/StormTrackVariables.xlsx'
#' single_storm_track(var_in = vars_in)
#' }
event_timeseries <- function(var_in,
                             data_path = NULL,
                             reserve = NULL,
                             storm_start = NULL,
                             storm_end = NULL,
                             view_start = NULL,
                             view_end = NULL,
                             recovery_start = NULL,
                             recovery_end = NULL,
                             stn_wq = NULL,
                             keep_flags = NULL,
                             skip = NULL,
                             user_units = NULL) {

  # ----------------------------------------------------------------------------
  # Define global variables
  # ----------------------------------------------------------------------------
  NERR.Site.ID_ <- rlang::sym('NERR.Site.ID')
  Status_ <- rlang::sym('Status')
  Station.Type_ <- rlang::sym('Station.Type')
  Reserve.Name_ <- rlang::sym('Reserve.Name')

  parameter_ <- rlang::sym('parameter')
  value_ <- rlang::sym('value')

  datetimestamp_ <- rlang::sym('datetimestamp')
  datetimestamp_day_ <- rlang::sym('datetimestamp_day')
  xmin_ <- rlang::sym('xmin')
  ymin_ <- rlang::sym('ymin')
  xmax_ <- rlang::sym('xmax')
  ymax_ <- rlang::sym('ymax')
  years_ <- rlang::sym('years')



  # ----------------------------------------------------------------------------
  # Read in Data
  # ----------------------------------------------------------------------------

  #a.  Read in the variable input template, var_in

  input_Parameters <- openxlsx::read.xlsx(var_in, sheet = "timeseries_recovery")
  input_Master <- openxlsx::read.xlsx(var_in, sheet = "MASTER")

  #b.  Read the following variables from template spreadsheet if not provided as optional arguments

  # read in all available stations based on reserve (reserve is a required field)
  if(is.null(reserve)) reserve <- input_Master[1,2]


  stations <- get('sampling_stations') %>%
    dplyr::filter(!! NERR.Site.ID_ == reserve) %>%
    dplyr::filter(!! Status_ == "Active")

  wq_stations <- stations %>%
    dplyr::filter(!! Station.Type_ == 1)

  if(is.null(storm_start)) storm_start <- input_Parameters[1,2]
  if(is.null(storm_end)) storm_end <- input_Parameters[2,2]
  if(is.null(view_start)) view_start <- input_Parameters[3,2]
  if(is.null(view_end)) view_end <- input_Parameters[4,2]
  if(is.null(recovery_start)) recovery_start <- input_Parameters[5,2]
  if(is.null(recovery_end)) recovery_end <- input_Parameters[6,2]
  #if(is.null(stn_wq)) stn_wq <- input_Parameters[7,2]
  if(is.null(stn_wq)) stn_wq <- if(is.na(input_Parameters[7,2])) {wq_stations$Station.Code} else {input_Parameters[7,2]}
  if(is.null(keep_flags)) keep_flags <- unlist(strsplit(input_Parameters[8,2],", "))
  if(is.null(skip)) skip <- input_Parameters[9,2]
  if(is.null(user_units)) user_units <- input_Parameters[10,2]
  if(is.null(data_path)) data_path <- 'data/cdmo'



  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping event_timeseries"))}



  ### 1. Read in data and wrangle ##############################################
  ls_par <- lapply(stn_wq, SWMPr::import_local, path = data_path)
  ls_par <- lapply(ls_par, SWMPr::qaqc, qaqc_keep = keep_flags)
  names(ls_par) <- stn_wq


  ## convert dataset to user defined units (if "SI", no conversion will take place)
  ls_par <- SWMPrStorm::convert_units(ls_par, user_units)

  ## tidy data

  ls_wq <- lapply(ls_par, tidyr::pivot_longer,cols = 2:13, names_to = 'parameter',values_to = 'value')

  parms <- unique(ls_wq[[1]]$parameter)



  ### Plot Data ################################################################

  for(i in 1:length(ls_wq)) {


    dat <- ls_wq[[i]]
    stn <- names(ls_wq)[i]

    df <- dat %>%
      dplyr::filter(dplyr::between(!! datetimestamp_, as.POSIXct(view_start), as.POSIXct(view_end)))

    parm_wq <- parms


    # one station, daily smooth, with recovery -----------------------------------

    plot_start <- as.POSIXct(view_start, format = "%Y-%m-%d %H:%M:%S")
    plot_end <- as.POSIXct(view_end, format = "%Y-%m-%d %H:%M:%S")

    df_day <- df %>%
      dplyr::filter(dplyr::between(!! datetimestamp_
                     , as.POSIXct(view_start)
                     , as.POSIXct(view_end))) %>%
      dplyr::mutate(datetimestamp_day = lubridate::floor_date(!! datetimestamp_, unit = 'day')) %>%
      dplyr::group_by(!! datetimestamp_day_, !! parameter_) %>%
      dplyr::summarize("value" = mean(!! value_, na.rm = T))

    for(i in 1:length(parms)){
      parm = parms[i]

      # Create a dummy data.frame for events and recovery
      df<-data.frame("xmin"=as.POSIXct(c(storm_start, recovery_start)),
                     "xmax"=as.POSIXct(c(storm_end, recovery_end)),
                     "ymin"=c(-Inf, -Inf),
                     "ymax"=c(Inf, Inf),
                     "years"=c('Event Duration', 'Event Recovery'))

      x <-
        df_day %>%
        dplyr::filter(!! parameter_ == parm)

      x <-
        ggplot2::ggplot(x, ggplot2::aes(x = !! datetimestamp_day_, y = !! value_)) +
        ggplot2::ggtitle(SWMPrExtension::title_labeler(nerr_site_id = stn_wq)) +
        ggplot2::geom_line(ggplot2::aes(color = 'Daily Avg'), lwd = 1) +
        ggplot2::geom_rect(data=df,ggplot2::aes(xmin=!! xmin_,ymin=!! ymin_,xmax=!! xmax_,ymax=!! ymax_,fill=!! years_),
                  alpha=0.1,inherit.aes=FALSE) +
        ggplot2::labs(x = '', y = SWMPrStorm::y_axis_unit_labeler(parm, user_units))  #REPLACE WTIH NEW UNIT SCHEME


      x <-
        x +
        ggplot2::scale_color_manual('', values = c('steelblue3')) +
        ggplot2::scale_fill_manual('', values = c('steelblue3', 'green')) +
        ggplot2::scale_x_datetime(date_breaks = '1 day',
                                  date_labels = '%b\n%d',
                                  guide = ggplot2::guide_axis(check.overlap = TRUE))

      x <- x +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                       strip.background = ggplot2::element_blank(),
                       panel.grid = ggplot2::element_blank(),
                       panel.border = ggplot2::element_rect(color = 'black', fill = NA),
                       plot.margin = ggplot2::margin(5.5, 24, 5.5, 5.5, unit = 'pt'),
                       axis.title.y = ggplot2::element_text(margin = ggplot2::unit(c(0, 8, 0, 8), 'pt'), angle = 90),
                       text = ggplot2::element_text(size = 16),
                       legend.position = 'top')


      x_ttl <- paste('output/wq/timeseries_event_recovery/timeseries_event_recovery_', stn, '_', parm, '.png', sep = '')

      ggplot2::ggsave(filename = x_ttl, plot = x, height = 4, width = 6, units = 'in', dpi = 300)

    }
  }
}
