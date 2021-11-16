#' Generate y-axis Label Based on SWMP Parameter Abbreviation
#'
#' Generate a y-axis label based on SWMP parameter abbreviation
#'
#' @param param chr string of variable abbreviation
#' @param converted logical, should the parameter label units be converted from metric to english? Defaults to \code{FALSE}. Currently available for \code{temp}, \code{depth}, \code{cdepth}, \code{level}, \code{clevel}, \code{atemp}, \code{wspd}, \code{maxwspd}, and \code{totprcp}
#'
#' @export
#'
#' @details A helper function used internally by several plotting functions to generate y-axis labels. This function does not convert sample results from metric to english. It only adjusts the units in the y-axis label.
#'
#' @author B. Crary
#'
#' @concept miscellaneous
#'
#' @return Returns character vector or an unevaluated expression
#'
#' @examples
#' \dontrun{
#' y_lab <- y_labeler('do_mgl')
#' }
#'
y_labeler <- function(param, converted = FALSE) {

  # Parameter abbreviations
  wq_params <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl'
                 , 'depth', 'cdepth', 'level', 'clevel'
                 , 'ph', 'turb', 'chlfluor')
  met_params <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd'
                  , 'maxwspdt', 'wdir', 'swdir', 'totpar'
                  , 'totprcp', 'totsorad')
  nut_params <- c('po4f', 'nh4f', 'no2f', 'no3f', 'no23f', 'chla_n', 'din', 'dip')

  if(!std_param_check(param)){
    # if user enters a non-std parameter, return the name of the parameter for a label
    y_lab <- param
  } else {

    # Parameter labels
    wq_lab <- c(quote('Water Temperature (' ~degree~ 'C)'), 'Specific Conductivity (mS/cm)', 'Salinity (psu)', 'Dissolved Oxygen Saturation (%)'
                , 'Dissolved Oxygen (mg/L)', 'Sonde Depth (m)', 'Depth, Corrected for \nBarometric Pressure (m)', 'Level (m)'
                , 'Level, corrected for \nBarometric Pressure (m)', 'pH (su)', 'Turbidity (NTU)', quote('Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

    met_lab <- c(quote('Air Temperature (' ~degree~ 'C)'), 'Relative Humidity (%)', 'Barometric Pressure (mb)', 'Wind Speed (m/s)'
                 , 'Maximum Wind Speed (m/s)', 'Maximum Time of \nWind Speed Measurement (hh:mm)', 'Wind Direction', 'Wind Direction \nStandard Deviation (sd)'
                 , quote('Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'), 'Precipitiation (mm)', quote('Total Solar \nRadiation (W/ '~m^2~ ')'))

    nut_lab <- c('Orthophosphate (mg/L)', 'Ammonium (mg/L)', 'Nitrite (mg/L)', 'Nitrate  (mg/L)', 'Nitrite + Nitrate  (mg/L)', quote('Chlorophyll-a (' ~mu~ 'g/L)')
                 , 'Diss. Inorganic Nitrogen (mg/L)', 'Diss. Inorganic Phosphorus (mg/L)')

    # Combine together for lookup
    all_params <- c(wq_params, met_params, nut_params)
    labs <- c(wq_lab, met_lab, nut_lab)

    names(labs) <- all_params

    if(converted){
      # Create labels for select parameters in english units
      converted_param <- c('temp', 'depth', 'cdepth', 'level', 'clevel'
                           , 'atemp', 'wspd', 'maxwspd', 'totprcp')
      converted_labs <- c(quote('Water Temperature (' ~degree~ 'F)'), 'Sonde Depth (ft)', 'Depth, Corrected for \nBarometric Pressure (ft)', 'Level (ft)'
                          , 'Level, corrected for \nBarometric Pressure (ft)', quote('Air Temperature (' ~degree~ 'F)'), 'Wind Speed (ft/s)'
                          , 'Maximum Wind Speed (ft/s)','Precipitiation (in)')

      names(converted_labs) <- converted_param

      y_lab <- converted_labs[[param]]

    } else {
      y_lab <- labs[[param]]
    }
  }

  return(y_lab)
}
