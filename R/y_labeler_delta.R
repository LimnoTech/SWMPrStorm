#' Generate y-axis Label Based on SWMP Parameter Abbreviation
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
#'
#' }
#'
y_labeler_delta <- function(param, converted = FALSE) {

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
    wq_lab <- c(quote('delta Water Temperature (' ~degree~ 'C)'), 'delta Specific Conductivity (mS/cm)', 'delta Salinity (psu)', 'delta Dissolved Oxygen \nSaturation (%)'
                , 'delta Dissolved Oxygen (mg/L)', 'delta Sonde Depth (m)', 'delta Depth, Corrected for \nBarometric Pressure (m)', 'delta Level (m)'
                , 'deltaLevel, corrected for \nBarometric Pressure (m)', 'delta pH (su)', 'delta Turbidity (NTU)', quote('delta Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

    met_lab <- c(quote('delta Air Temperature (' ~degree~ 'C)'), 'delta Relative Humidity (%)', 'delta Barometric Pressure (mb)', 'delta Wind Speed (m/s)'
                 , 'delta Maximum Wind Speed (m/s)', 'delta Maximum Time of \nWind Speed Measurement (hh:mm)', 'delta Wind Direction', 'delta Wind Direction \nStandard Deviation (sd)'
                 , quote('delta Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'), 'delta Precipitiation (mm)', quote('delta Total Solar \nRadiation (W/ '~m^2~ ')'))

    nut_lab <- c('delta Orthophosphate (mg/L)', 'delta Ammonium (mg/L)', 'delta Nitrite (mg/L)', 'delta Nitrate  (mg/L)', 'delta Nitrite + Nitrate  (mg/L)', quote('delta Chlorophyll-a (' ~mu~ 'g/L)')
                 , 'delta Diss. Inorganic Nitrogen (mg/L)', 'delta Diss. Inorganic Phosphorus (mg/L)')

    # Combine together for lookup
    all_params <- c(wq_params, met_params, nut_params)
    labs <- c(wq_lab, met_lab, nut_lab)

    names(labs) <- all_params

    if(converted){
      # Create labels for select parameters in english units
      converted_param <- c('temp', 'depth', 'cdepth', 'level', 'clevel'
                           , 'atemp', 'wspd', 'maxwspd', 'totprcp')
      converted_labs <- c(quote('delta Water Temperature (' ~degree~ 'F)'), 'delta Sonde Depth (ft)', 'delta Depth, Corrected for \nBarometric Pressure (ft)', 'delta Level (ft)'
                          , 'delta Level, corrected for \nBarometric Pressure (ft)', quote('delta Air Temperature (' ~degree~ 'F)'), 'delta Wind Speed (ft/s)'
                          , 'delta Maximum Wind Speed (ft/s)','delta Precipitiation (in)')

      names(converted_labs) <- converted_param

      y_lab <- converted_labs[[param]]

    } else {
      y_lab <- labs[[param]]
    }
  }

  return(y_lab)
}
