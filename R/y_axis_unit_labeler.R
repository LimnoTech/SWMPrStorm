#' Generate y-axis Label Based on SWMP Parameter Abbreviation
#'
#'
#' @param param chr string of variable abbreviation
#' @param user_units chr string equal to 'English" or 'SI"
#'
#' @export
#'
#' @details A helper function used internally by several plotting functions to generate y-axis labels. This function does not convert sample results from metric to english. It only adjusts the units in the y-axis label.
#'
#'
#' @return Returns character vector that can be used as a y-axis label in ggplot2. includes special characters.
#'
#' @examples
#' \dontrun{
#' y_axis_unit_labeler <- y_labeler('do_mgl', 'English')
#' }
#'
y_axis_unit_labeler <- function(param, user_units) {

  # Parameter abbreviations
  wq_params <- c('temp',
                 'spcond',
                 'sal',
                 'do_pct',
                 'do_mgl'
                 , 'depth',
                 'cdepth',
                 'level',
                 'clevel',
                 'ph',
                 'turb',
                 'chlfluor')

  met_params <- c('atemp',
                  'rh',
                  'bp',
                  'wspd',
                  'maxwspd',
                  'maxwspdt',
                  'wdir',
                  'sdwdir',
                  'totpar',
                  'totprcp',
                  'intensprcp',
                  'totsorad')

  all_params <- c(wq_params, met_params)

  if(!std_param_check(param)){
      # if user enters a non-std parameter, return the name of the parameter for a label
      y_lab <- param
    } else if(user_units == "SI") {

      # Parameter labels
      wq_lab <- c(quote('Water Temperature (' ~degree~ 'C)'),
                  'Specific Conductivity (mS/cm)',
                  'Salinity (psu)',
                  'Dissolved Oxygen Saturation (%)',
                  'Dissolved Oxygen (mg/L)',
                  'Sonde Depth (m)',
                  'Depth, Corrected for \nBarometric Pressure (m)',
                  'Level (m)',
                  'Level, corrected for \nBarometric Pressure (m)',
                  'pH (su)',
                  'Turbidity (NTU)',
                  quote('Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

      met_lab <- c(quote('Air Temperature (' ~degree~ 'C)'),
                   'Relative Humidity (%)',
                   'Barometric Pressure (mb)',
                   'Wind Speed (m/s)',
                   'Maximum Wind Speed (m/s)',
                   'Maximum Time of \nWind Speed Measurement (hh:mm)',
                   quote('Wind Direction (' ~degree~ ')'),
                   'Wind Direction \nStandard Deviation (sd)',
                   quote('Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'),
                   'Precipitiation (mm)',
                   'Precipitation Intensity (mm/hr)',
                   quote('Total Solar \nRadiation (W/ '~m^2~ ')'))


      labs <- c(wq_lab, met_lab)
      names(labs) <- all_params

      ylab <- labs[[param]]


    } else if(user_units == "English") {

      # Parameter labels
      wq_lab <- c(quote('Water Temperature (' ~degree~ 'F)'),
                  'Specific Conductivity (mS/cm)',
                  'Salinity (psu)',
                  'Dissolved Oxygen Saturation (%)',
                  'Dissolved Oxygen (mg/L)',
                  'Sonde Depth (ft)',
                  'Depth, Corrected for \nBarometric Pressure (ft)',
                  'Level (ft)',
                  'Level, corrected for \nBarometric Pressure (ft)',
                  'pH (su)',
                  'Turbidity (NTU)',
                  quote('Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

      met_lab <- c(quote('Air Temperature (' ~degree~ 'F)'),
                   'Relative Humidity (%)',
                   'Barometric Pressure (inHg)',
                   'Wind Speed (mph)',
                   'Maximum Wind Speed (mph)',
                   'Maximum Time of \nWind Speed Measurement (hh:mm)',
                   quote('Wind Direction (' ~degree~ ')'),
                   'Wind Direction \nStandard Deviation (sd)',
                   quote('Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'),
                   'Precipitiation (in)',
                   'Precipitation Intensity (in/hr)',
                   quote('Total Solar \nRadiation (W/ '~m^2~ ')'))


      labs <- c(wq_lab, met_lab)
      names(labs) <- all_params

      ylab <- labs[[param]]

    }

  return(ylab)

}
