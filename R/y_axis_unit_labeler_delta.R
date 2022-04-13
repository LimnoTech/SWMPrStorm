#' y_axis_unit_labeler_delta
#'
#' @param param wq or met cdmo parameter code
#' @param user_units 'English' or 'SI'
#'
#' @return returns a chr string for y-axis labels in roc plots with specialized characters
#' @export
#'
#' @examples
#'
#' \dontrun{
#' parameter <- 'temp'
#' units <- 'English'
#' y_axis_unit_labeler_delta(parameter, units)
#' }
y_axis_unit_labeler_delta <- function(param, user_units) {
  # Parameter abbreviations
  wq_params <- c('temp',
                 'spcond',
                 'sal',
                 'do_pct',
                 'do_mgl',
                 'depth',
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
    wq_lab <- c(quote(~Delta~'Water Temperature (' ~degree~ 'C)'),
                quote(~Delta~'Specific Conductivity (mS/cm)'),
                quote(~Delta~'Salinity (psu)'),
                quote(~Delta~'Dissolved Oxygen Saturation (%)'),
                quote(~Delta~'Dissolved Oxygen (mg/L)'),
                quote(~Delta~'Sonde Depth (m)'),
                quote(~Delta~'Depth, Corrected for \nBarometric Pressure (m)'),
                quote(~Delta~'Level (m)'),
                quote(~Delta~'Level, corrected for \nBarometric Pressure (m)'),
                quote(~Delta~'pH (su)'),
                quote(~Delta~'Turbidity (NTU)'),
                quote(~Delta~'Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

    met_lab <- c(quote(~Delta~'Air Temperature (' ~degree~ 'C)'),
                 quote(~Delta~'Relative Humidity (%)'),
                 quote(~Delta~'Barometric Pressure (mb)'),
                 quote(~Delta~'Wind Speed (m/s)'),
                 quote(~Delta~'Maximum Wind Speed (m/s)'),
                 quote(~Delta~'Maximum Time of \nWind Speed Measurement (hh:mm)'),
                 quote(~Delta~'Wind Direction (' ~degree~ ')'),
                 quote(~Delta~'Wind Direction \nStandard Deviation (sd)'),
                 quote(~Delta~'Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'),
                 quote(~Delta~'Precipitiation (mm)'),
                 quote(~Delta~'Precipitation Intensity (mm/hr)'),
                 quote(~Delta~'Total Solar \nRadiation (W/ '~m^2~ ')'))


    labs <- c(wq_lab, met_lab)
    names(labs) <- all_params

    ylab <- labs[[param]]


  } else if(user_units == "English") {

    # Parameter labels
    wq_lab <- c(quote(~Delta~'Water Temperature (' ~degree~ 'F)'),
                quote(~Delta~'Specific Conductivity (mS/cm)'),
                quote(~Delta~'Salinity (psu)'),
                quote(~Delta~'Dissolved Oxygen Saturation (%)'),
                quote(~Delta~'Dissolved Oxygen (mg/L)'),
                quote(~Delta~'Sonde Depth (ft)'),
                quote(~Delta~'Depth, Corrected for \nBarometric Pressure (ft)'),
                quote(~Delta~'Level (ft)'),
                quote(~Delta~'Level, corrected for \nBarometric Pressure (ft)'),
                quote(~Delta~'pH (su)'),
                quote(~Delta~'Turbidity (NTU)'),
                quote(~Delta~'Chlorophyll Fluorescence (' ~mu~ 'g/L)'))

    met_lab <- c(quote(~Delta~'Air Temperature (' ~degree~ 'F)'),
                 quote(~Delta~'Relative Humidity (%)'),
                 quote(~Delta~'Barometric Pressure (inHg)'),
                 quote(~Delta~'Wind Speed (mph)'),
                 quote(~Delta~'Maximum Wind Speed (mph)'),
                 quote(~Delta~'Maximum Time of \nWind Speed Measurement (hh:mm)'),
                 quote(~Delta~'Wind Direction (' ~degree~ ')'),
                 quote(~Delta~'Wind Direction \nStandard Deviation (sd)'),
                 quote(~Delta~'Photosynthetically \nActive Radiation (mmol/' ~m^2~ ')'),
                 quote(~Delta~'Precipitiation (in)'),
                 quote(~Delta~'Precipitation Intensity (in/hr)'),
                 quote(~Delta~'Total Solar \nRadiation (W/ '~m^2~ ')'))


    labs <- c(wq_lab, met_lab)
    names(labs) <- all_params

    ylab <- labs[[param]]

  }

  return(ylab)

}




