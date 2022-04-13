#' convert_units()
#'
#' @param dat list of datasets CDMO datasets
#' @param to_units unit set to convert from default SWMPrStorm units ('English' or 'SI')
#'
#' @return df, list of datasets in the same input format as dat
#' @export
#'
#' @examples
#' \dontrun{
#' convert_units(dat, 'English')
#' }
convert_units <- function(dat,
                          to_units) {


  params <- get('unit_table')
  params <- as.vector(params$code)


  #df <- ls_par
  df <- dat

  if(to_units == "English") {

    df <- lapply(df, function(x) {if('cdepth' %in% names(x)) {x$cdepth <- x$cdepth*3.28084; x} else {x}}) # m to ft
    df <- lapply(df, function(x) {if('chlfluor' %in% names(x)) {x$chlfluor <- x$chlfluor; x} else {x}}) # no conversion from ug/L
    df <- lapply(df, function(x) {if('clevel' %in% names(x)) {x$clevel <- x$clevel*3.28084; x} else {x}}) # m to ft
    df <- lapply(df, function(x) {if('depth' %in% names(x)) {x$depth <- x$depth*3.28084; x} else {x}}) # m to ft
    df <- lapply(df, function(x) {if('do_mgl' %in% names(x)) {x$do_mgl <- x$do_mgl; x} else {x}}) # no conversion from mg/L
    df <- lapply(df, function(x) {if('do_pct' %in% names(x)) {x$do_pct <- x$do_pct; x} else {x}}) # no conversion from %
    df <- lapply(df, function(x) {if('level' %in% names(x)) {x$level <- x$level*3.28084; x} else {x}}) # m to ft
    df <- lapply(df, function(x) {if('pH' %in% names(x)) {x$pH <- x$pH; x} else {x}}) # no conversion from su
    df <- lapply(df, function(x) {if('sal' %in% names(x)) {x$sal <- x$sal; x} else {x}}) # no conversion from psu
    df <- lapply(df, function(x) {if('spcond' %in% names(x)) {x$spcond <- x$spcond; x} else {x}}) # no conversion from mS/cm
    df <- lapply(df, function(x) {if('temp' %in% names(x)) {x$temp <- x$temp*9/5+32; x} else {x}}) # C to F
    df <- lapply(df, function(x) {if('turb' %in% names(x)) {x$turb <- x$turb; x} else {x}}) # no conversion from NTU
    df <- lapply(df, function(x) {if('atemp' %in% names(x)) {x$atemp <- x$atemp*9/5+32; x} else {x}}) # C to F
    df <- lapply(df, function(x) {if('bp' %in% names(x)) {x$bp <- x$bp/33.864; x} else {x}}) # mb to in Hg
    df <- lapply(df, function(x) {if('intensprcp' %in% names(x)) {x$intensprcp <- x$intensprcp / 25.4; x} else {x}}) # mm/hr to in/hr
    df <- lapply(df, function(x) {if('maxwspd' %in% names(x)) {x$maxwspd <- x$maxwspd * 3600 * 1 / 1609.34; x} else {x}}) # m/s to mph
    df <- lapply(df, function(x) {if('rh' %in% names(x)) {x$rh <- x$rh; x} else {x}}) # no conversion from %
    df <- lapply(df, function(x) {if('sdwdir' %in% names(x)) {x$sdwdir <- x$sdwdir; x} else {x}}) # no conversion from sd
    df <- lapply(df, function(x) {if('totpar' %in% names(x)) {x$totpar <- x$totpar; x} else {x}}) # no conversion from mmol/m2
    df <- lapply(df, function(x) {if('totprcp' %in% names(x)) {x$totprcp <- x$totprcp / 25.4; x} else {x}})# mm to in
    df <- lapply(df, function(x) {if('totsorad' %in% names(x)) {x$totsorad <- x$totsorad; x} else {x}}) # no conversion from W/m2
    df <- lapply(df, function(x) {if('wdir' %in% names(x)) {x$wdir <- x$wdir; x} else {x}}) # no conversion from deg
    df <- lapply(df, function(x) {if('wspd' %in% names(x)) {x$wspd <- x$wspd * 3600 * 1 / 1609.34; x} else {x}}) # m/s to mph

  }


#  if(to_units == "SI") {

#    df <- lapply(df, function(x) {if('cdepth' %in% names(x)) {x$cdepth <- x$cdepth; x} else {x}}) # no conversion from m
#    df <- lapply(df, function(x) {if('chlfluor' %in% names(x)) {x$chlfluor <- x$chlfluor; x} else {x}}) # no conversion from ug/L
#    df <- lapply(df, function(x) {if('clevel' %in% names(x)) {x$clevel <- x$clevel; x} else {x}}) # no conversion from m
#    df <- lapply(df, function(x) {if('depth' %in% names(x)) {x$depth <- x$depth; x} else {x}}) # no conversion from m
#    df <- lapply(df, function(x) {if('do_mgl' %in% names(x)) {x$do_mgl <- x$do_mgl; x} else {x}}) # no conversion from mg/L
#    df <- lapply(df, function(x) {if('do_pct' %in% names(x)) {x$do_pct <- x$do_pct; x} else {x}}) # no conversion from %
#    df <- lapply(df, function(x) {if('level' %in% names(x)) {x$level <- x$level; x} else {x}}) # no conversion from m
#    df <- lapply(df, function(x) {if('pH' %in% names(x)) {x$pH <- x$pH; x} else {x}}) # no conversion from su
#    df <- lapply(df, function(x) {if('sal' %in% names(x)) {x$sal <- x$sal; x} else {x}}) # no conversion from psu
#    df <- lapply(df, function(x) {if('spcond' %in% names(x)) {x$spcond <- x$spcond; x} else {x}}) # no conversion from mS/cm
#    df <- lapply(df, function(x) {if('temp' %in% names(x)) {x$temp <- x$temp; x} else {x}}) # no conversion from C
#    df <- lapply(df, function(x) {if('turb' %in% names(x)) {x$turb <- x$turb; x} else {x}}) # no conversion from NTU
#    df <- lapply(df, function(x) {if('atemp' %in% names(x)) {x$atemp <- (32 - x$atemp)*5/9; x} else {x}}) # F to C
#    df <- lapply(df, function(x) {if('bp' %in% names(x)) {x$bp <- x$bp; x} else {x}}) # no conversion from mb
#    df <- lapply(df, function(x) {if('intensprcp' %in% names(x)) {x$intensprcp <- x$intensprcp/25.4; x} else {x}}) # in/hr to mm/hr
#    df <- lapply(df, function(x) {if('maxwspd' %in% names(x)) {x$maxwspd <- x$maxwspd*0.44704; x} else {x}}) # mph to m/s
#    df <- lapply(df, function(x) {if('rh' %in% names(x)) {x$rh <- x$rh; x} else {x}}) # no conversion from %
#    df <- lapply(df, function(x) {if('sdwdir' %in% names(x)) {x$sdwdir <- x$sdwdir; x} else {x}}) # no conversion from sd
#    df <- lapply(df, function(x) {if('totpar' %in% names(x)) {x$totpar <- x$totpar; x} else {x}}) # no conversion from mmol/m2
#    df <- lapply(df, function(x) {if('totprcp' %in% names(x)) {x$totprcp <- x$totprcp/25.4; x} else {x}})# in to mm
#    df <- lapply(df, function(x) {if('totsorad' %in% names(x)) {x$totsorad <- x$totsorad; x} else {x}}) # no conversion from W/m2
#    df <- lapply(df, function(x) {if('wdir' %in% names(x)) {x$wdir <- x$wdir; x} else {x}}) # no conversion from deg
#    df <- lapply(df, function(x) {if('wspd' %in% names(x)) {x$wspd <- x$wspd*0.44704; x} else {x}}) # mph to m/s
#  }


return(df)

}
