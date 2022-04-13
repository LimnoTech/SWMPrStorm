#' Check user parameter against standard parameter list
#'
#' @param param user provided parameter code
#'
#' @return TRUE/FALSE
#' @export
#'
#' @examples
#'
#' \dontrun{
#' test_param <- 'totprcp'
#' st_param_check(test_param)
#' }
std_param_check <- function(param) {

  # Parameter abbreviations
  wq_params <- c('temp', 'spcond', 'sal', 'do_pct', 'do_mgl'
                 , 'depth', 'cdepth', 'level', 'clevel'
                 , 'ph', 'turb', 'chlfluor')

  met_params <- c('atemp', 'rh', 'bp', 'wspd', 'maxwspd'
                  , 'maxwspdt', 'wdir', 'sdwdir', 'totpar'
                  , 'totprcp', 'intensprcp', 'totsorad')

  nut_params <- c('po4f', 'nh4f', 'no2f', 'no3f', 'no23f', 'chla_n', 'din', 'dip')

  # Combine together for lookup
  all_params <- c(wq_params, met_params, nut_params)


  ifelse(param %in% all_params, TRUE, FALSE)
}

