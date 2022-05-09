#' print_lab_dir_stn_order()
#'
#' @param reserve 3-digit reserve code
#'
#' @export
#'
#' @examples
#' \dontrun{
#' print_lab_dir_stn_order('del')
#' }
print_lab_dir_stn_order <- function(reserve) {

  #load stations data
  stations_data <- get('sampling_stations')


  # query active list of stations at selected reserve
  stations <- stations_data[(stations_data$NERR.Site.ID == reserve
                                 & stations_data$Status == 'Active'), ]$Station.Code

  # subset only wq and met stations (omitting nutrient stations)
  to_match <- c('wq', 'met')
  stns <- stations[grep(paste(to_match, collapse = '|'), stations)]

  # print order
  print(paste0("set lab_dir using this station order: ",paste0(stns, collapse = ", ")))

}
