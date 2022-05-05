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

  # query active list of stations at selected reserve
  stations <- sampling_stations[(sampling_stations$NERR.Site.ID == reserve
                                 & sampling_stations$Status == 'Active'), ]$Station.Code

  # subset only wq and met stations (omitting nutrient stations)
  to_match <- c('wq', 'met')
  stns <- stations[grep(paste(to_match, collapse = '|'), stations)]

  # print order
  print(paste0("set lab_dir using this station order: ",paste0(stns, collapse = ", ")))

}
