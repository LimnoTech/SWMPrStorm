#' initialize_project()
#'
#' @param path_to_project pathway to project location. If blank, the current working directory will be called. (string).
#'
#' @return no objects returned. standard SWMPrStorm empty output directories are created.
#' @export
#'
#' @examples
#'
#' \dontrun{
#' ## initialize in current working director:
#' initialize_project()
#' ## initialize in specific location
#' working_dir <- 'C:/path/to/directory'
#' initialize_project(working_dir)
#' }
initialize_project <- function(path_to_project = NULL) {

  if(is.null(path_to_project)) path_to_project <- getwd()

  ifelse(substring(path_to_project, nchar(path_to_project), nchar(path_to_project))!="/",
         path_to_project,
         substring(path_to_project, 0, nchar(path_to_project)))

  print(path_to_project)

  #Lookup paths and create directories if they do not exist
  if(!dir.exists(paste0(path_to_project, "/output"))) {dir.create(paste0(path_to_project, "/output"))}
  if(!dir.exists(paste0(path_to_project, "/output/maps/"))) {dir.create(paste0(path_to_project, "/output/maps/"))}
  if(!dir.exists(paste0(path_to_project, "/output/combined/"))) {dir.create(paste0(path_to_project, "/output/combined/"))}
  if(!dir.exists(paste0(path_to_project, "/output/combined/timeseries_dual_axis/"))) {dir.create(paste0(path_to_project, "/output/combined/timeseries_dual_axis/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/"))) {dir.create(paste0(path_to_project, "/output/wq/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/data_one_event_multi_reserve_table/"))) {dir.create(paste0(path_to_project, "/output/wq/data_one_event_multi_reserve_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/data_one_reserve_multi_event_table/"))) {dir.create(paste0(path_to_project, "/output/wq/data_one_reserve_multi_event_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/data_table/"))) {dir.create(paste0(path_to_project, "/output/wq/data_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/event_roc/"))) {dir.create(paste0(path_to_project, "/output/wq/event_roc/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/ridgelines/"))) {dir.create(paste0(path_to_project, "/output/wq/ridgelines/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/timeseries_event_hourly/"))) {dir.create(paste0(path_to_project, "/output/wq/timeseries_event_hourly/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/timeseries_event_recovery"))) {dir.create(paste0(path_to_project, "/output/wq/timeseries_event_recovery"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/data_one_reserve_one_event_table/"))) { dir.create(paste0(path_to_project, "/output/wq/data_one_reserve_one_event_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/"))) {dir.create(paste0(path_to_project, "/output/met/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/barplot/"))) {dir.create(paste0(path_to_project, "/output/met/barplot/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/data_one_event_multi_reserve_table/"))) {dir.create(paste0(path_to_project, "/output/met/data_one_event_multi_reserve_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/data_one_reserve_multi_event_table/"))) {dir.create(paste0(path_to_project, "/output/met/data_one_reserve_multi_event_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/event_roc/"))) {dir.create(paste0(path_to_project, "/output/met/event_roc/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/ridgelines/"))) {dir.create(paste0(path_to_project, "/output/met/ridgelines/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/timeseries_event_hourly/"))) {dir.create(paste0(path_to_project, "/output/met/timeseries_event_hourly/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/windrose/"))) { dir.create(paste0(path_to_project, "/output/met/windrose/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/data_one_reserve_one_event_table/"))) { dir.create(paste0(path_to_project, "/output/met/data_one_reserve_one_event_table/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/data_table/"))) {dir.create(paste0(path_to_project, "/output/met/data_table/"))}

}
