#' initialize_project()
#'
#' @param path_to_project
#'
#' @return
#' @export
#'
#' @examples
initialize_project <- function(path_to_project = NULL) {

  if(is.null(path_to_project)) path_to_project <- getwd()

  ifelse(substring(path_to_project, nchar(path_to_project), nchar(path_to_project))!="/",
         path_to_project,
         substring(path_to_project, 0, nchar(path_to_project)))

  print(path_to_project)

  if(!dir.exists(paste0(path_to_project, "/output"))) {dir.create(paste0(path_to_project, "/output"))}
  if(!dir.exists(paste0(path_to_project, "/output/maps/"))) {dir.create(paste0(path_to_project, "/output/maps/"))}
  if(!dir.exists(paste0(path_to_project, "/output/combined/"))) {dir.create(paste0(path_to_project, "/output/combined/"))}
  if(!dir.exists(paste0(path_to_project, "/output/combined/timeseries_dual_axis/"))) {dir.create(paste0(path_to_project, "/output/combined/timeseries_dual_axis/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/"))) {dir.create(paste0(path_to_project, "/output/wq/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/comparison_one_evt_multi_reserve/"))) {dir.create(paste0(path_to_project, "/output/wq/comparison_one_evt_multi_reserve/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/comparison_one_reserve_multi_event/"))) {dir.create(paste0(path_to_project, "/output/wq/comparison_one_reserve_multi_event/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/event_roc/"))) {dir.create(paste0(path_to_project, "/output/wq/event_roc/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/ridgelines/"))) {dir.create(paste0(path_to_project, "/output/wq/ridgelines/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/timeseries_event_hourly/"))) {dir.create(paste0(path_to_project, "/output/wq/timeseries_event_hourly/"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/timeseries_event_recovery"))) {dir.create(paste0(path_to_project, "/output/wq/timeseries_event_recovery"))}
  if(!dir.exists(paste0(path_to_project, "/output/wq/compare_one_reserve_one_event/"))) { dir.create(paste0(path_to_project, "/output/wq/compare_one_reserve_one_event/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/"))) {dir.create(paste0(path_to_project, "/output/met/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/barplot/"))) {dir.create(paste0(path_to_project, "/output/met/barplot/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/comparison_one_evt_multi_reserve/"))) {dir.create(paste0(path_to_project, "/output/met/comparison_one_evt_multi_reserve/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/comparison_one_reserve_multi_event/"))) {dir.create(paste0(path_to_project, "/output/met/comparison_one_reserve_multi_event/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/event_roc/"))) {dir.create(paste0(path_to_project, "/output/met/event_roc/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/ridgelines/"))) {dir.create(paste0(path_to_project, "/output/met/ridgelines/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/timeseries_event_hourly/"))) {dir.create(paste0(path_to_project, "/output/met/timeseries_event_hourly/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/windrose/"))) { dir.create(paste0(path_to_project, "/output/met/windrose/"))}
  if(!dir.exists(paste0(path_to_project, "/output/met/compare_one_reserve_one_event/"))) { dir.create(paste0(path_to_project, "/output/met/compare_one_reserve_one_event/"))}

}
