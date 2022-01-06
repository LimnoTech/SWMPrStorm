#' Title
#'
#' @param map_in
#' @param nerr_site_id
#' @param storm_nm
#' @param storm_rank
#' @param stations
#' @param bbox
#' @param path_to_shp
#' @param lab_loc
#' @param scale_pos
#'
#' @return
#' @export
#'
#' @examples
res_multi_storm_track <- function(map_in
                            , nerr_site_id = NULL
                            , storm_nm = NULL
                            , storm_rank = NULL
                            , stations = NULL
                            , bbox = NULL
                            , path_to_shp = NULL
                            , lab_loc = NULL
                            , scale_pos = 'bottomleft') {

  ### 0. Read variables ########################################################

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(map_in, sheetName = "Parameters")
  input_Shp <- xlsx::read.xlsx(map_in, sheetName = "shps")
  input_Stations <- xlsx::read.xlsx(map_in, sheetName = "Stations")


  if(is.null(nerr_site_id)) nerr_site_id <- unlist(strsplit(input_Parameters[1,2],", "))
  if(is.null(bbox)) bbox <- as.numeric(unlist(strsplit(input_Parameters[2,2],", ")))
  if(is.null(storm_nm)) storm_nm <- input_Shp$storm_nm[!is.na(input_Shp$storm_nm)]
  if(is.null(path_to_shp)) path_to_shp <- input_Shp$path[!is.na(input_Shp$path)]
  if(is.null(storm_rank)) storm_rank <- input_Shp$storm_rank[!is.na(input_Shp$storm_rank)]


}
