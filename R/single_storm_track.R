#' single_storm_track
#'
#' @param map_in .xlsx with all required input variables defined (string).
#' @param nerr_site_id 3 digit reserve code (string)
#' @param storm_nm name of storm event (string).
#' @param storm_rank numeric rank of storm for plotting aesthetics. a single storm with rank = 1 will be emphasized. (numeric integer)
#' @param stations comma separated list of stations to plot on map (string)
#' @param bbox comma separated bounding box for map extents (string vector )
#' @param path_to_shp pathway to shapefile for each storm track (string)
#' @param lab_loc location of labels on the map. optional (string).
#' @param scale_pos location of the scale on th map optional (string)
#' @param path_to_base pathway to shapefile for the basemap world boundaries (string)
#' @param skip TRUE/FALSE. If TRUE, function will be skipped (string).
#'
#' @return
#' @export
#'
#' @examples
single_storm_track <- function(map_in
                                  , nerr_site_id = NULL
                                  , storm_nm = NULL
                                  , storm_rank = NULL
                                  , stations = NULL
                                  , bbox = NULL
                                  , path_to_shp = NULL
                                  , path_to_base = NULL
                                  , lab_loc = NULL
                                  , scale_pos = 'bottomleft'
                                  , skip = NULL) {

  ### 0. Read variables ########################################################

  #a.  Read in the variable input template, var_in

  input_Parameters <- xlsx::read.xlsx(map_in, sheetName = "Single-storm parameters")
  input_Shp <- xlsx::read.xlsx(map_in, sheetName = "Single-storm shps")


  if(is.null(nerr_site_id)) nerr_site_id <- unlist(strsplit(input_Parameters[1,2],", "))
  if(is.null(bbox)) bbox <- as.numeric(unlist(strsplit(input_Parameters[2,2],", ")))
  if(is.null(storm_nm)) storm_nm <- input_Shp$storm_nm[!is.na(input_Shp$storm_nm)]
  if(is.null(path_to_shp)) path_to_shp <- input_Shp$path[!is.na(input_Shp$path)]
  if(is.null(path_to_base)) path_to_base <- input_Parameters[5,2]
  if(is.null(skip)) skip <- input_Parameters[6,2]
  if(is.null(storm_rank)) storm_rank <- input_Shp$storm_rank[!is.na(input_Shp$storm_rank)]


  ############## Tests #########################################################
  if(skip == "TRUE") {return(warning("skip set to 'TRUE', skipping single_storm_track"))}





  #b. Generate reserve labels

  loc <- get('sampling_stations')
  loc <- loc[(loc$NERR.Site.ID %in% nerr_site_id), ]
  loc$abbrev <- toupper(loc$NERR.Site.ID)

  reserve <- loc %>%
    distinct(abbrev, Latitude, Longitude) %>%
    mutate(Longitude = Longitude*-1) %>%
    group_by(abbrev) %>%
    summarize(avgx = mean(Longitude),
              avgy = mean(Latitude))

  #c. Read data as shapefiles

  for(i in 1:length(path_to_shp)) {

    temp <- sf::st_read(path_to_shp[i]) %>%
      sf::st_set_crs(4326) %>%
      #sf::st_crop(., xmin=bbox[1], xmax=bbox[3], ymin=bbox[4], ymax=bbox[2]) %>%
      dplyr::mutate(NAME = storm_nm[i]) %>%
      dplyr::mutate(RANK = storm_rank[i]) %>%
      dplyr::mutate(LABEL = c(storm_nm[i], rep("",nrow(.)-1)))

    ifelse(i == 1, shps <- temp, shps <- rbind(shps,temp))

  }


  # helper function

  linestring_labels <- function(linestrings) {

    dat <- linestrings

    names <- unique(dat$NAME)
    len <- length(unique(dat$NAME))


    for (n in names) {

      coords <- dat %>%
        dplyr::filter(NAME == n) %>%
        sf::st_cast("MULTILINESTRING") %>%
        sf::st_cast("LINESTRING") %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        #filter(X == nth(X,which(abs(X-target_x)==min(abs(X-target_x))))) %>%
        slice_head(n=1) %>%
        mutate(NAME = n)

      label_coords <- if (n == names[1]) {
        coords
      } else {
        rbind(label_coords, coords)
      }

      ## add color, transform into sf, plot as geom_sf or geom_text?

    }

    return(label_coords)

  }

  labs <- linestring_labels(shps)



  linestring_points <- function(linestrings) {

    dat <- linestrings

    names <- unique(dat$NAME)

    for(n in names) {

      df <- dat %>%
        filter(NAME == n)

      markers <- data.frame(sf::st_coordinates(df)) %>%
        select(-L1) %>%
        distinct()

      markers$X.after <- c(markers$X[-1], NA)
      markers$Y.after <- c(markers$Y[-1], NA)
      markers$X.dir <- with(markers, X.after-X)
      markers$Y.dir <- with(markers, Y.after-Y)
      markers$NAME <- n

      n1 <- markers[ceiling(nrow(markers)*.25),]
      n2 <- markers[ceiling(nrow(markers)*.5),]
      n3 <- markers[ceiling(nrow(markers)*.75),]
      n4 <- markers[nrow(markers)-1,]

      markers <- rbind(n1,n2,n3,n4)

      if(n == names[1]) {
        markerset <- markers
      } else {
        markerset <- rbind(markerset, markers)
      }

    }

    return(markerset)
  }


  arr <- linestring_points(shps)
  shps_combined <- sf::st_union(shps, by_feature = TRUE)
  shps_smooth <- smoothr::smooth(shps, method = "chaikin")


  #f. load base boundaries
  base <- sf::st_read(path_to_base)


  m <- ggplot2::ggplot() +
    #ggspatial::annotation_map_tile(zoom=3) +
    #basemaps::basemap_gglayer(shps, map_service = "osm", map_type = "no_labels") +
    ggplot2::geom_sf(data = base, fill = "#efefef", color = "#cfcfd1", lwd = .5) +
    ggplot2::geom_sf(data=shps_smooth, aes(color = NAME, size = as.factor(RANK)), inherit.aes = FALSE) +
    ggplot2::geom_point(data=reserve, aes(x = avgx, y = avgy), size = 3, color = "white") +
    ggplot2::geom_point(data=reserve, aes(x = avgx, y = avgy), size = 2, color = "grey10") +
    ggplot2::geom_segment(data=arr, aes(x = X, xend = X.after, y = Y, yend = Y.after, color = NAME),
                          arrow = arrow(
                            length=unit(0.15, "cm"),
                            type = "closed")) +
    ggrepel::geom_label_repel(data=reserve, aes(x = avgx, y = avgy, label = abbrev),
                              fill = "grey90",
                              fontface = "bold",
                              color = "grey10",
                              alpha = 0.5,
                              size = 3,
                              box.padding = 1,
                              segment.curvature =.1,
                              segment.size = 1,
                              segment.color = "grey30") +
    ggrepel::geom_label_repel(data=labs, aes(x = X , y = Y , label = NAME, fill = NAME),
                              color = "white",
                              alpha = 0.5,
                              size = 4,
                              box.padding = 7,
                              segment.color = NA) +
    ggplot2::coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[4], bbox[2]), expand = F) +
    viridis::scale_fill_viridis(discrete = TRUE, guide = "none") +
    viridis::scale_color_viridis(discrete = TRUE, guide = "none") +
    ggplot2::scale_size_manual(values = c(.5, rep(0.25, length(unique(shps$RANK))-1)), guide = "none") +
    ggplot2::theme_void() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(color = NA, fill = "#cfcfd1"),
                   axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text = element_blank(),
                   plot.margin = margin(0,0,0,0))


m

  ggsave("output/maps/single_storm_track2.png", m, width = 3.4722, height = 3.4722, units = "in", dpi=200)


}

