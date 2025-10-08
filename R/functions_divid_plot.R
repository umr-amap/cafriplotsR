


#' Extract all corners of 1ha plot
#'
#' Extract all corners in UTM and geographic coordinates of 1ha plot and map it as well
#'
#'
#' @author Hugo Leblanc
#'
#' @param coordinates tibble output of query_plots, element coordinates
#'
#' @importFrom data.table data.table setnames rbindlist
#' @importFrom ggpubr ggarrange
#' @importFrom sf st_multipoint
#' 
#' @return list
#' @export
extract_corners = function(coordinates, map_res = FALSE) {
  
  all_plot <- coordinates %>% distinct(plot_name)
  
  
  all_res <- vector('list', nrow(all_plot))
  for (i in 1:nrow(all_plot)) {
  
    coordinates_subset <- 
      coordinates %>% 
      filter(plot_name == all_plot$plot_name[i])
    
    longlat <- coordinates_subset[, c("typevalue_ddlon", "typevalue_ddlat")]
    coordRel <- coordinates_subset[, c("Xrel", "Yrel")]
    rangeX <- c(0, 100)
    rangeY <- c(0, 100)
    maxDist <- 10
    rmOutliers <- TRUE
    projCoord <- NULL
    
    
    # check -------------------------------------------------------------------
    
    if (is.null(longlat)) {
      stop("Give one set of coordinates: coordinates[, c('typevalue_ddlon', 'typevalue_ddlat')]")
    }
    
    if (!all(data.table::between(coordRel[, 1], lower = rangeX[1], upper = rangeX[2]) &
             data.table::between(coordRel[, 2], lower = rangeY[1], upper = rangeY[2]))) {
      stop("coordinates[, c('Xrel', 'Yrel')] must be inside the 0 and 100 ranges")
    }
    if ((!is.null(longlat) && any(dim(longlat) != dim(coordRel))) ||
        (!is.null(projCoord) && any(dim(projCoord) != dim(coordRel)))) {
      stop("GPS and relative coordinates are not of the same dimension")
    }
    
    if (!is.null(longlat)) {
      projCoord <- latlong2UTM(longlat)
      codeUTM <- unique(projCoord[, "codeUTM"])
      projCoord <- projCoord[, c("X", "Y")]
    }
    
    
    # Projection --------------------------------------------------------------
    
    res <- procrust(projCoord, coordRel)
    coordAbs <- as.matrix(coordRel) %*% res$rotation
    coordAbs <- sweep(coordAbs, 2, res$translation, FUN = "+")
    dist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 + (coordAbs[,2] - projCoord[, 2])^2)
    outliers <- which(dist > maxDist)
    
    
    # Outliers ---------------------------------------------------------------
    
    if (length(outliers) == nrow(projCoord)) {
      stop("All coordinates points are considered as outliers at the first stage.\n\n         This may be because some coordinates have very large error associated.\n\n         Try to remove these very large error or reconsider the maxDist parameter by increasing the distance")
    }
    if (rmOutliers & length(outliers) > 0) {
      refineCoord <- TRUE
      while (refineCoord) {
        res <- procrust(projCoord[-outliers, ], coordRel[-outliers,
        ])
        coordAbs <- as.matrix(coordRel) %*% res$rotation
        coordAbs <- sweep(coordAbs, 2, res$translation,
                          FUN = "+")
        newdist <- sqrt((coordAbs[, 1] - projCoord[, 1])^2 +
                          (coordAbs[, 2] - projCoord[, 2])^2)
        if (all(which(newdist > maxDist) == outliers))
          refineCoord <- FALSE
        outliers <- which(newdist > maxDist)
      }
    }
    
    
    
    # Create plot polygons ----------------------------------------------------
    
    cornerCoord <- as.matrix(expand.grid(X = sort(rangeX), Y = sort(rangeY)))
    cornerCoord <- cornerCoord[c(1, 2, 4, 3), ]
    cornerCoord <- as.matrix(cornerCoord) %*% res$rotation
    cornerCoord <- sweep(cornerCoord, 2, res$translation, FUN = "+")
    p <- st_multipoint(rbind(cornerCoord, cornerCoord[1, ]))
    ps <- st_polygon(list(p), 1)
    sps <- st_sfc(list(ps))
    if (length(outliers) != 0 & !rmOutliers) {
      warning("Be carefull, you may have GNSS measurement outliers. \n",
              "Removing them may improve the georeferencing of your plot (see  the rmOutliers argument).")
    }
    correct_plot <-
      list(
        cornerCoords = data.frame(X = cornerCoord[, 1], Y = cornerCoord[, 2]),
        correctedCoord = data.frame(X = coordAbs[, 1], Y = coordAbs[, 2]),
        polygon = sps,
        outliers = outliers
      )
    
    
    # Extract corners ---------------------------------------------------------
    
    if (!is.null(longlat)) {
      correct_plot$codeUTM <- codeUTM
    }
    
    projCoord = correct_plot$cornerCoords
    plot = rep("plot", 4)
    cornerNum = c(1, 2, 3, 4)
    gridsize = 20
    dimX = 100
    dimY = 100
    
    if (!(length(dimY) %in% c(1, length(unique(plot))))) {
      stop("Your dimY vector must be of length 1 or of length equal to length(unique(plot))")
    }
    if (any(gridsize > dimX) || any(gridsize > dimY)) {
      stop("Your gridsize is larger than the X or Y dimensions")
    }
    cornerCoord <- data.table(plot = plot, X = projCoord[, 1],
                              Y = projCoord[, 2], cornerNum = cornerNum)
    
    data.table::setnames(cornerCoord, colnames(cornerCoord), c("plot", "X",
                                                   "Y", "cornerNum"))
    cornerCoord <- cornerCoord[order(cornerNum), .SD, by = plot]
    dimRel <- data.table::data.table(plot = unique(plot), dimX = dimX, dimY = dimY)
    gridFunction <- function(data, gridsize) {
      absCoordMat <- as.matrix(data[, .(X, Y)])
      plotDimX <- as.numeric(unique(data[, "dimX"]))
      plotDimY <- as.numeric(unique(data[, "dimY"]))
      relCoordMat <- matrix(c(0, 0, 0, plotDimY, plotDimX,
                              plotDimY, plotDimX, 0), byrow = T, ncol = 2)
      gridMat <- as.matrix(expand.grid(X = seq(0, max(relCoordMat[,
                                                                  1]), by = gridsize), Y = seq(0, max(relCoordMat[,
                                                                                                                  2]), by = gridsize)))
      absCoord <- bilinear_interpolation(coord = gridMat,
                                         from_corner_coord = relCoordMat, to_corner_coord = absCoordMat)
      return(data.table(XRel = gridMat[, 1], YRel = gridMat[,
                                                            2], XAbs = absCoord[, 1], YAbs = absCoord[, 2]))
    }
    
    
    cornerCoord <- cornerCoord[dimRel, on = "plot"][, gridFunction(.SD,
                                                                   gridsize), by = plot]
    numberingCorner <- function(data) {
      data.table::rbindlist(apply(data[XRel < max(XRel) & YRel < max(YRel),
                           -"plot"], 1, function(x) {
                             X <- x["XRel"]
                             Y <- x["YRel"]
                             data[(XRel == X & YRel == Y) | (XRel == X + gridsize &
                                                               YRel == Y) | (XRel == X + gridsize & YRel ==
                                                                               Y + gridsize) | (XRel == X & YRel == Y + gridsize),
                                  .(subplot = paste(plot, X/gridsize, Y/gridsize,
                                                    sep = "_"), XRel, YRel, XAbs, YAbs)][, `:=`(cornerNum,
                                                                                                c(1, 4, 2, 3))]
                           }))
    }
    cornerCoord <- cornerCoord[, numberingCorner(.SD), by = plot,
                               .SDcols = colnames(cornerCoord)]
    
    
    
    # Corners names correction ------------------------------------------------
    
    rawXRel = cornerCoord[['XRel']]
    rawYRel = cornerCoord[['YRel']]
    
    cornerCoord <- 
      as.data.frame(cornerCoord) %>% 
      mutate(XRel = rawYRel, 
             YRel = rawXRel,
             jalon = paste(XRel,YRel,sep='_'))  %>%
      group_by(subplot) %>% 
      mutate(sousplot = paste(min(XRel),min(YRel),sep='_'))%>%
      ungroup() %>% 
      dplyr::select(sousplot, jalon, XRel, YRel, XAbs, YAbs, cornerNum)
    
    
    # Plots -------------------------------------------------------------------
    
    if (map_res) {
      raw <- ggplot(coordinates_subset,
                    aes(
                      x = typevalue_ddlon,
                      y = typevalue_ddlat,
                      label = paste(coord1, coord2, sep = '_')
                    )) +
        geom_label(fill = 'grey') +
        theme_bw()
      
      jalon <- 
        ggplot(cornerCoord, aes(x=XAbs,y=YAbs, label = paste(XRel,YRel,sep='_'))) +
        geom_label(fill = 'grey') +
        theme_bw() 
      
      
      map <- 
        ggpubr::ggarrange(raw, 
                          jalon, 
                          labels = c("inputs", "outputs"),
                          ncol = 2, nrow = 1)
    } else {
      map <- NA
    }
    
    
    # Latlong and UMT projection ----------------------------------------------
    
    
    # Convertir en objet sf en UTM
    corners_utm <- 
      st_as_sf(cornerCoord %>% 
                 mutate(plot_name = all_plot$plot_name[i]), 
               coords = c("XAbs", "YAbs"), crs = codeUTM)
    
    # Convertir en objet sf latlong
    corners_latlong <- st_transform(corners_utm, crs = 4326)
    
    all_res[[i]] <-
      list(corners_utm = corners_utm,
           corners_latlong = corners_latlong,
           map = map)
    
    
  }
  
  res_utm <- 
    bind_rows(lapply(all_res, function(x) x[[1]]))
  
  res_latlong <- 
    bind_rows(lapply(all_res, function(x) x[[2]]))
  
  maps <- 
    lapply(all_res, function(x) x[[3]])
  
  return(list(res_utm = res_utm,
              res_latlong = res_latlong,
              maps = maps))
  
}


#' Divid into quadrats a 1ha plot
#'
#' Divid into quadrats a 1ha plot
#'
#'
#' @author Hugo Leblanc
#'
#' @param corners sf POINT the outputs WGS 84 of extract_corners function
#' @param plot_name vector string the name of the plot
#'
#' @importFrom leafem addStaticLabels
#' @importFrom mapview mapview
#' 
#' @return list sub_plot element is sf POLYGON with each quadrat as polygon
#' @export
divid_plot <- function (corners) {
  
  
  all_plot <- corners %>% distinct(plot_name)
  
  
  all_res <- vector('list', nrow(all_plot))
  for (i in 1:nrow(all_plot)) {
    
    corners_subset <- 
      corners %>% 
      filter(plot_name == all_plot$plot_name[i])
    
    # Extract subplot coordinates ---------------------------------------------
    
    latlong = sf::st_coordinates(corners_subset)
    
    subplot <- corners_subset %>% 
      as.data.frame() %>% 
      select(-geometry) %>% 
      mutate(XAbs = latlong[,1],
             YAbs = latlong[,2]) %>% 
      group_by(sousplot) %>%
      summarise(sousplot = unique(sousplot),
                X1 = XAbs[1],
                X2 = XAbs[2],
                X3 = XAbs[4],
                X4 = XAbs[3],
                X5 = XAbs[1],
                Y1 = YAbs[1],
                Y2 = YAbs[2],
                Y3 = YAbs[4],
                Y4 = YAbs[3],
                Y5 = YAbs[1])%>%
      mutate(X = stringr::str_split(sousplot, '_', simplify = T)[,1],
             Y = stringr::str_split(sousplot, '_', simplify = T)[,2]) %>%
      arrange(X,Y) %>%
      select(-c(X,Y))
    
    
    for (j in 1:25) {
      
      tmp  <-   st_polygon(
        list(
          rbind(
            c(X = subplot$X1[j],Y = subplot$Y1[j]),
            c(X = subplot$X2[j],Y = subplot$Y2[j]),
            c(X = subplot$X3[j],Y = subplot$Y3[j]),
            c(X = subplot$X4[j],Y = subplot$Y4[j]),
            c(X = subplot$X1[j],Y = subplot$Y1[j])
          )
        )
      )
      
      assign (paste('tttttt',j,sep = "_"), tmp)
      
    }
    
    
    # Create sf object with 25 polygons ---------------------------------------
    
    nrows <- 25
    
    sub_plot <- st_sf(crs= 4326,
                      quadrat = 1:nrows,
                      geometry = st_sfc(lapply(1:nrows,
                                               function(x) st_geometrycollection())
                      )
    )
    
    
    # Add coordinates to sf object --------------------------------------------
    
    for (j in 1:25) {sub_plot$geometry[j] <- mget(ls(pattern = "tttttt"))[[j]]}
    
    
    # Rename subplots ---------------------------------------------------------
    
    sub_plot <- sub_plot %>% 
      mutate(plot_name = all_plot$plot_name[i]) # Add the plot name
    
    sub_plot$quadrat <- as.character(sub_plot$quadrat) # Renamme the subplot id
    
    sub_plot$quadrat <- fct_recode(sub_plot$quadrat,
                                          "0_0" = '1',
                                          "0_20" = '12',
                                          "0_40" = '19',
                                          "0_60" = '20',
                                          "0_80" = '21',
                                          "20_0" = '22',
                                          "20_20" = '23',
                                          "20_40" = '24',
                                          "20_60" = '25',
                                          "20_80" = '2',
                                          "40_0" = '3',
                                          "40_20" = '4',
                                          "40_40" = '5',
                                          "40_60" = '6',
                                          "40_80" = '7',
                                          "60_0" = '8',
                                          "60_20" = '9',
                                          "60_40" = '10',
                                          "60_60" = '11',
                                          "60_80" = '13',
                                          "80_0" = '14',
                                          "80_20" = '15',
                                          "80_40" = '16',
                                          "80_60" = '17',
                                          "80_80" = '18'
    )
    
    
    # Map ---------------------------------------------------------------------
    
    map = mapview::mapview(sub_plot, zcol = "quadrat", legend = FALSE) %>%
      leafem::addStaticLabels(label = sub_plot$quadrat,
                              noHide = TRUE,
                              direction = 'top',
                              textOnly = TRUE,
                              textsize = "20px")
    
    
    print(map)
    
    
    all_res[[i]] <- 
      list(sub_plot = sub_plot, map = map)
    
   
    
    
    
  }
  
  res_ <- 
    bind_rows(lapply(all_res, function(x) x[[1]]))
  
  return(res_)

}

# Secondary functions --------------------------------------------------------

#' Get UTM from geographical coordinates
#'
#' Get UTM from geographical coordinates
#'
#'
#' @author Hugo Leblanc
#'
#' @param coord sf POINT the outputs WGS 84 of extract_corners function
#' 
#' @importFrom data.table setDF is.data.table data.table
#'
#' @export
latlong2UTM <- 
  function (coord){
  
  coord <- data.table(coord, check.names = TRUE)
  data.table::setnames(coord, colnames(coord), c("long", "lat"))
  if (!requireNamespace("proj4")) {
    stop("Please install the package 'proj4'\n\n         \t\tinstall.packages('proj4').")
  }
  codelatlong2UTM <- function(long, lat) {
    Nzone <- (floor((long + 180)/6)%%60) + 1
    Nzone <- paste0(Nzone, ifelse(lat >= 0, " +north ",
                                  " +south "))
    Nzone <- paste0("+proj=utm +zone=", Nzone, "+ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    return(Nzone)
  }
  coord[, `:=`(codeUTM, codelatlong2UTM(long, lat))]
  coord[, `:=`(c("X", "Y"), proj4::project(.(long, lat), proj = unique(.BY))),
        by = codeUTM]
  data.table::setDF(coord)
  return(coord)
}

procrust <- 
  function(X, Y) {
  xmean <- colMeans(X)
  ymean <- colMeans(Y)
  
  X <- base::scale(X, scale = FALSE)
  Y <- base::scale(Y, scale = FALSE)
  
  XY <- base::crossprod(X, Y)
  sol <- base::svd(XY)
  A <- sol$v %*% t(sol$u)
  
  b <- xmean - ymean %*% A
  
  return(list(rotation = A, translation = b))
}

bilinear_interpolation <- 
  function(coord, from_corner_coord, to_corner_coord, ordered_corner = F) {
  
  # Parameters verification
  if(nrow(from_corner_coord)!=4 | nrow(to_corner_coord)!=4 | nrow(from_corner_coord)!=nrow(from_corner_coord)) {
    stop("from_corner_coord and to_corner_coord must have 4 rows representing the 4 corners of the plot")
  }
  if(!(is.data.frame(coord) | is.matrix(coord) | data.table::is.data.table(coord))){
    stop("tree coordinates must be a data.frame, a matrix or a data.table")
  }
  if (data.table::is.data.table(coord)) coord <- data.frame(coord)
  if (data.table::is.data.table(from_corner_coord) | data.table::is.data.table(to_corner_coord)) {
    from_corner_coord <- data.frame(from_corner_coord)
    to_corner_coord <- data.frame(to_corner_coord)
  }
  
  # to_corner_coord colnames attribution
  if(is.null(colnames(to_corner_coord))) {
    to_corner_coord <- to_corner_coord[,1:2]
    colnames(to_corner_coord) <- c("x_interp","y_interp")
  }
  
  # Sorting rows if necessary
  centroid <- colMeans(from_corner_coord[,1:2])
  if(!ordered_corner) {
    # Sort from_corner_coord and to_corner_coord rows in a counter-clockwise direction
    angles <- base::atan2(from_corner_coord[, 2] - centroid[2], from_corner_coord[, 1] - centroid[1])
    from_corner_coord <- from_corner_coord[order(angles), ]
    to_corner_coord <- to_corner_coord[order(angles), ]
  }
  
  # Verification of a rectangular plot for from_corner_coord
  if(!all(abs(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)] - mean(stats::dist(rbind(from_corner_coord[,1:2],centroid))[c(4,7,9,10)]))<0.01)) {
    stop("The plot in the relative coordinate system is not a rectangle (or a square). You may consider using trustGPScorners = F")
  }
  
  x_A <- from_corner_coord[1,1] ; x_B <- from_corner_coord[2,1] ; x_C <- from_corner_coord[3,1]  ; x_D <- from_corner_coord[4,1]
  y_A <- from_corner_coord[1,2] ; y_B <- from_corner_coord[2,2] ; y_C <- from_corner_coord[3,2] ; y_D <- from_corner_coord[4,2]
  u_A <- to_corner_coord[1,1] ; u_B <- to_corner_coord[2,1];  u_C <- to_corner_coord[3,1];  u_D <- to_corner_coord[4,1] 
  v_A <- to_corner_coord[1,2] ; v_B <- to_corner_coord[2,2] ; v_C <- to_corner_coord[3,2] ; v_D <- to_corner_coord[4,2] 
  
  apply_bilinear_interpolation <- function(x,y,to_corner_coord_colnames) {
    rate_A <- (1-(x-x_A)/(x_C-x_A)) * (1-(y-y_A)/(y_C-y_A))
    rate_B <- (1-(x-x_B)/(x_D-x_B)) * (1-(y-y_B)/(y_D-y_B))
    rate_C <- (1-(x-x_C)/(x_A-x_C)) * (1-(y-y_C)/(y_A-y_C))
    rate_D <- (1-(x-x_D)/(x_B-x_D)) * (1-(y-y_D)/(y_B-y_D))
    interp_df <- data.frame(
      rate_A*u_A + rate_B*u_B + rate_C*u_C + rate_D*u_D,
      rate_A*v_A + rate_B*v_B + rate_C*v_C + rate_D*v_D
    )
    data.table::setnames(interp_df, new = to_corner_coord_colnames)
    interp_df
  }
  
  return(apply_bilinear_interpolation(x=coord[,1],y=coord[,2],to_corner_coord_colnames=colnames(to_corner_coord)[1:2]))
}



#' Project stems in geographical space
#'
#' Project stems in geogaphical space
#'
#'
#' @author Gilles Dauby
#'
#' @param coord_sf sf polygon output of query_plots, using show_all_coordinates TRUE
#' @param coord_rel tibble extract of query_plots individuals with relative coordinates
#' 
#' @details
#' The coord_rel should have the columns x_100 and y_100 that are the relative coordinates in the plot
#'  
#' @export
proj_rel_xy <- 
  function(coord_sf,
           coord_rel) {
    
    coord_rel <- coord_rel %>% ungroup()
    
    res_list <- vector('list', length(unique(coord_rel$plot_name)))
    utm_plot_list <- vector('list', length(unique(coord_rel$plot_name)))
    for (i in 1:length(unique(coord_rel$plot_name))) {
      
      square_geo <- 
        coord_sf %>% filter(plot_name == unique(coord_rel$plot_name)[i])
      
      utm_plot <- 
        latlong2UTM(coord = st_coordinates(square_geo)[, c("X", "Y")])
      
      utm_plot_list[[i]] <- 
        tibble(plot_name = unique(coord_rel$plot_name)[i], 
               utm = utm_plot$codeUTM[1])
      
      square_proj <- 
        st_transform(square_geo, utm_plot$codeUTM[1])
      
      square_matrix <- st_coordinates(square_proj)[1:4, ]  # Get the 4 corners
      origin <- 
        square_matrix[1, 1:2]  # First point (assumed to be origin)
      v1 <- square_matrix[2, 1:2] - origin  # Local x direction (from pt 1 to 2)
      v2 <- square_matrix[4, 1:2] - origin  # Local y direction (from pt 1 to 4)
      
      relative_coords <- 
        coord_rel %>% 
        filter(plot_name == unique(coord_rel$plot_name)[i]) %>% 
        select(x_100, y_100, id_n)
      
      size <- 100
      
      absolute_coords <- t(apply(relative_coords, 1, function(pt) {
        origin + pt[1] * v1 / size + pt[2] * v2 / size
      }))
      
      reproj_coords <- 
        absolute_coords %>% 
        as_tibble() %>% 
        mutate(id_n = relative_coords$id_n,
               plot_name = unique(coord_rel$plot_name)[i])
      
      res_list[[i]] <- 
        reproj_coords
    }
    
    return(list(coords = bind_rows(res_list),
                utm_code = bind_rows(utm_plot_list)))
    
    
    
  }

#' Project stems in geographical space
#'
#' Project stems in geogaphical space
#'
#'
#' @author Gilles Dauby
#'
#' @param dataset extract of query_plots individuals TRUE
#' 
#' @details
#' Add columns x_100 and y_100 with relative position in plot based on position_x_iphone and positions_y_iphone
#' 
#' 
#'  
#' @export
get_plot_rel_xy <- function(dataset,
                            col_subplot = "quadrat",
                            col_pos_x = "position_x_iphone",
                            col_pos_y = "position_y_iphone") {
  
  col_subplot <- rlang::ensym(col_subplot)
  col_pos_x   <- rlang::ensym(col_pos_x)
  col_pos_y   <- rlang::ensym(col_pos_y)
  
  dataset %>%
    mutate(
      x_quadrat = as.numeric(stringr::str_split(!!col_subplot, "_", simplify = TRUE)[, 1]),
      y_quadrat = as.numeric(stringr::str_split(!!col_subplot, "_", simplify = TRUE)[, 2]),
      x_100 = !!col_pos_x + x_quadrat,
      y_100 = !!col_pos_y + y_quadrat
    )
}


#' Interpolate x y position based on neighnour 
#'
#' Experimental - Interpolate x y position based on neighnour, for isolated missing value only
#'
#'
#' @author Gilles Dauby
#'
#' @param dataset extract
#' @param col_subplot string column name of quadrat
#' @param col_plot string column name of plot
#' @param col_pos_x numeric column name of x relative positioning within quadrat
#' @param col_pos_y numeric column name of y relative positioning within quadrat
#' @param tag numeric tag of individual within plot
#' 
#' @details
#' Interpolate and fill x and y positioning
#' 
#' @importFrom zoo na.approx
#'  
#' @export
approximate_isolated_xy <- function(dataset,
                                    col_subplot = "quadrat",
                                    col_plot = "plot_name",
                                    col_pos_x = "position_x_iphone",
                                    col_pos_y = "position_y_iphone",
                                    tag = "tag") {
  
  col_subplot <- rlang::ensym(col_subplot)
  col_pos_x   <- rlang::ensym(col_pos_x)
  col_pos_y   <- rlang::ensym(col_pos_y)
  col_plot   <- rlang::ensym(col_plot)
  tag   <- rlang::ensym(tag)
  
  all_unique_plots <- unique(dataset$plot_name)
  res_plots <- vector("list", length(all_unique_plots))
  for (i in 1:length(all_unique_plots)) {
    
    df <- 
      dataset %>% 
      filter(!!col_plot == all_unique_plots[i]) %>% 
      rename(tag = !!tag,
             x = !!col_pos_x,
             y = !!col_pos_y)
    
    df_filled <- 
      df %>%
      arrange(tag) %>%
      group_by(!!col_subplot) %>%   # stay inside quadrats
      mutate(
        x_filled = if (any(!is.na(x))) zoo::na.approx(x, x = tag, na.rm = FALSE) else x,
        y_filled = if (any(!is.na(y))) zoo::na.approx(y, x = tag, na.rm = FALSE) else y
      ) %>%
      ungroup() %>% 
      rename(!!col_pos_x := x_filled,
             !!col_pos_y := y_filled,
             !!tag := tag) %>% 
      select(-x, -y)
    
    res_plots[[i]] <- df_filled
  }
  
  
  return(bind_rows(res_plots))
  
}