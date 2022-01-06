rasterize_ranking <- function(rank_df){

  rank_raster <- ocean_df %>%
    filter(f_eez > 0) %>%
    left_join(rank_df) %>%
    replace(is.na(.), 0) %>%
    arrange(pick_order) %>%
    mutate(rank = 1 - scales::rescale(base::rank(pick_order, ties.method = "first", na.last = F)),) %>%
    select(lon, lat, rank, f_highly_mpa) %>%
    rasterFromXYZ(crs = crs(ocean_low_res_moll))

  return(rank_raster)
}

map_priorities <- function(rank_raster,
                           priority,
                           scenario,
                           projection,
                           land_shp,
                           mpas_shp){

  prj_raster_df <- rank_raster %>%
    projectRaster(crs = projection, method = c("ngb"), over = T) %>%
    crop(as_Spatial(st_transform(eez, projection))) %>%
    mask(as_Spatial(st_transform(eez, projection))) %>%
    as.data.frame(xy = T) %>%
    set_names(c("lon", "lat", "rank", "f_highly_mpa")) %>%
    filter(!is.na(rank) | f_highly_mpa > 0)

  min_mpa_local_rank <- prj_raster_df %>%
    summarise_at(c("rank"), ~min(.x[f_highly_mpa >0], na.rm = T)) %>%
    pull(rank)

  min_mpa_local_rank <- pmin(1, min_mpa_local_rank)

  mpas_shp <- st_transform(mpas_shp, crs = projection)

  land_shp <- st_transform(land_shp, crs = projection)

  local_breaks <- sort(unique(c(global_breaks, min_mpa_local_rank)))

  local_labels <- local_breaks[-1]

  local_colors <- global_cols

  local_colors[local_labels > min_mpa_local_rank] <- "lightblue"

  #local_colors[1:4] <- viridis::viridis(n = 4)

  local_labels <- 100 - 100*local_labels

  local_labels[local_labels ==  (100-100*min_mpa_local_rank)] <- " "

  local_legend_key_widths <- tibble(to = as.numeric(as.character(local_breaks[-1])),
                                    from = lag(to)) %>%
    replace_na(list(from = 0)) %>%
    mutate(range = to - from) %>%
    mutate(w = range*0.6) %>%
    mutate(w2 = if_else(to <= 0.8, w/1.5, w))

  map <- prj_raster_df %>%
    mutate_at(vars("rank"),
              .funs = ~cut(.x,
                           breaks = local_breaks,
                           labels = local_breaks[-1],
                           include.lowest = T)) %>%
    replace_na(list(rank = 1)) %>%
    ggplot(aes(x = lon, y = lat))+
    geom_raster(aes(fill = rank), alpha = 1)+
    geom_sf(data = mpas_shp,
            inherit.aes = F, fill = "lightblue", col = "transparent") +
    geom_sf(data =  land_shp,
            inherit.aes = F, fill = "gray", col = "transparent",linetype = 2, size = 0.5)+
    scale_fill_manual(na.value = "black",
                      values  = local_colors,
                      labels = local_labels,
                      guide = guide_legend(title = "Top % of EEZ",
                                           direction = "horizontal",
                                           keyheight = unit(0.01, "npc"),
                                           keywidth = unit(rev(local_legend_key_widths$w2), "npc"),
                                           title.position = 'top',
                                           title.hjust = 0.5,
                                           label.hjust = -.02,
                                           nrow = 1,
                                           byrow = T,
                                           reverse = T,
                                           label.position = "bottom"))+
    labs(title = paste("Conservation Priorities:", priority, sep = " "),
         subtitle = scenario)+
    easyR::ggtheme_map()

  return(map)

}
