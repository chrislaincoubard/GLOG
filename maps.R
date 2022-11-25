#Map 
plot_map <- function(df, country_col, data_type, color1, color2){
  tmap_mode("view")
  
  df$iso_a3=iso.alpha(df[,country_col],3)
  joined_df = merge(World, df, by="iso_a3")
  
  pal <- colorRampPalette(c(color1, color2))

  p = tm_shape(joined_df) +
    tm_polygons(col=data_type, palette=pal(2), style="cont", popup.vars=c(country_col, data_type)) +
    tm_view(set.view = c(10,30,1.5))
  
  return(p)
}


# Map displayed when no file is selected (world basemap + grey countries and borders)
plot_empty_map <- function(){
  tmap_mode("view")
   p = tm_shape(World) +
     tm_fill(col = "#eaeaea") +
     tm_borders(col = "#d0cfd4") +
     tm_view(set.view = c(10,30,1.5))
    
   return(p)
}
