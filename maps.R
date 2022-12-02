#Map
plot_map <- function(df, country_col, ctype, data_type, color1, color2, color3){
  tmap_mode("view")
  
  if (ctype == "names"){
    df$iso_a3 = iso.alpha(df[,country_col],3)
  }
  
  if (ctype == "a2"){
    df$ccc = NA
    for ( i in 1:nrow(df) ) {
      code = df[i,country_col]
      if (code=="EL") {
        df$ccc[i] = "Greece" ### in ECDC data, 2-letter code for Greece shows up as "EL" instead of "GR" 
      }
      else{
        idx = match(code, iso3166$a2)
        if(!is.na(idx)){
        df$ccc[i] = iso3166$ISOname[idx]
        }
      }
    }
    df$iso_a3 = iso.alpha(df$ccc,3)
  }
  if (ctype == "a3"){
    colnames(df)[which(names(df) == country_col)] <- "iso_a3"
  }

  joined_df = merge(World, df, by="iso_a3")
  
  
  if (is.null(color3)){
    pal <- colorRampPalette(c(color1, color2))
  }
  else{
    pal <- colorRampPalette(c(color1, color3, color2))
  }

  p = tm_shape(joined_df) +
    tm_polygons(col=data_type, palette=pal(3), style="cont", popup.vars=c(country_col, data_type)) +
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
