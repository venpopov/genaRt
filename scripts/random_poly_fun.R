library(ggplot2)
gen_random_polygons <- function() {
  # generate N polygons each with M vertices and 
  # fill each polygon with a different color then
  # plot in polar coordinates
  
  N=sample(1:10,1)
  M=sample(1:20,1)
  color_ids = sample(colors(), N)
  
  matdat <- data.frame(
    ids = rep(1:N, each=M),
    x = runif(N*M),
    y = runif(N*M),
    value = rep(color_ids, each=M)
  )
  
  ggplot(matdat, aes(x,y, group=ids)) +
    geom_polygon(fill=matdat$value) +
    theme_void() +
    coord_polar()
}
