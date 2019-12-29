library(ggplot2)
rswirl <- function(N=sample(2:10,1), M=sample(2:15,1), holes=TRUE, seed=NULL) {
  # generate N polygons each with M vertices and 
  # fill each polygon with a different color then
  # plot in polar coordinates
  if (!is.null(seed)) set.seed(seed)
  color_ids = sample(colors(), N)
  
  matdat <- data.frame(
    ids = rep(1:N, each=M),
    x = runif(N*M),
    y = runif(N*M),
    value = rep(color_ids, each=M),
    subid = 1L
  )
  
  # add holes to polygons if wanted
  if (holes) {
    holes <- do.call(rbind, lapply(split(matdat, matdat$id), function(df) {
      df$x <- df$x + runif(1) * (mean(df$x) - df$x)
      df$y <- df$y + runif(1) * (mean(df$y) - df$y)
      df
    }))
    holes$subid <- 2L
    matdat <- rbind(matdat, holes)
  }
  
  ggplot(matdat, aes(x,y, group=ids, subgroup=subid)) +
    geom_polygon(fill=matdat$value) +
    theme_void() +
    coord_polar()
}
