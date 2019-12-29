rpalette <- function(N, color_seed=NULL) {
  # generate a color palette of length N. Either provide a 
  # color_seed, or let it randomly generate. The funciton then
  # selects complementary and ajacent colors from the seed and
  # adds some blacks and greys
  
  # extract non-bw colors from base
  if (N > 9) stop('Number of colors should be <= 9')
  
  color_set <- colors()
  bw_idx <- grepl('grey',color_set, fixed=T) | grepl('white', color_set, fixed=T) | grepl('black', color_set, fixed=T) | grepl('gray',color_set, fixed=T)
  bw <- color_set[bw_idx]
  color_set <- color_set[!bw_idx]
  
  # use one color as seed and generate adjacent and complementary colors
  if (is.null(color_seed)) color_seed <- sample(color_set, 1) 
  
  my_palette <- c('black',
                  sample(bw,2),
                  colortools::adjacent(color_seed, plot=F)[-1],
                  colortools::splitComp(color_seed, plot=F)[-1],
                  colortools::complementary(color_seed, plot=F)[-1])
  use_colors <- c(color_seed, sample(my_palette, N-1))
  use_colors <- unique(use_colors)
  return(use_colors)
}


plot_palette <- function(palette) {
  N = length(palette)
  qplot(x=1:N, y = 1, fill=factor(1:N), geom="tile") +
    scale_fill_manual(values = palette) +
    theme_void()+
    theme(legend.position="none") 
}

areColors <- function(x) {
  # checks if elements of a vector are colors
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
           error=function(e) FALSE)
  })
}
