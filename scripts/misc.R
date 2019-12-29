library(tidyverse)
library(randomcoloR)
theme_set(theme_void())
dat <- data.frame(x=1,y=1)


# remove axes and plot a blue square
ggplot(dat, aes(x,y)) +
  theme_void() +
  theme(plot.background = element_rect(fill='blue'))
ggsave('img/blue_square.jpg', w=4, h=4)


# selec a color manually
ggplot(dat, aes(x,y)) +
  theme_void() +
  theme(plot.background = element_rect(fill='deepskyblue2')) +
  ggsave('img/deepskyblue_square.png', w=3, h=3)


# make a 2x2 matrix with different colors
matdat <- data.frame(
  ids = rep(1:4, each=4),
  x = c(0,1,1,0, 1,2,2,1, 0,1,1,0, 1,2,2,1),
  y = c(0,0,1,1, 0,0,1,1, 1,1,2,2, 1,1,2,2),
  value = rep(c("#FFB90F", "#FF8C00", "#1E90FF", "#00BFFF"), each=4)
)
ggplot(matdat, aes(x,y, group=ids)) +
  geom_polygon(fill=matdat$value) +
  theme_void() +
  theme(legend.position = 'none') +
  ggsave('img/4mat.png', w=3, h=3)

#TODO: can I make a function (or does one exist) that selects compatible colors?

# same as last one but in polar coordinate
ggplot(matdat, aes(x,y, group=ids)) +
  geom_polygon(fill=matdat$value) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_polar() +
  ggsave('img/polar4.png', w=10, h=10)


#### select polygon coords and colors at random ####
set.seed(19198118)
N=4
M=10
color_ids = colors()[sample(length(colors()), N)]
matdat <- data.frame(
  ids = rep(1:N, each=M),
  x = runif(N*M),
  y = runif(N*M),
  value = rep(color_ids, each=M)
)
ggplot(matdat, aes(x,y, group=ids)) +
  geom_polygon(fill=matdat$value) +
  theme_void() +
  theme(legend.position = 'none') +
  coord_polar()
ggsave('img/swirl.png', w=5, h=5)

#### make the previous into a function
set.seed(19198118)
gen_random_polygons <- function(N=sample(1:10,1), M=sample(1:20,1), coord_type='polar') {
  color_ids = colors()[sample(length(colors()), N)]
  matdat <- data.frame(
    ids = rep(1:N, each=M),
    x = runif(N*M),
    y = runif(N*M),
    value = rep(color_ids, each=M)
  )
  p <- ggplot(matdat, aes(x,y, group=ids)) +
    geom_polygon(fill=matdat$value) +
    theme_void() +
    theme(legend.position = 'none')
  attr(p, 'N') <- N
  attr(p, 'M') <- M
  if (coord_type=='cartesian') {
    return(p) 
  } else {
    return(p+coord_polar())
  }
  return(p)
}
gen_random_polygons(coord_type = 'polar')

#### use the previous function to generate a lot of random outputs ####
# Record the seed for each output and append to file.name to reproduce the interesting ones afterwards

set.seed(100)
seeds <- sample(1:2^15, 1000)
for (i in 1:1000) {
  set.seed(seeds[i])
  gen_random_polygons(coord_type = 'polar')
  path = paste0('img/rswirl/',seeds[i],'.png')
  ggsave(path, w=5, h=5)
}

set.seed(14259)
gen_random_polygons(coord_type='polar')

# generate the 16 best but this time in cartesian coords
paths = dir('img\\selected_swirl\\favs\\')
seeds = substr(paths,0,nchar(paths)-4) %>% as.numeric()
for (i in 1:length(seeds)) {
  set.seed(seeds[i])
  gen_random_polygons()
  path = paste0('img/rswirl_cart/',seeds[i],'.png')
  ggsave(path, w=5, h=5)
}

##### with holes
gen_random_polygons_holes <- function(N=sample(2:10,1), M=sample(2:15,1), coord_type='polar') {
  color_ids = colors()[sample(length(colors()), N)]
  matdat <- data.frame(
    ids = rep(1:N, each=M),
    x = runif(N*M),
    y = runif(N*M),
    value = rep(color_ids, each=M)
  )
  holes <- do.call(rbind, lapply(split(matdat, matdat$id), function(df) {
    df$x <- df$x + runif(1) * (mean(df$x) - df$x)
    df$y <- df$y + runif(1) * (mean(df$y) - df$y)
    df
  }))
  matdat$subid <- 1L
  holes$subid <- 2L
  matdat <- rbind(matdat, holes)
  
  p <- ggplot(matdat, aes(x,y, group=ids, subgroup=subid)) +
    geom_polygon(fill=matdat$value) +
    theme_void() +
    theme(legend.position = 'none') +
    coord_polar()
  attr(p, 'N') <- N
  attr(p, 'M') <- M
  return(p)
}

set.seed(10330)
seeds <- sample(1:2^15, 2000)
for (i in 1:2000) {
  set.seed(seeds[i])
  gen_random_polygons_holes(coord_type = 'polar')
  path = paste0('img/rswirl_holes/',seeds[i],'.png')
  ggsave(path, w=5, h=5)
}


##### rainbow palette
gen_random_polygons_holes_rainbow <- function(N=sample(2:10,1), M=sample(2:15,1), coord_type='polar') {
  color_ids = heat.colors(N)
  matdat <- data.frame(
    ids = rep(1:N, each=M),
    x = runif(N*M),
    y = runif(N*M),
    value = rep(color_ids, each=M)
  )
  holes <- do.call(rbind, lapply(split(matdat, matdat$id), function(df) {
    df$x <- df$x + runif(1) * (mean(df$x) - df$x)
    df$y <- df$y + runif(1) * (mean(df$y) - df$y)
    df
  }))
  matdat$subid <- 1L
  holes$subid <- 2L
  matdat <- rbind(matdat, holes)
  
  p <- ggplot(matdat, aes(x,y, group=ids, subgroup=subid)) +
    geom_polygon(fill=matdat$value) +
    theme_void() +
    theme(legend.position = 'none') +
    coord_polar()
  attr(p, 'N') <- N
  attr(p, 'M') <- M
  return(p)
}

gen_random_polygons_holes_rainbow()

set.seed(10330)
seeds <- sample(1:2^15, 2000)
for (i in 1:2000) {
  set.seed(seeds[i])
  gen_random_polygons_holes_rainbow(coord_type = 'polar')
  path = paste0('img/rswirl_holes/',seeds[i],'.png')
  ggsave(path, w=5, h=5)
}

