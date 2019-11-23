library("ggmap")
library("ggplot2")
library("rayshader")
library("hexbin")
library("stringr")

#get data
nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

#set theme
themez = theme(panel.border = element_rect(colour = "white", fill=NA, size=2),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 axis.line = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank(),
                 legend.key = element_blank(),
                 plot.margin = unit(c(0.5, 0, 0, 0), "cm"))
                 
# get coordinates for map boundaries
left <- min(nyc_squirrels$long) - 0.005
right <- max(nyc_squirrels$long) + 0.005
top <- max(nyc_squirrels$lat) + 0.005
bottom <- min(nyc_squirrels$lat) - 0.005

# get the map
map <- get_stamenmap(bbox = c(left = left, right = right, top = top, bottom = bottom), zoom = 13, maptype = "toner")

# plot lat/lon data on the map
g <- ggmap(map, base_layer = (ggplot(data = nyc_squirrels, aes(x = long, y = lat)))) + coord_cartesian() + geom_hex(aes(colour = ..count..), bins = 50) + scale_fill_viridis_c() + scale_color_viridis_c() + themez + labs(fill=str_wrap('Number of Squirrels', width= 10)) + guides(color = FALSE)

# create layer with just hexagons to be converted to 3d
g_hex <- ggmap(map, base_layer = (ggplot(data = nyc_squirrels, aes(x = long, y = lat))), darken = c(1, "white"))  + coord_cartesian()  + geom_hex(aes(colour = ..count..), bins = 50)  + scale_fill_viridis_c() + scale_color_viridis_c() + themez + labs(fill=str_wrap('Number of Squirrels', width= 10)) + guides(color = FALSE)

# use rayshader to covert fill color to 3d
ggheight = plot_gg(list(g,g_hex), height_aes = "fill", width=5,height=9, scale = 600)

# render movie
render_movie("tidytuesday.mp4", type = "orbit", zoom = 0.8,
  title_text = "#TidyTuesday", title_offset = c(20, 20), title_color = "black",
  title_size = 30, title_font = "arial")
 
