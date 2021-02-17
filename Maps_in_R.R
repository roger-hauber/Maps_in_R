##### Load ppackages
library(sf)
library(ggplot2)

#set bland and white theme
theme_set(theme_bw())

# load rnaturalearth (for world map) and rnaturalearthdata for data

library(rnaturalearth)
library(rnaturalearthdata)
library(rgeos)

#pull world map via ne_countries (scale = medium and class = "sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

#create and plot world map

ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World Map", subtitle = paste0("(", length(unique(world$name)), " countries)"))

# now a map of the world with countries filled according to their population (pop_est in wolrd data frame)
# geom_sf() takes aes() like any other geom

ggplot(data = world) +
  geom_sf(aes(fill = pop_est))

# now the same using scale_fill_viridis_c() with option = plasma
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma")

# and now the same with trans = sqrt (presumably to differentiate the colours better??)
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt") 



# select different projections via the sf_coord() function in separate layer of the ggplot
# use st_crs function in crs argument: 3035 is a code for a 
#european_centric ETRS Lambert Azimuthal Equal Area projection

ggplot(data = world) +
  geom_sf() +
  coord_sf(crs = st_crs(3035))

#or we can use coord_sf to set x and y limits (by longitude and latitude respectively)  
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c((1/60 + 52)/60 + 5, (37/60 + 2)/60 + 15), 
           ylim = c((18/60 + 16)/60 + 47, (4.6/60 + 5)/60 + 55), expand = F)

#write custom function to calculate decimal coord from minutes/seconds
min_to_dec <- function(deg, min, sec){
  dec_crd <- (sec + min)/60 + deg
  return(dec_crd)
}

# test function by zooming to UK
ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(min_to_dec(-2, -25, 0)-5,min_to_dec(-2, -25, 0)+5), 
           ylim = c(min_to_dec(53, 50, 0)-5,min_to_dec(53, 50, 0)+5), expand = T)
#add london
ggplot(data = world) +
  geom_sf() +
  geom_point(aes(x = -0.118092, y = 51.509865), size = 1) +
  coord_sf(xlim = c(min_to_dec(-2, -25, 0)-5,min_to_dec(-2, -25, 0)+5), 
           ylim = c(min_to_dec(53, 50, 0)-5,min_to_dec(53, 50, 0)+5), expand = T)

## now let's do it for europe with country labels
# extract centroid points of countries from world via st_centroid
country_cent <- cbind(world, st_coordinates(st_centroid(world$geometry)))

eur_lab <- ggplot(data = world) +
  geom_sf() +
  geom_text(data = country_cent, aes(x = X, y = Y, label = name), size = 2) +
  coord_sf(xlim = c(min_to_dec(-9, -30, -2.9), min_to_dec(69, 5, 21)),
           ylim = c(min_to_dec(36, 0, 24), min_to_dec(71, 7, 59)))

# this has a bunch of overlapping labels --> apparently theres a package that can help
install.packages("ggrepel")
library(ggrepel)

eur_lab <- ggplot(data = world) +
  geom_sf() +
  geom_text_repel(data = country_cent, aes(x = X, y = Y, label = name), size = 2) +
  coord_sf(xlim = c(min_to_dec(-9, -30, -2.9), min_to_dec(69, 5, 21)),
           ylim = c(min_to_dec(36, 0, 24), min_to_dec(71, 7, 59)))
#this plots all data and adds labels and arrows for countries outside of x and y limits..
# subset country_cent by centroid coordinates(x and y) within xlim and ylim boundaries
eur_lab <- ggplot(data = world) +
  geom_sf() +
  geom_text_repel(data = country_cent[country_cent$X > -9 & country_cent$X < 69 &
                                        country_cent$Y > 36 & country_cent$Y < 70, ], 
                  aes(x = X, y = Y, label = name), size = 2, force = 0.5, force_pull = 2) +
  coord_sf(xlim = c(min_to_dec(-9, -30, -2.9), min_to_dec(69, 5, 21)),
           ylim = c(min_to_dec(36, 0, 24), min_to_dec(71, 7, 59)))

##### load rds of germany (level 3) see GADM Project Metadata 
# use gg_i for i in levels of germany for unique identification according to statistisches Bundesamt
germany_lvl_4 <- readRDS("gadm36_DEU_4_sf.rds")

germ_map <- ggplot(data = germany_lvl_4) +
  geom_sf()

#first letter of town

first_letter_town <- character()
for (i in germany_lvl_4$NAME_4) {
  first_letter_town <- c(first_letter_town, strsplit(i, split = "", fixed = T)[[1]][1])
}

# add to germany
germany_lvl_4 <- cbind(germany_lvl_4, first_letter = as.factor(first_letter_town))

#plot first letters by town
germ_map <- ggplot(data = germany_lvl_4) +
  geom_sf(aes(fill = first_letter), colour = NA)+
  ggtitle("Deutsche Gemeinden nach Anfangsbuchstaben") +
  labs(fill = "Anfangsbuchstabe")+
  theme(panel.border = element_blank())+
  theme(panel.grid = element_blank())+
  theme(axis.line = element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(legend.box.just = "center")

ggsave(plot = germ_map, filename = "Gemeinden_nach_Anfangsbuchstabe.jpg", device = "jpg")

  

