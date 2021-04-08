###Bionomics and Ecology of Anopheles merus along the East and Southern Africa coast 
##Written by: Brian Bartilol


# import the mosquito collection data -------------------------------------


spatial_distribution <- read.csv("Anopheles_merus_collection_data_version_3.csv") 

# Figure 2 ---------------------------------------------------------------

##mapping the collection sites

#load packages

library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
theme_set(theme_bw())
library(sf)
library(ggrepel)
library(tidyverse)


mos_map_data <- spatial_distribution %>% 
  distinct(location, .keep_all = TRUE)# drop off repeated column names


world <- ne_countries(scale = "medium", returnclass = "sf")
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))#country names

##map

my_plot <- ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "blue", size = 2, fontface = "bold", check_overlap = FALSE) + #country names
  geom_point(data = mos_map_data, aes(x = longitude, y = latitude), size = 2, fill = "black") +
  geom_text_repel(data = mos_map_data, aes(x = longitude, y = latitude, 
                                           label = location), size = 3,
                  force = 10, max.iter = 50) +
  xlab("longitude")+
  ylab("latitude") +
  coord_sf(xlim = c(22, 60), ylim = c(1, -35.0), expand = TRUE)#size of map 

my_plot


tiff(filename = "Figure 2.png",width = 8,  height = 8, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(my_plot)
dev.off()


# Figure 3 ----------------------------------------------------------------

##bar plot of proportions An gambiae complex members in the East and Southern Africa region

#load packages
library(ggpubr)
library(dplyr)

#Plot
mos_count <- spatial_distribution[-c(29, 68, 69, 70),] %>% #drop Inyamithe, Makanis Drift and Soutini does not have count data
  gather(key = "species",
         value = "count",
         An_merus, An_arabiensis, An_gambiae, An_quadriannulatus)#gather the data to ease charting

loc_facet_region <- ggplot(mos_count, aes(location, count, fill = species))+
  geom_histogram(stat = "identity", position = "fill") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_discrete(labels = c("An arabiensis", "An gambiae", "An merus", "An quadriannulatus")) +
  ylab("proportions") +
  theme_pubr() +
  facet_grid(~country, scales = "free", space = "free") +
  theme(strip.text.x = element_text(size = 7,face = "bold", colour = "black",),#facet x font size
        axis.text.x = element_text(angle = 60, hjust = 1,size = 8), #adjust appearance of x-axis labels
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.line = element_blank(),
        legend.text = element_text(face = "italic"),
        legend.position = "top") +
  facet_wrap(~region, nrow = 2, scales = "free_x")

loc_facet_region

tiff(filename = "Figure 3.png",width = 8,  height = 5, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(loc_facet_region)
dev.off()


# Figure 4 ----------------------------------------------------------------

##species distribution and divesity

#load packages

library(tabula)
library(magrittr)
library(khroma)
library(data.table)
library(tidyverse)
library(plyr)
library(vegan)

#load data
#omit data identified by salt tolerance, then combine mosquito counts from same location

ento_diversity_data <- spatial_distribution[-c(29, 68, 69, 70),] %>% #drop Inyamithe, Makanis Drift and Soutini does not have count data
  filter(identification_method != "salt tolerance") %>% #filter off mosquitoes identified by salt tolerance test
  select(location, An_merus, An_arabiensis, An_gambiae, An_quadriannulatus) %>% 
  group_by(location) %>% 
  summarise_each((funs(sum)))# combine mosquito counts of the various locations


#species richness
species_richness <- ddply(ento_diversity_data,~location,function(x) {
  data.frame(Species_richness=sum(x[-1]>0))
})

##simpson index

simpson_index <- ddply(ento_diversity_data,~location,function(x) {
  data.frame(Simpson_dominance_index=diversity(x[-1], index="simpson"))
})

#Shannon-weiener_Index
Shannon_Weiner <- ddply(ento_diversity_data,~location,function(x) {
  data.frame(Shannon_weiner_index=diversity(x[-1], index="shannon"))
})

#Pilou_Evenness
Pilou_eveness <- ddply(ento_diversity_data,~location,function(x) {
  data.frame(Pilou_evenness=diversity(x[-1], index="simpson")/log(sum(x[-1]>0)))
})

#Margalefs index

menhinick <- function(x) {
  (sum(x>0)-1)/log(sum(x))
}

Margalefs_index <- ddply(ento_diversity_data,~location,function(x) {
  data.frame(Margalefs_index=menhinick(x[-1]))
})

#combine all the output
entomological_indices <- join_all(list(species_richness,simpson_index, 
                                       Shannon_Weiner, Pilou_eveness, Margalefs_index), by='location', type='left')

##gather data and plot

jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow",
                                 "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)

entomological_indices_plot <- gather(data = entomological_indices,
                                     key = "index",
                                     value = "value",
                                     Species_richness, Simpson_dominance_index, Shannon_weiner_index, Margalefs_index, Pilou_evenness) %>% 
  ggplot(aes(location, index, fill= value)) +
  geom_tile() +
  scale_x_discrete(expand = c(0,0)) +
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8)) +
  coord_equal() 


tiff(filename = "Figure 4.png",width = 9,  height = 3, units = 'in',
     res = 600, compression = "lzw", type = "cairo")
print(entomological_indices_plot)
dev.off()

entomological_indices_plot


#comparison statistics --------------------------------------------------------------


#select district, location, region and gather mosquito count

mosquito_data <- spatial_distribution %>% 
  select(country, district, location, region, An_merus, An_arabiensis, An_gambiae, An_quadriannulatus) %>% 
  gather(key = "species",
         value = "count",
         An_merus, An_arabiensis, An_gambiae, An_quadriannulatus)

#comparisons of mosquito counts between East and Southern Africa
#Anopheles merus
merus_east_south <- filter(mosquito_data, species=="An_merus") 

wilcox.test(count ~ region, data = merus_east_south,
            exact = FALSE)

#Anopheles quadriannulatus
An_quadriannulatus_east_south <- filter(mosquito_data, species=="An_quadriannulatus") 

wilcox.test(count ~ region, data = An_quadriannulatus_east_south,
            exact = FALSE)

#Anopheles arabiensis
An_arabiensis_east_south <- filter(mosquito_data, species=="An_arabiensis") 

wilcox.test(count ~ region, data = An_arabiensis_east_south,
            exact = FALSE)

##Anopheles gambiae
An_gambiae_east_south <- filter(mosquito_data, species=="An_gambiae") 

wilcox.test(count ~ region, data = An_gambiae_east_south,
            exact = FALSE)

#*************
#END
#*************

