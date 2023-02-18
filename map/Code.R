# Script to make the map (figure 2)

rm(list=ls())

library(tidyverse)
library(rgdal)
library(sf)
library(maps)
library(ggnewscale)
library(ggspatial)
library(grid)
library(ggplotify)

PEP <- read.csv("../parametrisation/MaxPotGrowth/data/PEP_climateNA_1970-2019MSY.csv") %>% 
  select(ID1, Latitude, Longitude) %>% unique() %>% 
  filter(!ID1 %in% c("8809410701", "8809411502")) %>% mutate(plot_name=as.character(1:10))
LDRTF <- read.csv("../parametrisation/MaxPotGrowth/data/LDRTF_climateNA_1970-2019MSY.csv") %>% 
  select(ID1=ID2, Latitude, Longitude) %>% unique() %>%
  mutate(plot_name="")

dom_bio <- readOGR(dsn = "data/sous_dom_bioclim.shp",
                   layer = "sous_dom_bioclim")

lim_ers <- readOGR(dsn = "data/Acer_saccharum_north_boundary.shp",
                   layer = "Acer_saccharum_north_boundary")
lim_ers <- spTransform(lim_ers, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lim_err <- readOGR(dsn = "data/Acer_rubrum_north_boundary.shp",
                   layer = "Acer_rubrum_north_boundary")
lim_err <- spTransform(lim_err, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lim_boj <- readOGR(dsn = "data/Betula_alleghaniensis_north_boundary.shp",
                   layer = "Betula_alleghaniensis_north_boundary")
lim_boj <- spTransform(lim_boj, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

lims <- raster::bind(lim_ers, lim_err, lim_boj)

PEP <- PEP %>% mutate(ID="Validation plots") %>% select(-ID1)
LDRTF <- LDRTF %>% mutate(ID="LDRTF") %>% select(-ID1)
data <- rbind(PEP, LDRTF)

# Get the BF-YB and BF-PB domains
dom_bio_map <- subset(dom_bio, dom_bio$DOM_BIO %in% c("4", "5"))


dom_bio_map <- spTransform(dom_bio_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

canada <- sf::st_as_sf(map('world', regions="canada", plot = FALSE, fill = TRUE))
world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

lim_bio <- dom_bio_map@bbox

dom_bio_map <- st_as_sf(dom_bio_map) 

dom_bio_map  = st_sf(
  aggregate(
    dom_bio_map,
    by=list(DOM_BIO=dom_bio_map$DOM_BIO),
    FUN=function(vals){vals[1]}))

lims <- st_as_sf(lims)
lims$Species <- c("Sugar maple", "Red maple", "Yellow birch")

color_blind <- c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible

main <- ggplot()+ geom_sf(data = canada) +
  geom_sf(data=dom_bio_map, aes(fill=DOM_BIO.1), alpha=0.4) +
  scale_fill_manual(values=c(color_blind[5], color_blind[4]),
                    breaks = c("4", "5"),
                    labels = c("Balsam fir -\nyellow birch", "Balsam fir -\npaper birch")) +
  labs(fill="Bioclimatic domain") +
  geom_sf(data=lims, aes(color=Species), size=1.5, linetype="dashed") +
  labs(color="Continuous northern distribution limit") +
  scale_color_manual(values=c(color_blind[3], color_blind[7], color_blind[8])) + 
  new_scale_color() + 
  geom_point(data=data, aes(x=Longitude, y=Latitude, color=ID), size=5)+
  scale_color_manual(values=c(color_blind[1], color_blind[2])) +
  geom_text(data=data, aes(x=Longitude, y=Latitude, label=plot_name)) +
  labs(color="Site")+
  coord_sf(xlim = c(lim_bio[1,1]+0.45, lim_bio[1,2]+lim_bio[1,2]/7), ylim = c(lim_bio[2,1], lim_bio[2,2]-0.4), expand=T) +
  theme_bw() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.35, height = unit(0.1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude")+
  theme(
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  )

# It may have some problemes when display in the view panel in Rstudio, but no problems 
# when saving in pdf

emprise <- ggplot(data=world) +
  geom_sf() +
  coord_sf(xlim = c(-99, -51), ylim = c(30, 62), expand = FALSE) +
  theme_void() +
  annotate("rect", xmin = lim_bio[1,1]+0.45, xmax = lim_bio[1,2]+lim_bio[1,2]/7, ymin = lim_bio[2,1], ymax = lim_bio[2,2]-0.4,
           alpha = .6, fill = "grey50", color="black",size=0.5) +
  theme(
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 1))

library("grid")
library("ggplotify")

main <- as.grob(main)
emprise <- as.grob(emprise)

pdf(file="map_ggplot.pdf")
grid.newpage()
grid.draw(main)
vp <-  viewport(x=.908, y=.775, width=.15, height=.15)
pushViewport(vp)
grid.draw(emprise)
upViewport()
dev.off()
