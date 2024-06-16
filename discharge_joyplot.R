
library(ggridges)      # create ridgeline plots in ggplot
library(RColorBrewer)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyverse)

setwd("C:\\Users\\wadea\\Documents\\Maps")

#import data downloaded from NWIS https://maps.waterdata.usgs.gov/mapper/index.html
discharge <- read.csv("kokosing.csv", header = TRUE)

#separate date column to calculate avg. daily discharge
discharge <- discharge |>
  separate(datetime, into=c('date','time'), sep=" ")

#create new date column in correct format
discharge$date <- as.Date(discharge$datetime, "%m/%d/%Y")

#calculate avg. daily discharge
dailydischarge <- discharge %>%
  # create new date variables, incl. decimalized month
  mutate(year = year(date), julian = yday(date), month = julian/31+1) |>
  #calculate mean daily discharge
  group_by(date) |>
  mutate(daily.dis = mean(discharge)) |>
  ungroup()

### Plot data

# 

ggplot(discharge, aes(julian, year, height = discharge, group = year, fill = year)) +
  geom_ridgeline(scale = 0.0015, size=0.33, color = NA) +   # adjust <scale> to change scaling and spacing of years
  scale_fill_viridis_c(option = "D", begin = 0, end = 0.8) +
  scale_fill_gradientn(colours = c("#1b220f", "#37611e", "#677d66", "#6a6a88","#cda744")) +
  scale_y_reverse(breaks = seq(1991, 2024, by=33)) +  # scale y axis
  labs(title = "33 YEARS OF KOKOSING RIVER FLOW, OH",
       caption = 'SOURCE: USGS NWIS',
       x = '', y = '') +
  theme_void() +
  theme(text = element_text(family='sans', color = "#1b220f"),
        plot.title = element_text(size= 32, hjust=0.5, vjust = 1,  color = "#1b220f" ,margin = margin(t = 1.5, unit = "cm")),
        plot.caption = element_text(size= 12, hjust=0.0, color = "#1b220f"),
        axis.text=element_text(size=16, color = "#1b220f"),
        axis.title=element_text(size=16, color = "#1b220f"),
        legend.position = 'none',
        plot.margin = margin(l=0.2, r=0.2, unit = "cm"), 
        plot.background = element_rect(fill = "floralwhite", color = NA), 
        panel.background = element_rect(fill = "floralwhite", color = NA), 
        legend.background = element_rect(fill = "floralwhite", color = NA))

#ggsave(filename = "kokosingflow.jpeg", height = 14, width = 11, units = "in", dpi = 900)



joy <- ggplot(discharge, aes(x = julian, y = year, group = year, fill = year, height = daily.dis)) +
  geom_density_ridges2(#rel_min_height = 0.000001,
                      stat = "identity",
                      scale = 10,
                      size = 0.1,
                      #fill = "white",
                      #color = "white",
                      ) +
  scale_fill_viridis_c(trans='reverse') +
  theme_void() +
  labs(title = "30 years of King's River flow, AR",
       caption = 'Source: USGS NWIS',
       x = '', y = '') +
  theme(
    text = element_text(color = "white"),
    axis.title.x = element_text(color = "white"),
    axis.title.y = element_text(color = "white"),
    plot.background = element_rect(fill = "#28282B", color = NA), 
    panel.background = element_rect(fill = "#28282B", color = NA), 
    legend.position = 'none',
    plot.title = element_text(size= 16, hjust=0.1, color = "white", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.caption = element_text(size= 10, hjust=0.0, color = "white"))
  
  

theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(colour = "white"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"))
joy





# Plot a version with fill color varying by flow (more informative)

ggplot(discharge, aes(month, year, height = cfs, group = year, fill = cfs)) +   # fill = cfs for this version
  with_shadow(geom_ridgeline_gradient(scale = 0.00006, size=0.33, color='black'), x_offset=2, y_offset=2, sigma=3) +
  scale_fill_gradientn(colors=turbo()) +
  # scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1890, 2025, by=10), expand=c(0,0.7)) +
  labs(title = "100 years of Colorado River flow at Lee's Ferry",
       x = 'Month', y = '',
       caption = 'Source: USGS/NWIS') +
  theme(text = element_text(family='sans'),
        axis.text=element_text(size=10),
        axis.title=element_text(size=10),
        legend.position = 'none',
        panel.background = element_blank())

ggsave("plots/LeesFerry variegated shadow.png", width=6, height=8)   # save the plot

### 3D Heatmap

#temp <- discharge[1,]
#for (i in 1:nrow(discharge)) {
#  if (discharge[i,]$julian < 365) {
#    newrow <- discharge[i,]
#    newrow$month <- (discharge[i,]$month + discharge[i+1,]$month)/2
#    newrow$cfs   <- (discharge[i,]$cfs + discharge[i+1,]$cfs)/2
#    temp <- rbind(temp, newrow)
#  }
#  temp <- rbind(temp, discharge[i+1,])
#}

myplot <- ggplot(discharge, aes(month, year, fill = cfs)) +
  geom_tile() +   # create a heatmap of the data
  scale_x_continuous(breaks = seq(1, 12, by=1), expand=c(0.01,0)) +
  scale_y_reverse(breaks = seq(1890, 2025, by=10), expand=c(0,0.7)) +
  scale_fill_gradientn(colors=turbo()) +
  labs(title = "Colorado River at Lees Ferry",
       caption = 'USGS / NWIS',
       x = 'Month', y = '') +
  theme(text = element_text(family='sans'),
        plot.title = element_text(vjust = 2),
        axis.text=element_text(size=9),
        axis.title=element_text(size=9),
        panel.background = element_blank())

plot_gg(myplot,             # ggplot to 3d-ize using rayshader library
        raytrace = F,       # use raytracing to create shadows
        multicore=T,        # use multiple cores for raytracing
        width=5, height=5,
        scale=200,          # vertical exaggeration
        sunangle=215,       # angle of light source for creating shadows
        shadow_intensity = 0.4,
        windowsize = c(1600,1100),
        zoom = 0.6,         # default zoom
        offsetedges = 0.5,
        phi=40)             # default view angle

render_snapshot(file='plots/3d flow Green.png', vignette=T) # capture image of plot


### Plot multiple stations moving downriver

# Important: List stations in upstream -> downstream order
selectedSites <- c("09026500", "09033300", "09034250", "09058000", "09070500", "09095500", "09163500", "09185600")
variable <- "00060" # Discharge in cfs
COdischarge_raw <- readNWISdv(selectedSites, variable, "1966-01-01", today())
COdischarge_raw %>% group_by(site_no) %>% summarize(FirstDate = min(Date), LastDate = max(Date)) # show date availability for ea. site
COdischarge_raw %>% select(-agency_cd, -X_00060_00003_cd) %>%
  rename(cfs = X_00060_00003, date = Date) %>%
  mutate(year = year(date), julian = yday(date), site_no = factor(site_no, levels=rev(selectedSites))) %>%
  filter(cfs >= 0, year==2017)  -> COdischarge

ggplot(COdischarge, aes(julian, site_no, height = cfs, fill = site_no)) +
  geom_joy(stat="identity", scale = 2, size = 0.5) +
  theme_joy() +
  theme(text=element_text(family="Arial Narrow", size=16),
        plot.title=element_text(family="Arial Narrow", size=20),
        plot.caption=element_text(color="#999999", size=10),
        legend.position = "none") +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = "Day of year", y = "USGS Gauging Station", title = "Average discharge along the Colorado River",
       subtitle="Headwaters to Lake Powell (Fraser, CO to Canyonlands, UT)",
       caption = "Lauren Steely | @MadreDeZanjas")

ggsave('downstream.png', height=6, width=9)

sitelocations <- filter(allSites, SiteCode %in% selectedSites) %>% select(SiteCode, SiteName, Latitude, Longitude)

### Add a map of gauge location

leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  addMarkers(lat=sitelocations$Latitude, lng=sitelocations$Longitude,
             label=paste(sitelocations$SiteCode, "-", sitelocations$SiteName)) %>%
  setView(lng = -107.8, lat = 39.4, zoom = 8) # point the camera at Blythe