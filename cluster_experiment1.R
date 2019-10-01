library(ggplot2)
library(reshape)
library(leaflet)
library(class)

df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
months = names(df)[7:18]

ll <- df[,c('Lat', 'Lon')]
N <- 6
km <- kmeans(ll, N)
ll$cluster = km$cluster
#km$centers
#km$cluster

pal <- colorNumeric(palette = "inferno", domain = ll$cluster)
mp <- leaflet(data = ll) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius =10, color = ~pal(cluster))
mp

####################################
#####################################
mm <- df[,months]
geo <- df[,c('Lat', 'Lon')]

N <- 3
clus2 <- kmeans(mm, N)
clus2

month_centers <- data.frame(clus2$centers)
month_centers$id <- seq(1:nrow(month_centers))
month_centers_long <- melt(month_centers, id.vars=c('id'))

ggplot(data = month_centers_long, aes(x=variable, y=value, group=id)) + geom_line()

geo$cluster = clus2$cluster
  

pal <- colorNumeric(palette = "inferno", domain = geo$cluster)
mp <- leaflet(data = geo) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius =10, color = ~pal(cluster))
mp
