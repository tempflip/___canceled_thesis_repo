df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
library(ggplot2)

# on map
ggplot(data=df, aes(x=Lon, y=Lat)) + geom_point()

# monthly consumpltion plot
months = names(df)[7:18]
m = df[,months]
m['id'] = seq(1:220)
m_long <- melt(m, id.vars=c('id'))

ggplot(data=m_long, aes(x=variable, y=value, group=id)) + geom_line()

#normalizing
m_ = df[,months]
m_norm_by_mean = m_ / apply(m_, 1, mean)
m_norm_by_mean['id'] = seq(1:220)
m_norm_by_mean_long = melt(m_norm_by_mean[150:170,], id.vars=c('id'))
ggplot(data=m_norm_by_mean_long, aes(x=variable, y=value, group=id)) + geom_line()

# mapping extra
library(leaflet)
mp <- leaflet(data = df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius = ~jan)
mp
