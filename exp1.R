df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
months = names(df)[7:18]
df['row_mean'] = apply(df[,months],1,mean)
df$row_mean = sqrt(df$row_mean)
library(ggplot2)

# on map
ggplot(data=df, aes(x=Lon, y=Lat)) + geom_point()

# monthly consumpltion plot


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

pal <- colorNumeric(palette = "inferno", domain = df$row_mean)

mp <- leaflet(data = df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius = ~row_mean/5, color=~pal(row_mean))
mp
