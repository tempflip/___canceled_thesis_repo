df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
months = names(df)[7:18]
df['row_mean'] = apply(df[,months],1,mean)
df$row_mean = sqrt(df$row_mean)
library(ggplot2)
library(reshape)
library(scales)
# on map
ggplot(data=df, aes(x=Lon, y=Lat)) + geom_point()

# monthly consumpltion plot


# some random houses
m = df[,months]
m['id'] = seq(1:220)
m_long <- melt(m[0:6,], id.vars=c('id'))
ggplot(data=m_long, aes(x=variable, y=value, group=id)) + geom_line(aes(color=id)) + theme(legend.position = "none")

# all the houses
m_long <- melt(m, id.vars=c('id'))
ggplot(data=m_long, aes(x=variable, y=value, group=id)) + geom_line(aes(color=id)) + theme(legend.position = "none")

# boxplots
m = df[,months]
m['id'] = seq(1:220)
boxplot(m[,1:12], las=2)
boxplot(subset(m[,1:12], m[,1:12] < 10000), las=2)
boxplot(subset(m[,1:12], m[,1:12] < 2000), las=2)

# corr-matrix
library(GGally)
ggcorr(m[,0:12])



#normalizing
m_ = df[,months]
m_norm_by_mean = m_ / apply(m_, 1, mean)
m_norm_by_mean['id'] = seq(1:220)
m_norm_by_mean_long = melt(m_norm_by_mean[10:70,], id.vars=c('id'))
ggplot(data=m_norm_by_mean_long, aes(x=variable, y=value, group=id)) + geom_line()

# mapping extra
library(leaflet)

pal <- colorNumeric(palette = "inferno", domain = df$row_mean)

mp <- leaflet(data = df) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius = ~row_mean/5, color=~pal(row_mean))
mp



