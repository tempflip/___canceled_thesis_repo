library(ggplot2)
library(reshape)
library(leaflet)
library(class)
library(gridExtra)

df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
months = names(df)[7:18]

#### some grouping
df$group = rep(0, nrow(df))
# Mangatan
df[c(131, 132, 133),]$group = 1
# Klöverstigen
df[seq(93, 106),]$group = 2



# plot on the map
ll <- df[,c('Lat', 'Lon', 'group')]

pal <- colorNumeric(palette = "inferno", domain = ll$group)
leaflet(data = ll) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(~Lon, ~Lat, radius =1, color=~pal(group))

# some boxplots of 2 groups
boxplot(df[,months])
boxplot(subset(df, group == 1)[,months])
boxplot(subset(df, group == 2)[,months])


g1box <- ggplot(data = melt(subset(df, group == 1)[,months]), aes(x=variable, y=value)) + 
  geom_boxplot() + 
  ylim(0, 15000)
g2box <- ggplot(data = melt(subset(df, group == 2)[,months]), aes(x=variable, y=value)) + 
  geom_boxplot() +
  ylim(0, 15000)

grid.arrange(g1box, g2box, nrow=1)

# clustering experiments
set.seed(123)
only_groups <- subset(df, group == 1 | group == 2)
