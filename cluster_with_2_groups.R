library(ggplot2)
library(reshape)
library(leaflet)
library(class)
library(gridExtra)
library(lubridate)


df <- read.csv("~/Desktop/uni/thesis_repo/KVH_data.csv")
months = names(df)[7:18]

#### some grouping
df$group = rep(0, nrow(df))
df$X <- seq(1, nrow(df))
# Mangatan
df[c(131, 132, 133),]$group = 1
# Klöverstigen
### 96 is an outlier!
df[c(93, 94, 95, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106),]$group = 2

## Vallgatan 13-21, Havavagen 74
df[c(205, 206, 207),]$group = 3



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
boxplot(subset(df, group == 3)[,months])


g1box <- ggplot(data = melt(subset(df, group == 1)[,months]), aes(x=variable, y=value)) + 
  geom_boxplot() + 
  ylim(0, 15000)
g2box <- ggplot(data = melt(subset(df, group == 2)[,months]), aes(x=variable, y=value)) + 
  geom_boxplot() +
  ylim(0, 15000)
g3box <- ggplot(data = melt(subset(df, group == 3)[,months]), aes(x=variable, y=value)) + 
  geom_boxplot() +
  ylim(0, 15000)

grid.arrange(g1box, g2box, g3box, nrow=1)

# clustering experiments
only_groups <- subset(df, group == 1 | group == 2 | group3)

set.seed(123)
clus1 <- kmeans(only_groups[,months], 3)
clus1$cluster

## -> not ideal


###########
###########
# polynomial regresson experiment
###########

melt_months <-df[1:1,c(months, 'X')] %>% melt(id.vars=c('X'))
m1 <- lm(value ~ as.numeric(variable), data = melt_months)

ggplot(data = melt_months, aes(x = variable, y = value, group = X)) +
  geom_line(color = "red") +
  geom_line(aes(y = predict(m1, melt_months)), color="pink")


