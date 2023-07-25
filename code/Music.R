# Imports -----------------------------------------------------------------

library(data.table)
library(igraph)
library(ggplot2)
library(dplyr)
library(shiny)
library(patchwork)
library(ggraph)
setwd("/Users/arturo/Desktop/DDMCS/project")

# Load data ---------------------------------------------------------------

dt = fread('./data/music.csv')
dt[, decade := as.integer(floor(release_date / 10) * 10)]

# Genre Evolution ---------------------------------------------------------

# Calculate the proportions for each genre in each decade
dt_prop <- dt %>% 
  group_by(decade, genre) %>% 
  summarize(n = n()) %>% 
  group_by(decade) %>% 
  mutate(prop = n / sum(n) * 100)

ggplot(dt_prop, aes(x = decade, y = prop, fill = genre)) +
  geom_col(position = "dodge") +  
  scale_y_continuous() +
  labs(x = "Decade", y = "Proportion of songs (%)", fill = "Genre") +
  theme_classic()

# Topic Evolution ---------------------------------------------------------

dt_prop <- dt %>% 
  group_by(decade, topic) %>% 
  summarize(n = n()) %>% 
  group_by(decade) %>% 
  mutate(prop = n / sum(n) * 100)

ggplot(dt_prop, aes(x = decade, y = prop, fill = topic)) +
  geom_col(position = "dodge") +  
  scale_y_continuous() +
  labs(x = "Decade", y = "Proportion of songs (%)", fill = "Topic") +
  theme_classic()

# Create graph --------------------------------------------------------------

edges = fread('./data/graphs/edges80.csv')
nodes = fread('./data/graphs/nodes80.csv')
nodes[, decade := as.integer(floor(release_date / 10) * 10)]
len = dim(nodes)[1]+1
g <- graph_from_data_frame(d=edges, vertices=nodes, directed=F)

# Degree -------------------------------------------------------------

degrees <- degree(g)
degree.histogram <- as.data.frame(table(degrees))
degree.histogram[,1] <- as.numeric(degree.histogram[,1])

ggplot(degree.histogram, aes(x = degrees, y = Freq)) +
  geom_point(color=rgb(255, 14, 131, maxColorValue = 255)) +
  scale_x_continuous("Degree",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000, 2000),
                     trans = "log10") +
  scale_y_continuous("Frequency",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000, 5000, 10000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") + 
  theme(panel.background = element_rect(fill = rgb(230, 230, 230, maxColorValue = 255),
                                    colour = rgb(255, 14, 131, maxColorValue = 255),
                                    size = .5, linetype = "solid"),
        panel.grid.minor = element_line(size = .3, linetype = 'dotted',
                                        colour = rgb(255, 96, 80, maxColorValue = 255)),
        panel.grid.major = element_blank())

# find top-100 degree nodes
nodes[song_id %in% names(sort(degree, decreasing = T)[1:100])]$topic

# Largest Connected Component ---------------------------------------------

# Find the largest connected component
cc <- clusters(g)$membership[which.max(table(clusters(g)$membership))]
largest_cc <- induced_subgraph(g, which(clusters(g)$membership == cc))

# Compute the distribution of the nodes decade attribute
round(prop.table(table(V(largest_cc)$decade)), 2)
round(prop.table(table(V(g)$decade)),2)

# Specify the color for each node based on the decade_group attribute
colors <- c("red", "orange", "yellow", "green", "blue", "purple", "pink")[as.numeric(factor(V(largest_cc)$decade))]

# Plot the largest connected component with node colors based on decade
plot(largest_cc, layout = layout.fruchterman.reingold(largest_cc), vertex.color = colors, vertex.size = 7,
     main = "Largest Connected Component", edge.width = 0.5, edge.alpha = 0.5,
     vertex.label = NA)

# Decades Connectivity ----------------------------------------------------

# k-core reduction
while(length(g)!=len){
  len <- length(g)
  g <- delete_vertices(g, which(degree(g) < 5))
}

# Compute the proportion of neighbors with the same decade release year
V(g)$same_decade_prop <- sapply(V(g), function(v) {
  neighbors <- neighbors(g, v)
  release_years <- V(g)[neighbors]$release_date
  decade <- as.integer(floor(release_years / 10) * 10)
  same_decade_prop <- mean(V(g)[v]$release_date %/% 10 == decade %/% 10)
  same_decade_prop
})

# Create a data.table to hold the proportions and decades
dtt <- data.table(proportions=V(g)$same_decade_prop, decade=factor(V(g)$decade))
ggplot(dtt, aes(x=proportions, fill=decade)) + 
  geom_density(alpha=0.7) + xlim(0,1) + 
  theme(legend.position = "bottom")

get_proportion <- function(g, d) {
  subg <- subgraph(g, V(g)[decade == d])
  cc <- clusters(subg)$membership[which.max(table(clusters(subg)$membership))]
  largest_cc <- induced_subgraph(subg, which(clusters(subg)$membership == cc))
  proportion <- length(largest_cc)/length(V(subg))
  return(proportion)
}

# Compute the largest connected component proportion for each decade
decades <- unique(V(g)$decade)
proportions <- lapply(X=decades, FUN=get_proportion, g=g)
# Create a data.table with the decade and proportion columns
dt <- data.table(decade = factor(decades), proportion = unlist(proportions))

decade_colors <- c("1950" = "red", "1960" = "orange", "1970" = "yellow", "1980" = "green",
                   "1990" = "blue", "2000" = "purple", "2010" = "pink")

# Create a bar plot with ggplot2
ggplot(dt, aes(x = decade, y = proportion, fill = decade)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = decade_colors) +
  labs(x = "Decade", y = expression(frac("# LCC per Decade", "# Subgraph per Decade")), fill = "Decade") +
  guides(fill = guide_legend(title = "Decade")) +
  theme_classic()


# Same Genre Neighbors Proportion ----------------------------------------------

get_proportion <- function(g, d) {
  # Get the subgraph of g that contains only the nodes from decade d
  subg <- subgraph(g, V(g)[decade == d])
  props <- sapply(V(subg), function(node) {
    neighbors <- neighbors(subg, node)
    same_genre_neighbors <- neighbors[V(subg)$genre[neighbors] == V(subg)$genre[node]]
    length(same_genre_neighbors) / length(neighbors)
  })
  # Return the average proportion of same-topic neighbors for the largest connected component
  mean(props, na.rm = TRUE)
}

# Compute the largest connected component proportion for each decade
decades <- unique(V(g)$decade)
proportions <- lapply(X=decades, FUN=get_proportion, g=g)
# Create a data.table with the decade and proportion columns
dt <- data.table(decade = factor(decades), proportion = unlist(proportions))

decade_colors <- c("1950" = "red", "1960" = "orange", "1970" = "yellow", "1980" = "green",
                   "1990" = "blue", "2000" = "purple", "2010" = "pink")

# Create a bar plot with ggplot2
ggplot(dt, aes(x = decade, y = proportion, fill = decade)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = decade_colors) +
  labs(x = "Decade", y = "Avg. Proportion of same genre neighbors", fill = "Decade") +
  guides(fill = guide_legend(title = "Decade")) +
  ylim(0,1) +
  theme_classic()


# Same Topic Neighbors Proportion ----------------------------------------------

get_proportion <- function(g, d) {
  # Get the subgraph of g that contains only the nodes from decade d
  subg <- subgraph(g, V(g)[decade == d])
  props <- sapply(V(subg), function(node) {
    neighbors <- neighbors(subg, node)
    same_topic_neighbors <- neighbors[V(subg)$topic[neighbors] == V(subg)$topic[node]]
    length(same_topic_neighbors) / length(neighbors)
  })
  # Return the average proportion of same-topic neighbors for the largest connected component
  mean(props, na.rm = TRUE)
}

# Compute the largest connected component proportion for each decade
decades <- unique(V(g)$decade)
proportions <- lapply(X=decades, FUN=get_proportion, g=g)
# Create a data.table with the decade and proportion columns
dt <- data.table(decade = factor(decades), proportion = unlist(proportions))

decade_colors <- c("1950" = "red", "1960" = "orange", "1970" = "yellow", "1980" = "green",
                   "1990" = "blue", "2000" = "purple", "2010" = "pink")

# Create a bar plot with ggplot2
ggplot(dt, aes(x = decade, y = proportion, fill = decade)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = decade_colors) +
  labs(x = "Decade", y = "Avg. Proportion of same topic neighbors", fill = "Decade") +
  guides(fill = guide_legend(title = "Decade")) +
  ylim(0,1) +
  theme_classic()

# Violent Songs Polarization ----------------------------------------------

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] == 'violence')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$violence, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic == 'violence')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

violent <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Violent Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Non-Violent Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(df_binned$mean_scores[1], max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Violence") +
  ylab("Proportion of Violent Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Violent Neighbors', 'Non-Violent Neighbors'),
                    values = c('Violent Neighbors'="red", 'Non-Violent Neighbors'="blue"))

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] != 'violence')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$violence, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic != 'violence')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

non_violent <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Non-Violent Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Violent Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(min(df_binned$mean_scores), max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Violence") +
  ylab("Proportion of Violent Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Violent Neighbors', 'Non-Violent Neighbors'),
                    values = c('Violent Neighbors'="red", 'Non-Violent Neighbors'="blue"))


violent + non_violent + 
  plot_annotation(title = "Neighbors Polarization of Violent and Non-Violent Songs")

# Obscene Songs Polarization ----------------------------------------------

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] == 'obscene')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$obscene, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic == 'obscene')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

obscene <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Obscene Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Non-Obscene Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(df_binned$mean_scores[1], max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Obscenity") +
  ylab("Proportion of Obscene Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Obscene Neighbors', 'Non-Obscene Neighbors'),
                    values = c('Obscene Neighbors'="red", 'Non-Obscene Neighbors'="blue"))

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] != 'obscene')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$obscene, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic != 'obscene')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

non_obscene <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Non-Obscene Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Obscene Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(min(df_binned$mean_scores), max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Obscene") +
  ylab("Proportion of Obscene Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Obscene Neighbors', 'Non-Obscene Neighbors'),
                    values = c('Obscene Neighbors'="red", 'Non-Obscene Neighbors'="blue"))

obscene + non_obscene + 
  plot_annotation(title = "Neighbors Polarization of Obscene and Non-Obscene Songs")

# Sad Songs Polarization ---------------------------------------------

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] == 'sadness')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$sadness, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic == 'sadness')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

sadness <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Sad Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Non-Sad Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(df_binned$mean_scores[1], max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Sadness") +
  ylab("Proportion of Sad Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Sad Neighbors', 'Non-Sad Neighbors'),
                    values = c('Sad Neighbors'="red", 'Non-Sad Neighbors'="blue"))

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] != 'sadness')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$sadness, topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic != 'sadness')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

non_sadness <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Non-Sad Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Sad Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(min(df_binned$mean_scores), max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Sadness") +
  ylab("Proportion of Sad Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Sad Neighbors', 'Non-Sad Neighbors'),
                    values = c('Sad Neighbors'="red", 'Non-Sad Neighbors'="blue"))

sadness + non_sadness + 
  plot_annotation(title = "Neighbors Polarization of Sad and Non-Sad Songs")

# Life Songs Polarization ---------------------------------------------

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] == 'world/life')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$'world/life', topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic == 'world/life')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

life <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Life Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Non-Life Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(df_binned$mean_scores[1], max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Lifeness") +
  ylab("Proportion of Life Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Life Neighbors', 'Non-Life Neighbors'),
                    values = c('Life Neighbors'="red", 'Non-Life Neighbors'="blue"))

neighbors_scores <- lapply(1:length(V(g)), function(i) {
  neighbors_p <- sum(V(g)$topic[neighbors(g, i)] != 'world/life')
  total_neighbors <- length(neighbors(g, i))
  neighbors_p / total_neighbors
})

# Plot the result
df <- data.table(scores = V(g)$'world/life', topic = V(g)$topic, prop = unlist(neighbors_scores)) %>% subset(., topic != 'world/life')
df_binned <- df[, .(mean_scores = mean(scores), prop_scores = mean(prop)), by = .(bin = cut(scores, breaks = seq(0, 1, by = 0.1)))]

non_life <- ggplot(df_binned, aes(x = mean_scores, ymin=0,  ymax = prop_scores)) +
  geom_ribbon(alpha = 0.8, aes(fill='Non-Life Neighbors')) +
  geom_ribbon(aes(ymin = prop_scores, fill='Life Neighbors', ymax = 1), alpha = 0.8) +
  geom_line(aes(y = prop_scores), color = "black") +
  scale_x_continuous(limits = c(min(df_binned$mean_scores), max(df_binned$mean_scores)), expand=c(0,0)) +
  scale_y_continuous(limits = c(0, 1), expand=c(0,0)) +
  xlab("Song Lifeness") +
  ylab("Proportion of Life Neighbors") +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_blank(),
        legend.key = element_rect(fill = "white", color = "black"),
        legend.key.size = unit(0.2, "cm")) +
  scale_fill_manual(breaks = c('Life Neighbors', 'Non-Life Neighbors'),
                    values = c('Life Neighbors'="red", 'Non-Life Neighbors'="blue"))

life + non_life + 
  plot_annotation(title = "Neighbors Polarization of Life and Non-Life Songs")

# Same Topic Density ------------------------------------------------------

# Compute the proportion of neighbors with the same decade release year
V(g)$same_topic_prop <- sapply(V(g), function(v) {
  neighbors <- neighbors(g, v)
  topic <- V(g)[neighbors]$topic
  same_topic_prop <- mean(V(g)[v]$topic == topic)
  same_topic_prop
})

# Create a data.table to hold the proportions and decades
dtt <- data.table(proportions=V(g)$same_topic_prop, decade=factor(V(g)$decade))
ggplot(dtt, aes(x=proportions, fill=decade)) + 
  geom_density(alpha=0.7) + xlim(0,1) + 
  theme(legend.position = "bottom") +
  theme_classic()

# Same Genre Density ------------------------------------------------------

# Compute the proportion of neighbors with the same decade release year
V(g)$same_genre_prop <- sapply(V(g), function(v) {
  neighbors <- neighbors(g, v)
  genre <- V(g)[neighbors]$genre
  same_genre_prop <- mean(V(g)[v]$genre == genre)
  same_genre_prop
})

# Create a data.table to hold the proportions and decades
dtt <- data.table(proportions=V(g)$same_genre_prop, decade=factor(V(g)$decade))
ggplot(dtt, aes(x=proportions, fill=decade)) + 
  geom_density(alpha=0.7) + xlim(0,1) + 
  theme(legend.position = "bottom") +
  theme_classic()

# Obscene Songs Genre Distribution ----------------------------------------

# Filter graph nodes by topic and create subgraph
obscene_subgraph <- subgraph(g, V(g)[topic == "obscene"])

# Extract genre and decade attributes and filter by topic
obscene_genre_by_decade <- data.table(get.data.frame(obscene_subgraph, what = "vertices"))[topic == "obscene", .(genre), by=decade]

# Plot genre distribution for each decade
ggplot(obscene_genre_by_decade, aes(x = decade, fill = genre)) + 
  geom_bar(position = "dodge") + 
  ggtitle("Genre Distribution for Obscene Songs by Decade") + 
  xlab("Decade") + 
  ylab("# Obscene Songs") +
  theme(legend.position = "bottom") +
  theme_classic()

# Evolution Over Time of Similarity ---------------------------------------

twenty10 = fread('./data/edges_2010.csv')
twenty00 = fread('./data/edges_2000.csv')
ninety90 = fread('./data/edges_1990.csv')
ninety80 = fread('./data/edges_1980.csv')
ninety70 = fread('./data/edges_1970.csv')
ninety60 = fread('./data/edges_1960.csv')
ninety50 = fread('./data/edges_1950.csv')

# list the 5 csv files
files <- c('1950', '1960', '1970', '1980', '1990', '2000', '2010')

# Bind the data frames
df_all <- bind_rows(ninety50, ninety60, ninety70, ninety80, ninety90, twenty00, twenty10)

# Add a column to identify each file
df_all$decade <- factor(rep(files, times=c(nrow(ninety50), nrow(ninety60), nrow(ninety70), 
                       nrow(ninety80), nrow(ninety90), nrow(twenty00), nrow(twenty10))))

setDT(df_all)
df_all[, density := .N / sum(df_all[, .N]), by = decade]

# Calculate the mean and variance for each file
df_stats <- df_all[, .(mean = mean(cosine), sd = sd(cosine)), by = decade]

# Merge the statistics with the original data
df_all_with_stats <- merge(df_all, df_stats, by = "decade")

ggplot(df_all_with_stats, aes(x=cosine, fill=decade)) +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.8, bins=25, color="white") +
  geom_vline(data=df_stats, aes(xintercept=mean), color = "black", size=2.5, show.legend=F) +
  labs(x ="Lyrics Similarity", y="Density") +
  xlim(0.1, 0.8) + theme_classic() +
  facet_wrap(~decade, ncol=1) +
  theme(legend.position="none")
