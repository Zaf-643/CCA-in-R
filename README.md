# CCA-in-R
library(vegan)
library(ggplot2)
library(ggrepel)
library(readxl)
library(ggordiplots)

#upload your data 
data <- read.csv("data.csv")

#upload the dependent data or community data
community_matrix <- data[, -c(1,2,3,4,5,6,7,8,9)]

#upload the independent data or environmental data
environment_matrix <- data[, c(1,3,4,5,6,7,8,9)]

cca_result <- cca(community_matrix, environment_matrix)

summary(cca_result)
#create data frame accordin to the CCA output
env_data <- as.data.frame(cca_result$CCA$biplot)
env_data["env"] <- row.names(env_data)
species_data <- as.data.frame(cca_result$CCA$v)
species_data["Species_Group"] <- factor(c(rep("Group-A",14), rep("Group-B",12), rep("Group-C", 2), rep("Group-D", 10), rep("Group-E", 3), rep("Group-F", 2), rep("Group-G", 3), rep("Group-H", 3), rep("Group-I", 10)))

#visualize the data
triplot_season <- gg_ordiplot(cca_result, groups = environment_matrix$Season, kind = "ehull")
triplot_season_plot <- ggplot() + 
  geom_point(data = triplot_season$df_ord, aes(x = x, y = y, color = Group), size = 3) +
  geom_path(data =triplot_season$df_ellipse, aes(x = x, y = y, color = Group), show.legend = FALSE) +
  geom_polygon(data = triplot_season$df_ellipse[1:101,], aes(x =x, y = y), fill = "#F8766D", alpha = 0.2) +
  geom_polygon(data = triplot_season$df_ellipse[102:202,], aes(x =x, y = y), fill = "#00BFC4", alpha = 0.2)
triplot_season_plot
triplot_season_plot + geom_point (data = env_data, aes(x = CCA1, y = CCA2), color = "black") +
  geom_segment(
    data = env_data,
    aes(
      x = 0,
      y = 0,
      xend = CCA1,
      yend = CCA2
    ),
    arrow = arrow(length = unit(0.3, "cm"))
  ) +
  geom_text_repel(
    data = env_data,
    aes (x = CCA1, y = CCA2, label = env_data$env),
    nudge_y = -0.05,
    color = "black",
    size = 5
  ) +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 18)) + theme_bw() +
  labs(color = "Season", title = "CCA Triplot") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(data = species_data, aes(x = CCA1, y = CCA2), color = "red", shape = "triangle") +
  geom_text_repel(data = species_data, 
                  aes(x=CCA1, y= CCA2, label = row.names(species_data)),
                  nudge_y = -0.5)


biplot_species <- ggplot(species_data, aes(x=CCA1, y=CCA2, color = Species_Group)) +
  geom_point(size = 2, alpha = 0.7, position = position_jitter(width = 0.1, height = 0.1)) +
  theme_minimal()
biplot_species + stat_ellipse() +
  labs(color = "Species", title = "CCA Biplot") +
  theme(plot.title = element_text(hjust = 0.5))
