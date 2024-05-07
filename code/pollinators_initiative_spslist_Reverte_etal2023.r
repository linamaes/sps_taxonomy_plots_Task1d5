# Remove all objects from the workspace (memory) of the R session
rm(list=ls())
gc()

# load the required packages
library(here)
library(ggmap)
library(dplyr)

## Read data
xin <- read.csv(here("data/pollinators_sps_list_Reverte_et_al_insect_conservation&diversity_2023.csv"))

## Group by order, family, and species to get unique species
x_grouped <- xin %>% group_by(Order, Family, GenusAndSpecies) %>%
  summarise()
x_grouped
dim(x_grouped)

## Now group by order and family and count the number of unique species
x_df <- x_grouped %>% group_by(Order, Family) %>%
  summarise(n=n_distinct(GenusAndSpecies)) %>% 
  arrange(desc(n)) %>% 
  ungroup()

dim(x_df)

x_df$orderName <- as.factor(x_df$Order)
x_df$familyName <- as.factor(x_df$Family)
x_df <- x_df %>% dplyr::select(orderName,familyName, n)
x_df <- as.data.frame(x_df)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(x_df$orderName), ncol(x_df)) )
colnames(to_add) <- colnames(x_df)
to_add$orderName <- rep(levels(x_df$orderName), each=empty_bar)
x_df<- rbind(x_df, to_add)
x_df <- x_df %>% arrange(factor(orderName))
x_df$id <- seq(1, nrow(x_df))

unique_x_count <- xin %>% 
  distinct(GenusAndSpecies) %>% 
  nrow()


# Get the name and the y position of each label
label_data <- x_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar  # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- x_df %>% 
  group_by(orderName) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
#x_df$nlog = (x_df$n)
#label_data$nlog = (label_data$n)

# Make the plot
p <- ggplot(x_df, aes(x=as.factor(id), y=n, fill=orderName)) +       
# Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=n, fill=orderName), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 400, xend = start, yend = 400), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(x_df$id), ), y = c(1, 400, 800), label = c("1", "400", "800") , color="grey", size=3, angle=0, fontface="bold", hjust=1) +
  # annotate("text", x = rep(max(x_df$id),2), y = c(1, 2), label = c("10", "100") , color="grey", size=3, angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x=as.factor(id), y=n, fill=orderName), stat="identity", alpha=0.5) +
  ylim(-550,1200) +
  theme_minimal() +
  theme(
    legend.position = "left",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm"),
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    legend.margin=margin(0,0,0,90),
    legend.box.margin=margin(0, -80, 0, 0)
  ) +
  guides(fill=guide_legend(ncol=1)) +
  coord_polar() + 
  labs(fill = "Order")+
  geom_text(data=label_data, aes(x=id, y=n+0.1, label=familyName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=6, angle= label_data$angle, inherit.aes = FALSE ) +
  annotate("text", x = 1, y = -500, label = paste("Species:", unique_x_count), size = 6, fontface = "bold") +
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  
  
p

ggsave(
  paste0("output/figures/pollinators_sps_list_Reverte_etal2013.pdf"),
  width = 9,
  height = 7,
  dpi = 600
)
