# Remove all objects from the workspace (memory) of the R session
rm(list=ls())
gc()

# load the required packages
library(here)
library(ggmap)
library(dplyr)

# Read data
xin0 <- read.csv(here("data/MSFD_descriptor1+worms.csv"))

# Exclude rows with empty taxonomic values and duplicates
xin <- xin0 %>% filter(Phylum != "")
xin <- distinct(xin)

# Replace the empty field class for "Not Available"
xin$Class <- replace(xin$Class, xin$Class == "", "Not available")

## Group by class, order, and species to get unique species
xin_grouped <- xin %>% group_by(Class, Order, ScientificName) %>%
  summarise()
xin_grouped
dim(xin_grouped)

## Now group by class and order and count the number of unique species
xin_df <- xin_grouped %>% group_by(Class, Order) %>%
  summarise(n=n_distinct(ScientificName)) %>% 
  arrange(desc(n)) %>% 
  ungroup()

xin_df$className <- as.factor(xin_df$Class)
xin_df$orderName <- as.factor(xin_df$Order)
xin_df <- xin_df %>% dplyr::select(className,orderName, n)
xin_df <- as.data.frame(xin_df)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(xin_df$className), ncol(xin_df)) )
colnames(to_add) <- colnames(xin_df)
to_add$className <- rep(levels(xin_df$className), each=empty_bar)
xin_df<- rbind(xin_df, to_add)
xin_df <- xin_df %>% arrange(factor(className))
xin_df$id <- seq(1, nrow(xin_df))

unique(xin_df$orderName)

unique_xin_count <- xin %>% 
  distinct(ScientificName) %>% 
  nrow()


# Get the name and the y position of each label
label_data <- xin_df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar  # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- xin_df %>% 
  group_by(className) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]
#xin_df$nlog = (xin_df$n)
#label_data$nlog = (label_data$n)

# Make the plot
p <- ggplot(xin_df, aes(x=as.factor(id), y=n, fill=className)) +       
# Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=n, fill=className), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(xin_df$id), ), y = c(10, 20, 40), label = c("10", "20", "40") , color="grey", size=3, angle=0, fontface="bold", hjust=1) +
  # annotate("text", x = rep(max(xin_df$id),2), y = c(1, 2), label = c("10", "100") , color="grey", size=3, angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x=as.factor(id), y=n, fill=className), stat="identity", alpha=0.5) +
  # Scale_y_log10(limits = c(-1000, 1000))+
  #scale_y_log10(limits = c(-100,100))+
  ylim(-25,55) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm"),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.margin=margin(0,0,0,0),
    legend.box.margin=margin(-100, 0, 0, 0)
  ) +
  guides(fill=guide_legend(ncol=4)) +
  coord_polar() + 
  labs(fill = "Class")+
  geom_text(data=label_data, aes(x=id, y=n+0.1, label=orderName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4., angle= label_data$angle, inherit.aes = FALSE ) +
  annotate("text", x = 1, y = -25, label = paste("Species:", unique_xin_count), size = 4, fontface = "bold") +
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -0.1, xend = end, yend = -0.1), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  #+


p

ggsave(
  here("output/figures/MSFD_worms_col3.pdf"),
  width = 10,
  height = 10,
  dpi = 600
)
