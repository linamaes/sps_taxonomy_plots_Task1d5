# Remove all objects from the workspace (memory) of the R session
rm(list=ls())
gc()

# load the required packages
library(here)
library(ggmap)
library(dplyr)

# Function for plotting according to Kingdom combination
plot_ias <- function(xin, kingdomin){

  if (kingdomin == 'Animalia'){
  xin <- xin %>% filter(kingdom == 'Animalia')
  } else if (kingdomin == 'Plantae&Chromista') {
      xin <- xin %>% filter(kingdom == 'Plantae' | kingdom == 'Chromista')
  } 


  # Group by order, family, and species to get unique species
  x_grouped <- xin %>% group_by(class, order, scientificName) %>%
    summarise()
  x_grouped
  dim(x_grouped)

  # Now group by order and family and count the number of unique species
  x_df <- x_grouped %>% group_by(class, order) %>%
    summarise(n=n_distinct(scientificName)) %>% 
    arrange(desc(n)) %>% 
    ungroup()

  dim(x_df)

  x_df$className <- as.factor(x_df$class)
  x_df$orderName <- as.factor(x_df$order)
  x_df <- x_df %>% dplyr::select(className,orderName, n)
  x_df <- as.data.frame(x_df)

  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 2
  to_add <- data.frame(matrix(NA, empty_bar*nlevels(x_df$className), ncol(x_df)))
  colnames(to_add) <- colnames(x_df)
  to_add$className <- rep(levels(x_df$className), each=empty_bar)
  x_df<- rbind(x_df, to_add)
  x_df <- x_df %>% arrange(factor(className))
  x_df$id <- seq(1, nrow(x_df))

  unique_x_count <- xin %>% 
    distinct(scientificName) %>% 
    nrow()

  # Get the name and the y position of each label
  label_data <- x_df
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar  # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse(angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)

  # prepare a data frame for base lines
  base_data <- x_df %>% 
    group_by(className) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))

  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  x_df$nlog = (x_df$n)
  label_data$nlog = (label_data$n)

  # Make the plot
  p <- ggplot(x_df, aes(x=as.factor(id), y=n, fill=className)) +       
  # Note that id is a factor. If x is numeric, there is some space between the first bar
    geom_bar(aes(x=as.factor(id), y=nlog, fill=className), stat="identity", alpha=0.5) +
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 3, xend = start, yend = 3), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(x_df$id), ), y = c(1, 2, 3, 4, 5), label = c("1", "2", "3", "4", "5") , color="grey", size=3, angle=0, fontface="bold", hjust=1) +
    geom_bar(aes(x=as.factor(id), y=nlog, fill=className), stat="identity", alpha=0.5) +
    ylim(-1.5,6) +
    theme_minimal() +
    theme(
      legend.position = "left",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(0,4), "cm"),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      legend.margin=margin(0,0,0,90),
      legend.box.margin=margin(-80, -80, 0, 0)
    ) +
    coord_polar() + 
    labs(fill = "Class")+
    geom_text(data=label_data, aes(x=id, y=nlog+0.1, label=orderName, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4., angle= label_data$angle, inherit.aes = FALSE ) +
    annotate("text", x = 1, y = -1.5, label = paste("Species:", unique_x_count), size = 4, fontface = "bold") +
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  #+

  p

  ggsave(
    here("output/figures/",paste0("ias_union_concern_col1_", kingdomin,".png")), # png pdf
    width = 10,
    height = 8,
    dpi = 600
  )

}

# Read data
xin <- read.csv(here("data/IAS_list_union_concern+gbif.csv"))

# Replace the empty field class for "Not Available"
xin$class <- replace(xin$class, xin$class == "", "Not available")
xin$order <- replace(xin$order, xin$order == "", "Not available")

# Select the Kingdom combination of interest
kingdomin <- 'Animalia'
# kingdomin <- 'Plantae&Chromista'

xout <- plot_ias(xin, kingdomin)
xout
