# Remove all objects from the workspace (memory) of the R session
rm(list=ls())
gc()

# load the required packages
library(here)
library(ggplot2)
library(stringr)

xin <- read.csv(here("data/european_red_list_2017_december.csv"))

xin$taxonomicRankClass <- str_to_sentence(xin$taxonomicRankClass)
xin$taxonomicRankKingdom <- str_to_sentence(xin$taxonomicRankKingdom)

# Select the red list field (i.e., column) to be used. In this case we selected de 'euRegionalRedListCategory'
# and filter the species in the following categories: VU: Vulnerable. EN: Endangered. CR: Critically endangered. CR(PE): Critically endangered (possibly extinct (PE)). EW: Extinct in the wild. EX: Extinct
xin2 <- xin %>% filter(!is.na(euRegionalRedListCategory)
 & (euRegionalRedListCategory == "VU" | euRegionalRedListCategory == "EN" |  euRegionalRedListCategory == "CR" |
 euRegionalRedListCategory == "CR (PE)" | euRegionalRedListCategory == "EW" |  euRegionalRedListCategory == "EX"))
dim(xin2)

# check for duplicates
duplicates <- duplicated(xin2$taxonomicRank)

colnames(xin2)

# Count the number of species by the 'Class' taxonomic rank
byclass <- xin2 |> group_by(taxonomicRankKingdom, taxonomicRankClass, euRegionalRedListCategory) |> 
  summarize(n = n()) |> 
  mutate(perc = 100*n/sum(n))

# Bar plot of number of species by Class by red list category
p = byclass |>
  ggplot(aes(x = taxonomicRankClass, y = n, fill = euRegionalRedListCategory, width=0.6, 
  )) +
  geom_col(show.legend = F) +
  scale_fill_manual("legend", values = c("VU" = "#FFCC00", "EN" = "orange", "CR" = "red", "CR (PE)" = "#610000", "EW" = "black", "EX" = "black")) +
  facet_grid(~factor(euRegionalRedListCategory, levels = c("VU", "EN", "CR", "CR (PE)", "EW", "EX")), #  facet_grid(~factor(team, levels=c('C', 'D', 'A', 'B')))
  labeller = label_wrap_gen(width=15)) +
  coord_flip() +
  ylim(c(0, 365)) +
  geom_text(aes(label = round(n, 1)), hjust = -.1, size = 6) +
  theme(strip.text = element_text(size = 20),
  axis.text=element_text(size=17),
        axis.title=element_text(size=20,face="bold")) +
  labs(y='Number of species', x='Class') +
  scale_x_discrete(limits=rev)

p

ggsave(
  paste0(here("figures/eu_redlist_nospecies_byclass_.png")),
  width = 14,
  height = 10,
  dpi = 600
)
