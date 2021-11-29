#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Bar chart of dog breed
#----------------------------------------------------------------


bite <- read.csv(here("data", "dogbite.csv"), header = TRUE)

bite %>%
  group_by(Breedclean) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = reorder(Breedclean, n), y = n)) + 
  geom_bar(stat = "identity", aes(fill = Breedclean)) + 
  theme_classic() + 
  theme(legend.position = "none") + 
  labs(y = "Count", x = "Breed") + 
  coord_flip()

ggsave(filename = here("results", "figure_barchart_dogbreed.png"))