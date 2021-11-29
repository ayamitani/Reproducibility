#-----------------------------------------------------------------
# Author: Aya Mitani
# Last updated: 2021-11-24
# What: Line chart by year and borough
#----------------------------------------------------------------


bite <- read.csv(here("data", "dogbite.csv"), header = TRUE)

bite %>%
  group_by(Borough, Year) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = Year, y = n, color = Borough)) + 
  geom_point() + 
  geom_line() + 
  labs(y = "Count", x = "Year") + 
  scale_x_continuous(breaks = seq(2015, 2017, by = 1)) + 
  theme_classic()

ggsave(filename = here("results", "figure_line_yearborough.png"))