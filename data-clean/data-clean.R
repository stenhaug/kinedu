library(tidyverse)

areas <- read_rds("data-clean/areas.rds")
areas %>% View()

milestones <- read_rds("data-clean/milestones.rds")
milestones %>% View()

r_wide <- read_rds("data-clean/responses_wide.rds")
r_wide %>% View()

r_long <- read_rds("data-clean/responses_long.rds")
r_long %>% View()

