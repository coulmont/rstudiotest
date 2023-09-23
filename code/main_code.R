library(tidyverse)

df <- read_csv("data/GLB.Ts+dSST.csv",skip = 1)

temperatures <- df %>%
  select(Year:Dec) %>%
  mutate( across(Jan:Dec, ~as.numeric(.) ) ) %>%
  pivot_longer(-Year,names_to = "month",values_to="diff_temp") %>% #View()
  filter(!is.na(diff_temp))


temperatures %>%
  mutate(date = ymd(paste(Year,month,1))) %>%
  group_by(Year) %>%
  mutate(color_temp = mean(diff_temp)) %>%
  ggplot(aes(date,diff_temp)) +
  geom_point(aes(color=color_temp),size=1) +
  scale_color_distiller(palette="RdYlBu") +
  theme(panel.background = element_rect(fill="#555555"),
        panel.grid = element_line(color="#999999"))

temperatures %>%
  ggplot(aes())
