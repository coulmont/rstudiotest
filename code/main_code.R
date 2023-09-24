library(tidyverse)

df <- read_csv("data/GLB.Ts+dSST.csv",skip = 1)

temperatures <- df %>%
  select(Year:Dec) %>%
  mutate( across(Jan:Dec, ~as.numeric(.) ) ) %>%
  pivot_longer(-Year,names_to = "month",values_to="diff_temp") %>% #View()
  filter(!is.na(diff_temp))


temperatures <- temperatures %>%
  mutate(date = ymd(paste(Year,month,1))) %>%
  group_by(Year) %>%
  mutate(color_temp = mean(diff_temp))
  

temperatures %>%
  ggplot(aes(date,diff_temp)) +
  geom_point(aes(color=color_temp),size=1) +
  scale_color_distiller(palette="RdYlBu") +
  theme(panel.background = element_rect(fill="#555555"),
        panel.grid = element_line(color="#999999"))


temperatures %>%
  mutate(date_group = ymd(paste("2000",month,1))) %>%
  ggplot(aes(date_group, diff_temp, group = Year)) +
  geom_line(aes(color=color_temp)) +
  coord_polar() +
  scale_y_continuous(limits=c(-2,2))


temperatures %>%
  ggplot(aes())
