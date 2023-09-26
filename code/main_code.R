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
  ggplot(aes(date,y=1)) +
  geom_tile(aes(color=color_temp),size=1) +
  scale_color_distiller(palette="RdYlBu") +
  scale_y_continuous(expand=expansion(add=c(0,0)) ) +
  scale_x_date(expand=expansion(add=c(0,0)) ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),panel.grid = element_blank(),
        legend.position = "none") +
  labs(y=NULL,x=NULL,
       title = "Écart à la température moyenne",
       caption = "Source : Nasa, GISS temp - GLB.Ts+dSST") 

# palette manuelle très détaillée
library(RColorBrewer)
zCuts <-
  seq(-.5, 1.1, length.out = 20)

# myPallette <-
#   c(rev(brewer.pal(9, "YlOrRd"))
#     , "white"
#     , brewer.pal(9, "Blues"))


myPallette <-
  c(rev(brewer.pal(9, "Blues"))
    , "white"
    , brewer.pal(9, "YlOrRd") )

temperatures %>%
  ggplot(aes(date,y=1)) +
  geom_tile(aes(color= cut(color_temp, zCuts) ),size=1) +
  #scale_color_distiller(palette="RdYlBu") +
  scale_color_manual(values = myPallette
                    , drop = FALSE) +
  scale_y_continuous(expand=expansion(add=c(0,0)) ) +
  scale_x_date(expand=expansion(add=c(0,0)) ) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),panel.grid = element_blank(),
        legend.position = "none") +
  labs(y=NULL,x=NULL,
       title = "Écart à la température moyenne",
       caption = "Source : Nasa, GISS temp - GLB.Ts+dSST") 



temperatures %>%
  mutate(date_group = ymd(paste("2000",month,1))) %>%
  ggplot(aes(date_group, diff_temp, group = Year)) +
  geom_line(aes(color=color_temp)) +
  coord_polar() +
  scale_y_continuous(limits=c(-2,2))


temperatures %>%
  ggplot(aes())
