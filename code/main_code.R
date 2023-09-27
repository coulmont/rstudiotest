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



# graphique barres


continuous_scale(
  aesthetics="color",
  scale_name = "test",
  palette = scales::gradient_n_pal(scales::brewer_pal(type="div", palette="RdYlBu", direction=1)(9), 
                           values=NULL, space="Lab")
)




scale_colour_distiller_9 <- function(..., type = "seq", 
                                     palette = 1, direction = -1, 
                                     values = NULL, space = "Lab",
                                     na.value = "grey50", 
                                     guide = "colourbar", 
                                     aesthetics = "colour") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    scale_name = "test",
    palette = scales::gradient_n_pal(scales::brewer_pal(type, palette, direction)(11), values, space),
    na.value = na.value, guide = guide, ...
  )
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}



scale_fill_distiller_9 <- function(..., type = "seq", 
                                     palette = 1, direction = -1, 
                                     values = NULL, space = "Lab",
                                     na.value = "grey50", 
                                     guide = "colourbar", 
                                     aesthetics = "fill") {
  # warn about using a qualitative brewer palette to generate the gradient
  type <- rlang::arg_match0(type, c("seq", "div", "qual"))
  if (type == "qual") {
    cli::cli_warn(c(
      "Using a discrete colour palette in a continuous scale",
      "i" = "Consider using {.code type = \"seq\"} or {.code type = \"div\"} instead"
    ))
  }
  continuous_scale(
    aesthetics,
    scale_name = "test",
    palette = scales::gradient_n_pal(scales::brewer_pal(type, palette, direction)(11), 
                                     values, space),
    na.value = na.value, guide = guide, ...
  )
  # NB: 6-7 colours per palette gives nice gradients; more results in more saturated colours which do not look as good
  # For diverging scales, you need an odd number to make sure the mid-point is in the center
}



temperatures %>%
  ggplot(aes(date,y=1)) +
  geom_tile(aes(color=color_temp,
                fill=color_temp),linewidth=1) +
  scale_colour_distiller_9(palette="RdYlBu") +
  scale_fill_distiller_9(palette="RdYlBu") +
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
