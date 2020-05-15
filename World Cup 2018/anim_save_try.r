library(ggplot2)    # general plotting base
library(dplyr)      # data manipulation/tidying
library(ggsoccer)   # draw soccer field plot
library(ggimage)    # add soccer ball emoji + flags
library(extrafont)  # incorporate Dusha font into plots
library(gganimate)  # animate goal plots
library(tweenr)     # create in-between frames for data
library(purrr)      # for creating a list of dataframes for tweenr
library(countrycode)



### DATAFRAMES

cornerkick_data <- data.frame(
  x = 99, y = 0.3,
  x2 = 94, y2 = 47)

osako_gol <- data.frame(
  x = 94, y = 49,
  x2 = 100, y2 = 55.5)

ball_data <- data.frame(
  x = c(99, 94, 100),
  y = c(0.3, 47, 55.5),
  time = c(1, 2, 3))

player_label <- data.frame(
  x = c(92, 99), 
  y = c(49, 2),
  label = c("Osako", "Honda"))

wc_logo <- data.frame(
  x = 107,
  y = 85) %>% 
  mutate(
    image = "https://upload.wikimedia.org/wikipedia/en/thumb/6/67/2018_FIFA_World_Cup.svg/1200px-2018_FIFA_World_Cup.svg.png")

flag_data <- data.frame(
  x = c(110, 110),
  y = c( 13, 53),
  team = c("japan", "colombia")
) %>% 
  mutate(
    image = team %>% 
      countrycode(., origin = "country.name", destination = "iso2c")
  ) %>% 
  select(-team)




### PLOTTING




ganim <- ggplot(ball_data) +
  annotate_pitch() +
  theme_pitch() +
  theme(
    text = element_text(family = "Dusha V5"),
    plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  coord_flip(
    xlim = c(55, 112),
    ylim = c(-1, 101)) +
  geom_label(
    data = player_label, 
    aes(x = x, y = y,
        label = label),
    family = "Dusha V5") +
  geom_point(
    aes(x = 98, y = 50), size = 3, color = "green") +
  annotate(
    geom = "text", family = "Dusha V5", 
    hjust = c(0.5, 0.5, 0.5, 0.5),
    size = c(6.5, 4.5, 5, 3),
    label = c("Japan             (2) vs. Colombia             (1)",
              "Kagawa (PEN 6'), Quintero (39'), Osako (73')",
              "Japan press their man advantage, substitute Honda\ndelivers a delicious corner kick for Osako to (somehow) tower over\nColombia's defense and flick a header into the far corner!",
              "by Ryo Nakagawara (@R_by_Ryo)"),
    x = c(110, 105, 70, 53), 
    y = c(30, 30, 47, 85)) +
  ggimage::geom_emoji(              # soccer ball emoji
    aes(x = x, 
        y = y),
    image = "26bd", size = 0.035) +
  ggimage::geom_flag(               # Japan + Colombia flag
    data = flag_data,
    aes(x = x, y = y,
        image = image),       
    size = c(0.08, 0.08)) +
  geom_image(                       # World Cup logo
    data = wc_logo,     
    aes(x = x, y = y,
        image = image), size = 0.17) +
  # new gganimate code:
  transition_states(
    time, 
    transition_length = 0.5, 
    state_length = 0.0001,
    wrap = FALSE) +
  ease_aes("quadratic-out")



anim_save(ganim)


