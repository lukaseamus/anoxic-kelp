# 1. Load data ####
require(tidyverse)
require(magrittr)
require(here)

TA <- here("Alkalinity", "Alkalinity.csv") %>% 
  read_csv() %>%
  filter(Experiment == 2) %>%
  drop_na(TA) %>%
  mutate(Treatment = Treatment %>% fct_relevel("Blank"),
         Flask = Flask %>% as_factor()) %T>%
  print()

TA_prior_posterior <- here("Alkalinity", "RDS", "TA_prior_posterior.rds") %>%
  read_rds() %>%
  mutate(Treatment = Treatment %>% fct_relevel("Prior", "Blank")) %T>%
  print()

# Predict across full predictor range
require(tidybayes) # needed for qi()
TA_prediction <- TA_prior_posterior %>%
  # Use 0 instead of min(TA$Day) which is 1
  expand_grid(Day = seq(0, max(TA$Day), length.out = 200)) %>%
  mutate( 
    mu = TA_prod * plogis( r * ( Day - m ) ) + TA_base,
    TA = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
   summarise(
    across(
      c(mu, TA),
      list(median = median, 
           lower_0.5 = ~ qi(.x, .width = .5)[1],
           upper_0.5 = ~ qi(.x, .width = .5)[2],
           lower_0.8 = ~ qi(.x, .width = .8)[1],
           upper_0.8 = ~ qi(.x, .width = .8)[2],
           lower_0.9 = ~ qi(.x, .width = .9)[1],
           upper_0.9 = ~ qi(.x, .width = .9)[2]),
      .names = "{.col}.{.fn}"
    ),
    .by = c(Day, Treatment)
  ) %>%
  rename(mu = mu.median, TA = TA.median) %>%
  pivot_longer(cols = contains("lower") | contains("upper")) %>%
  separate(col = name, into = c("name", ".width"), sep = "_(?=[^_]*$)") %>%
  pivot_wider(names_from = name, values_from = value) %T>%
  print()

# 2. Aesthetics ####
# Data points
TA_points <- TA %>%
  select(Treatment, Day, TA) %>%
  rename(x = Day, y = TA) %>% # Rename to x and y for consistency
  mutate(
    y = y/1e3, # convert µM to mM
    alpha = 0.5, # Baseline transparency
    colour = case_when( # Colours
      Treatment == "Blank" ~ "#6f5229",
      Treatment == "Control" ~ "#627d0e",
      Treatment == "Sulfate" ~ "#7030a5",
      Treatment == "Sulfur" ~ "#f1c700"
    )
  ) %T>%
  print()

# Text
TA_text <- TA_prediction %>%
  distinct(Treatment) %>%
  mutate(
    label = Treatment %>% as.character(),
    colour = case_when(
      Treatment == "Prior" ~ "#b5b8ba", # here I also have Prior
      Treatment == "Blank" ~ "#6f5229",
      Treatment == "Control" ~ "#627d0e",
      Treatment == "Sulfate" ~ "#7030a5",
      Treatment == "Sulfur" ~ "#f1c700"
    ),
    x = 2, y = 80
  ) %T>%
  print()

# Lines
TA_line <- TA_prediction %>%
  filter(.width == .5) %>% # line is just repeated across ribbon stats
  select(Treatment, Day, TA) %>%
  rename(x = Day, y = TA) %>%
  mutate(
    y = y/1e3, # convert µM to mM
    colour = case_when(
      Treatment == "Prior" ~ "#b5b8ba",
      Treatment == "Blank" ~ "#6f5229",
      Treatment == "Control" ~ "#627d0e",
      Treatment == "Sulfate" ~ "#7030a5",
      Treatment == "Sulfur" ~ "#f1c700"
    )
  ) %T>%
  print()

# Ribbons
TA_ribbon <- TA_prediction %>%
  group_by(Treatment, .width) %>%
  reframe(
    x = c(Day, rev(Day)),
    y = c(TA.upper, rev(TA.lower))
  ) %>%
  mutate(
    y = y/1e3, # convert µM to mM
    alpha = case_when(
      .width == .9 ~ .5, 
      .width == .8 ~ .4, 
      .width == .5 ~ .3
    ),
    fill = case_when(
      Treatment == "Prior" ~ "#b5b8ba",
      Treatment == "Blank" ~ "#6f5229",
      Treatment == "Control" ~ "#627d0e",
      Treatment == "Sulfate" ~ "#7030a5",
      Treatment == "Sulfur" ~ "#f1c700"
    )
  ) %>%
  select(-.width) %T>%
  print()

# Densities
TA_dens <- TA_prior_posterior %>%
  mutate(TA_prod = TA_prod/1e3) %>% # convert µM to mM
  group_by(Treatment) %>% 
  reframe(
    x = c(0, density(TA_prod, n = 2^10, from = 0, to = 120, bw = 120 * 0.01)$x, 120),
    y = c(0, density(TA_prod, n = 2^10, from = 0, to = 120, bw = 120 * 0.01)$y, 0)
  ) %>% # Normalise density area to 1e3 (chosen to match y axis text spacing)
  mutate( y = y * 1e3 / ( sum(y) * ( x[3] - x[2] ) ) , .by = Treatment ) %>%
  mutate(
    fill = case_when(
      Treatment == "Prior" ~ "#b5b8ba",
      Treatment == "Blank" ~ "#6f5229",
      Treatment == "Control" ~ "#627d0e",
      Treatment == "Sulfate" ~ "#7030a5",
      Treatment == "Sulfur" ~ "#f1c700"
    )
  ) %T>%
  print()

# Kelp illustration
TA_image <- TA %>%
  distinct(Treatment) %>%
  filter(Treatment != "Blank") %>% # no kelp in Blank
  mutate(
    image = here("Figures", "Ecklonia.pdf"),
    alpha = 1, # baseline opacity
    x = 233, y = 63
  ) %T>%
  print()

# 3. Static figures ####
# Define custom theme
mytheme <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  plot.margin = margin(0.2, 0.5, 0.2, 0.2, unit = "cm"),
  axis.line = element_line(),
  axis.title = element_text(size = 12, hjust = 0),
  axis.text = element_text(size = 10, colour = "black"),
  axis.ticks.length = unit(.25, "cm"),
  axis.ticks = element_line(colour = "black", lineend = "square"),
  legend.key = element_blank(),
  legend.key.width = unit(.25, "cm"),
  legend.key.height = unit(.45, "cm"),
  legend.key.spacing.x = unit(.5, "cm"),
  legend.key.spacing.y = unit(.05, "cm"),
  legend.background = element_blank(),
  legend.position = "top",
  legend.justification = 0,
  legend.text = element_text(size = 12, hjust = 0),
  legend.title = element_blank(),
  legend.margin = margin(0, 0, 0, 0, unit = "cm"),
  strip.background = element_blank(),
  strip.text = element_text(size = 12, hjust = 0),
  panel.spacing = unit(1, "cm"),
  text = element_text(family = "Futura")
)

# Top panel
require(ggimage)
ggplot() +
  geom_point(
    data = TA_points,
    aes(x, y, colour = colour, alpha = alpha),
    shape = 16, size = 2.35
  ) +
  geom_line(
    data = TA_line,
    aes(x, y, colour = colour)
  ) +
  geom_polygon(
    data = TA_ribbon,
    aes(x, y, fill = fill, alpha = alpha, group = alpha)
  ) +
  geom_text(
    data = TA_text,
    aes(x, y, label = Treatment, colour = colour),
    hjust = 0, vjust = 1, size.unit = "pt", 
    size = 12, family = "Futura"
  ) +
  geom_image(
    data = TA_image,
    aes(x, y, image = image, alpha = alpha),
    size = .4, by = "height"
  ) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_x_continuous(breaks = seq(0, 240, 60)) +
  facet_grid(~ Treatment, space = "free", scales = "free") +
  labs(x = "Time since kelp addition (days)", y = "Total alkalinity (mM)") +
  coord_cartesian(xlim = c(0, 240), ylim = c(0, 80), 
                  expand = FALSE, clip = "off") +
  mytheme

# Bottom panel
ggplot() +
  geom_polygon(
    data = TA_dens,
    aes(x = x, y = y, fill = fill)
  ) +
  scale_fill_identity() +
  facet_grid(~ Treatment, space = "free", scales = "free") +
  labs(x = "Produced total alkalinity (mM)",
       # Ensure same y axis spacing
       y = "Total alkalinity (mM)") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70), expand = F, clip = "off") +
  mytheme +
  # These will be hidden (transparent) but ensure identical spacing
  theme(axis.title.y = element_text(colour = "transparent"),
        axis.text.y = element_text(colour = "transparent"),
        axis.ticks.y = element_line(colour = "transparent"),
        axis.line.y = element_line(colour = "transparent"))

# 4. Animation ####
# Define enter/exit functions
enter <- function(data){
  data$alpha <- 0
  data
}

exit <- function(data){
  data$alpha <- 0
  data
}

# Tween points
require(tweenr)
TA_points_ani <- bind_rows( 
  tween_state( # Points are arbitrarily paired
    TA_points %>% filter(Treatment == "Prior"), 
    TA_points %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    TA_points %>% filter(Treatment == "Blank"), 
    TA_points %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    TA_points %>% filter(Treatment == "Control"), 
    TA_points %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    TA_points %>% filter(Treatment == "Sulfate"), 
    TA_points %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_state(
    TA_points %>% filter(Treatment == "Sulfur"), 
    TA_points %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# Tween text
TA_text_ani <- bind_rows( 
  tween_state( # Points are arbitrarily paired
    TA_text %>% filter(Treatment == "Prior"), 
    TA_text %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    TA_text %>% filter(Treatment == "Blank"), 
    TA_text %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    TA_text %>% filter(Treatment == "Control"), 
    TA_text %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    TA_text %>% filter(Treatment == "Sulfate"), 
    TA_text %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_state(
    TA_text %>% filter(Treatment == "Sulfur"), 
    TA_text %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# Tween lines
# tween_state also works for some lines, but 
# I believe it is more stable to use tween_path.
require(transformr)
TA_line_ani <- bind_rows(
  tween_path(
    TA_line %>% filter(Treatment == "Prior"), 
    TA_line %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    TA_line %>% filter(Treatment == "Blank"), 
    TA_line %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    TA_line %>% filter(Treatment == "Control"), 
    TA_line %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    TA_line %>% filter(Treatment == "Sulfate"), 
    TA_line %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    TA_line %>% filter(Treatment == "Sulfur"), 
    TA_line %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# Tween ribbons
# tween_path is more stable than tween_polygon for ribbons.
TA_ribbon_ani <- bind_rows(
  tween_path(
    TA_ribbon %>% filter(Treatment == "Prior"),
    TA_ribbon %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_path(
    TA_ribbon %>% filter(Treatment == "Blank"),
    TA_ribbon %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_path(
    TA_ribbon %>% filter(Treatment == "Control"),
    TA_ribbon %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_path(
    TA_ribbon %>% filter(Treatment == "Sulfate"),
    TA_ribbon %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_path(
    TA_ribbon %>% filter(Treatment == "Sulfur"),
    TA_ribbon %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# Tween densities
TA_dens_ani <- bind_rows(
  tween_polygon(
    TA_dens %>% filter(Treatment == "Prior"),
    TA_dens %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50),
  tween_polygon(
    TA_dens %>% filter(Treatment == "Blank"),
    TA_dens %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_polygon(
    TA_dens %>% filter(Treatment == "Control"),
    TA_dens %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_polygon(
    TA_dens %>% filter(Treatment == "Sulfate"),
    TA_dens %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_polygon(
    TA_dens %>% filter(Treatment == "Sulfur"),
    TA_dens %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# Tween illustration
TA_image_ani <- bind_rows( 
  tween_state(
    TA_image %>% filter(Treatment == "Prior"), 
    TA_image %>% filter(Treatment == "Blank"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50),
  tween_state(
    TA_image %>% filter(Treatment == "Blank"), 
    TA_image %>% filter(Treatment == "Control"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 1 * 150),
  tween_state(
    TA_image %>% filter(Treatment == "Control"), 
    TA_image %>% filter(Treatment == "Sulfate"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 2 * 150),
  tween_state(
    TA_image %>% filter(Treatment == "Sulfate"), 
    TA_image %>% filter(Treatment == "Sulfur"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 3 * 150),
  tween_state(
    TA_image %>% filter(Treatment == "Sulfur"), 
    TA_image %>% filter(Treatment == "Prior"),
    ease = "cubic-in-out", nframes = 100,
    enter = enter, exit = exit
  ) %>%
    keep_state(nframes = 50) %>%
    mutate(.frame = .frame + 4 * 150)
) %T>%
  print()

# 5. Animated figures ####
require(gganimate) # also install gifski
require(ggtext) # for html syntax in gif

# Top panel
( ggplot() +
    geom_point(
      data = TA_points_ani,
      aes(x, y, colour = colour, alpha = alpha),
      shape = 16, size = 2.35
    ) +
    geom_line(
      data = TA_line_ani,
      aes(x, y, colour = colour)
    ) +
    geom_polygon(
      data = TA_ribbon_ani,
      aes(x, y, fill = fill, alpha = alpha, group = alpha)
    ) +
    geom_text(
      data = TA_text_ani,
      aes(x, y, label = Treatment, colour = colour),
      hjust = 0, vjust = 1, size.unit = "pt", 
      size = 12, family = "Futura"
    ) +
    geom_image(
      data = TA_image_ani,
      aes(x, y, image = image, alpha = alpha),
      size = .4, by = "height"
    ) +
    scale_alpha_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    labs(x = "Time since kelp addition (days)", y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 80), 
                    expand = FALSE, clip = "off") +
    transition_manual(.frame) +
    mytheme ) %>%
  animate(
    fps = 25, # optimal gif frame rate (4 centiseconds per frame) with reasonable file size
    duration = 5 * 3, # number of transitions (9) * time per transition (3 s) to set speed
    detail = 2, # 25*5*3 is 1/2 of all frames (5*150), so the skipped frames can add detail
    width = 20, height = 6,
    units = "cm", res = 300,
    renderer = gifski_renderer(),
    device = "ragg_png"
  ) %>%
  anim_save(
    filename = "TA_top.gif", 
    path = here("Figures", "Animation")
  )

# Bottom panel
( ggplot() +
    geom_polygon(
      data = TA_dens_ani,
      aes(x = x, y = y, fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = "Produced total alkalinity (mM)", y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 100), ylim = c(0, 70), expand = F, clip = "off") +
    transition_manual(.frame) +
    mytheme +
    theme(
      # These will be hidden (transparent) but ensure identical spacing
      axis.title.y = element_text(colour = "transparent"),
      axis.text.y = element_text(colour = "transparent"),
      axis.ticks.y = element_line(colour = "transparent"),
      axis.line.y = element_line(colour = "transparent")
    ) ) %>%
  animate(
    fps = 25, duration = 5 * 3, detail = 2,
    width = 20, height = 4, units = "cm", res = 300, 
    renderer = gifski_renderer(), device = "ragg_png"
  ) %>%
  anim_save(
    filename = "TA_bottom.gif", 
    path = here("Figures", "Animation")
  )