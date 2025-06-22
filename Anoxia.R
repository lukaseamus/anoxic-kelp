require(tidyverse)
O2 <- read.csv("~/Desktop/PhD/Papers/Anoxia/Data/Oxygen.csv") %>%
  mutate(Treatment = rep(c("Control", "+ Sulphate"), each = 3951))

O2 %>%
  ggplot(aes(as.numeric(delta_t)/60, Value, colour = Treatment)) +
    geom_point(shape = 16, alpha = 0.2) +
    geom_smooth(method = "loess", span = 0.1, se = F) +
    labs(x = "Time (h)", y = expression(O[2]*" ("*mu*"M)")) +
    scale_colour_manual(values = c("purple", "goldenrod")) +
    theme_minimal() +
    theme(legend.position = c(0.9, 0.9))

require(seacarb)
TA <- read.csv("~/Desktop/PhD/Papers/Anoxia/Data/Alkalinity.csv") %>%
  mutate(DIC = carb(flag = 8, var1 = pH, var2 = TA, S = 35, T = 25)$DIC,
         H_CO3 = carb(flag = 8, var1 = pH, var2 = TA, S = 35, T = 25)$HCO3 +
                 carb(flag = 8, var1 = pH, var2 = TA, S = 35, T = 25)$CO3,
         TA_g = TA * Volume/1e3 / Mass,
         H_CO3_g = H_CO3 * Volume/1e3 / Mass)

# generate values for pH contours
pHcontour <- expand_grid(TA = seq(0, 60000, 500),
                         DIC = seq(0, 80000, 500)) %>%
  mutate(pH = carb(flag = 15, var1 = TA, var2 = DIC, S = 35, T = 25)$pH)

require(metR)
Fig1 <- 
TA %>%
  filter(Experiment == 1) %>%
  ggplot() +
    geom_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH), 
                 breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey") +
    geom_text_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH),
                      breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey",
                      skip = 0, label.placer = label_placer_fraction(frac = 0),
                      nudge_x = 1, nudge_y = -1, size = 3) +
    geom_point(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask)) +
    geom_path(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask), 
              arrow = arrow(length = unit(0.3, "cm"))) +
    scale_colour_manual(values = c("black", "darkred", "lightblue")) +
    labs(x = expression(C[T]*" (mM)"), y = expression(A[T]*" (mM)")) +
    lims(x = c(0, 80), y = c(0, 60)) +
    theme_minimal() +
    theme(legend.position = c(0.1, 0.9),
          legend.title = element_blank())

ggsave(Fig1, filename = "Fig1.pdf", path = "~/Desktop", device = cairo_pdf, 
       width = 10, height = 10, units = "cm")

# TA %>%
#   filter(Experiment == 1) %>%
#   ggplot(aes(Day, TA_g/1e3, colour = Treatment, group = Flask)) +
#   geom_point() +
#   geom_smooth(method = "loess", se = F, span = 0.7) +
#   scale_colour_manual(values = c("black", "darkred", "lightblue")) +
#   labs(x = "Time (d)", y = expression(A[T]*" (mmol g"^-1*")")) +
#   lims(x = c(0, 90), y = c(0, 4)) +
#   theme_minimal() +
#   theme(legend.position = c(0.1, 0.9),
#         legend.title = element_blank())

Fig2 <- 
TA %>%
  filter(Experiment == 1) %>%
  ggplot(aes(Day, H_CO3_g/1e3, colour = Treatment, group = Flask)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = 0.7) +
  scale_colour_manual(values = c("black", "darkred", "lightblue")) +
  labs(x = "Time (d)", y = expression(HCO[3]^"–"*" + "*CO[3]^"2–"*" (mmol g"^-1*")")) +
  lims(x = c(0, 90), y = c(0, 4)) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank())

ggsave(Fig2, filename = "Fig2.pdf", path = "~/Desktop", device = cairo_pdf, 
       width = 15, height = 10, units = "cm")


Fig3 <- 
TA %>%
  filter(Experiment == 2) %>%
  ggplot() +
  geom_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH), 
               breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey") +
  geom_text_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH),
                    breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey",
                    skip = 0, label.placer = label_placer_fraction(frac = 0),
                    nudge_x = 1, nudge_y = -1, size = 3) +
  geom_point(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask)) +
  geom_path(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask), 
            arrow = arrow(length = unit(0.3, "cm"))) +
  scale_colour_manual(values = c("black", "darkgreen", "goldenrod", "lightblue")) +
  labs(x = expression(C[T]*" (mM)"), y = expression(A[T]*" (mM)")) +
  lims(x = c(0, 80), y = c(0, 60)) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank())

ggsave(Fig3, filename = "Fig3.pdf", path = "~/Desktop", device = cairo_pdf, 
       width = 10, height = 10, units = "cm")

Fig4 <- 
TA %>%
  filter(Experiment == 2, Treatment != "Blank") %>%
  ggplot(aes(Day, H_CO3_g/1e3, colour = Treatment, group = Flask)) +
  geom_point() +
  geom_smooth(method = "loess", se = F, span = 0.8) +
  scale_colour_manual(values = c("darkgreen", "goldenrod", "lightblue")) +
  labs(x = "Time (d)", y = expression(HCO[3]^"–"*" + "*CO[3]^"2–"*" (mmol g"^-1*")")) +
  lims(x = c(0, 90), y = c(0, 15)) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank())

ggsave(Fig4, filename = "Fig4.pdf", path = "~/Desktop", device = cairo_pdf, 
       width = 15, height = 10, units = "cm")

TA %>%
  filter(Experiment == 3) %>%
  ggplot() +
  geom_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH), 
               breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey") +
  geom_text_contour(data = pHcontour, aes(x = DIC/1e3, y = TA/1e3, z = pH),
                    breaks = c(4, seq(4.6, 6.4, by = 0.2), 7, 8, 9), colour = "grey",
                    skip = 0, label.placer = label_placer_fraction(frac = 0),
                    nudge_x = 1, nudge_y = -1, size = 3) +
  geom_point(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask)) +
  geom_path(aes(DIC/1e3, TA/1e3, colour = Treatment, group = Flask), 
            arrow = arrow(length = unit(0.3, "cm"))) +
  scale_colour_manual(values = c("black", "grey")) +
  labs(x = expression(C[T]*" (mM)"), y = expression(A[T]*" (mM)")) +
  lims(x = c(0, 10), y = c(0, 10)) +
  theme_minimal() +
  theme(legend.position = c(0.1, 0.9),
        legend.title = element_blank())



?carbfull

carbfull(flag = 8, var1 = 8, var2 = 2300, S = 35, T = 25, Patm = 1)$CO2
carbfull(flag = 8, var1 = 4.6046, var2 = 1959.260636, S = 35, T = 25, Patm = 1, HSt = 159630)$CO2
carbfull(flag = 8, var1 = 4.6046, var2 = 1959.260636, S = 35, T = 25, Patm = 1, NH4t = 90465000)$CO2
