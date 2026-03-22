require(tidyverse)
require(here)
require(magrittr)

TA <- here("Alkalinity", "Alkalinity.csv") %>% 
  read_csv() %>%
  # Count BDL as 0
  mutate(NOx = if_else(NOx == "<50", "0", NOx) %>%
           as.numeric()) %T>%
  print()


mytheme <- theme(panel.background = element_blank(),
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
                 strip.text = element_text(size = 12, hjust = 0, face = "bold"),
                 panel.spacing.x = unit(1, "cm"),
                 panel.spacing.y = unit(0.8, "cm"),
                 text = element_text(family = "Futura"))

TA %>%
  filter(Experiment == 1) %>%
  ggplot(aes(Day, TA/1e3, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 2) %>%
  drop_na(TA) %>%
  ggplot(aes(Day, TA/1e3, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 3) %>%
  drop_na(TA) %>%
  ggplot(aes(Day, TA/1e3, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 1) %>%
  drop_na(NH4) %>%
  ggplot(aes(Day, NH4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 2) %>%
  drop_na(NH4) %>%
  ggplot(aes(Day, NH4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 3) %>%
  drop_na(NH4) %>%
  ggplot(aes(Day, NH4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme



TA %>%
  filter(Experiment == 1) %>%
  drop_na(PO4) %>%
  ggplot(aes(Day, PO4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 2) %>%
  drop_na(PO4) %>%
  ggplot(aes(Day, PO4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 3) %>%
  drop_na(PO4) %>%
  ggplot(aes(Day, PO4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 1) %>%
  drop_na(NOx) %>%
  ggplot(aes(Day, NOx, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


TA %>%
  filter(Experiment == 2) %>%
  drop_na(NOx) %>%
  ggplot(aes(Day, NOx, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 3) %>%
  drop_na(NOx) %>%
  ggplot(aes(Day, NOx, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme


# Focus on Experiment 2 ammonia and TA


TA %>%
  filter(Experiment == 2) %>%
  drop_na(TA) %>%
  ggplot(aes(Day, TA/1e3, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 2) %>%
  drop_na(NH4) %>%
  ggplot(aes(Day, NH4, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA %>%
  filter(Experiment == 2) %>%
  drop_na(pH) %>%
  ggplot(aes(Day, pH, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

TA_Exp2 <- TA %>%
  filter(Experiment == 2) %>%
  drop_na(TA) %>%
  mutate(Treatment = Treatment %>% fct(),
         Flask = Flask %>% as_factor()) %T>%
  print()

NH4_Exp2 <- TA %>%
  filter(Experiment == 2) %>%
  drop_na(NH4) %>%
  mutate(NH4_uM = NH4 / 14.007,  # µg N in NH4 / 14.007 is µM N in NH4 = µM NH4
         Treatment = Treatment %>% fct(),
         Flask = Flask %>% as_factor()) %T>%
  print()

NH4_Exp2 %>%
  ggplot(aes(Day, NH4_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.5) +
    geom_line(aes(group = Flask)) +
    mytheme

# 2.1.2 Stan model ####
require(cmdstanr)
TA_model <- here("Alkalinity", "Stan", "TA_Exp2.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

require(tidybayes)
TA_samples <- TA_model$sample(
          data = TA_Exp2 %>%
            select(Day, TA, Treatment, Flask) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
TA_samples$draws() %>%
  write_rds(here("Alkalinity", "RDS", "TA_samples.rds"))
TA_samples$draws(format = "df") %>%
  write_rds(here("Alkalinity", "RDS", "TA_samples_df.rds"))

# 2.1.3 Model checks ####
# Rhat
TA_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000682.

# Chains
require(bayesplot)
TA_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
TA_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_TA_base", "log_TA_prod_mu", 
                      "log_r_mu", "log_m_mu"))
# No correlation between logistic parameters

TA_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_TA_prod_T[1]", "log_TA_prod_F[1]",
                      "log_TA_prod_T[2]", "log_TA_prod_F[2]"))
TA_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_r_T[1]", "log_r_F[1]",
                      "log_r_T[2]", "log_r_F[2]"))
TA_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_m_T[1]", "log_m_F[1]",
                      "log_m_T[2]", "log_m_F[2]"))
# Strong correlation between flasks and treatments, meaning they are hardly
# distinguishable, especially for TA_prod and m. Nonetheless, I choose this as the
# optimal model.

# 2.1.4 Prior-posterior comparison ####
source("functions.R")
TA_prior <- prior_samples(
  model = TA_model,
  data = TA_Exp2 %>%
    select(Day, TA, Treatment, Flask) %>%
    compose_data()
)

TA_prior %>% 
  prior_posterior_draws(
    posterior_samples = TA_samples,
    group = TA_Exp2 %>% select(Treatment, Flask),
    parameters = c("log_TA_base", "log_TA_prod_mu", "log_r_mu", "log_m_mu", "theta", 
                   "log_TA_prod_T[Treatment]", "log_r_T[Treatment]", "log_m_T[Treatment]",
                   "log_TA_prod_F[Flask]", "log_r_F[Flask]", "log_m_F[Flask]",
                   "log_TA_prod_T_sigma", "log_r_T_sigma", "log_m_T_sigma",
                   "log_TA_prod_F_sigma", "log_r_F_sigma", "log_m_F_sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(
    group_name = "Treatment",
    second_group_name = "Flask"
  )


# 2.1.5 Parameters ####
TA_prior_posterior <- TA_prior %>% 
  prior_posterior_draws(
    posterior_samples = TA_samples,
    group = TA_Exp2 %>% select(Treatment),
    parameters = c("log_TA_base", "theta", "log_TA_prod_T[Treatment]",
                   "log_r_T[Treatment]", "log_m_T[Treatment]",
                   "log_TA_prod_F_sigma", "log_r_F_sigma", "log_m_F_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate parameters for new flasks
    TA_base = exp( log_TA_base ),
    TA_prod = exp( rnorm( n() , log_TA_prod_T , log_TA_prod_F_sigma ) ),
    r = exp( rnorm( n() , log_r_T , log_r_F_sigma ) ),
    m = exp( rnorm( n() , log_m_T , log_m_F_sigma ) )
  ) %>% # Embed prior in treatments
  filter(Treatment == "Control" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    Treatment = if_else(
      distribution == "prior", "Prior", Treatment
    ) %>% fct()
  ) %>%
  select(starts_with("."), Treatment, TA_base, 
         TA_prod, r, m, theta, log_TA_prod_F_sigma) %T>%
  print()


# Save parameter distributions
TA_prior_posterior %>%
  write_rds(here("Alkalinity", "RDS", "TA_prior_posterior.rds"))

# 2.1.6 Prediction ####
TA_prediction <- TA_prior_posterior %>%
  spread_continuous(
    data = TA_Exp2,
    predictor_name = "Day",
    group_name = "Treatment"
  ) %>%
  mutate( 
    mu = TA_prod * plogis( r * ( Day - m ) ) + TA_base,
    TA = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
  group_by(Treatment, Day) %>%
  median_qi(mu, TA, .width = c(.5, .8, .9)) %T>%
  print()

TA_prediction %>%
  write_rds(here("Alkalinity", "RDS", "TA_prediction.rds"))


# 2.1.2 Stan model ####
NH4_model <- here("Alkalinity", "Stan", "NH4_Exp2.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

NH4_samples <- NH4_model$sample(
          data = NH4_Exp2 %>%
            select(Day, NH4_uM, Treatment, Flask) %>%
            rename(NH4 = NH4_uM) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4
        ) %T>%
  print()

# Save draws
NH4_samples$draws() %>%
  write_rds(here("Alkalinity", "RDS", "NH4_samples.rds"))
NH4_samples$draws(format = "df") %>%
  write_rds(here("Alkalinity", "RDS", "NH4_samples_df.rds"))

# 2.1.3 Model checks ####
# Rhat
NH4_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.000138.

# Chains
NH4_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
NH4_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_NH4_base", "log_NH4_prod_mu", 
                      "log_r_mu", "log_m_mu"))
# No correlation between logistic parameters

NH4_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_NH4_prod_T[1]", "log_NH4_prod_F[1]",
                      "log_NH4_prod_T[2]", "log_NH4_prod_F[2]"))
NH4_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_r_T[1]", "log_r_F[1]",
                      "log_r_T[2]", "log_r_F[2]"))
NH4_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("log_m_T[1]", "log_m_F[1]",
                      "log_m_T[2]", "log_m_F[2]"))
# Strong correlation between flasks and treatments, meaning they are hardly
# distinguishable, especially for m. Nonetheless, I choose this as the
# optimal model.

# 2.1.4 Prior-posterior comparison ####
NH4_prior <- prior_samples(
  model = NH4_model,
  data = NH4_Exp2 %>%
    select(Day, NH4_uM, Treatment, Flask) %>%
    rename(NH4 = NH4_uM) %>%
    compose_data()
)

NH4_prior %>% 
  prior_posterior_draws(
    posterior_samples = NH4_samples,
    group = NH4_Exp2 %>% select(Treatment, Flask),
    parameters = c("log_NH4_base", "log_NH4_prod_mu", "log_r_mu", "log_m_mu", "theta", 
                   "log_NH4_prod_T[Treatment]", "log_r_T[Treatment]", "log_m_T[Treatment]",
                   "log_NH4_prod_F[Flask]", "log_r_F[Flask]", "log_m_F[Flask]",
                   "log_NH4_prod_T_sigma", "log_r_T_sigma", "log_m_T_sigma",
                   "log_NH4_prod_F_sigma", "log_r_F_sigma", "log_m_F_sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(
    group_name = "Treatment",
    second_group_name = "Flask"
  )


# 2.1.5 Parameters ####
NH4_prior_posterior <- NH4_prior %>% 
  prior_posterior_draws(
    posterior_samples = NH4_samples,
    group = NH4_Exp2 %>% select(Treatment),
    parameters = c("log_NH4_base", "theta", "log_NH4_prod_T[Treatment]",
                   "log_r_T[Treatment]", "log_m_T[Treatment]",
                   "log_NH4_prod_F_sigma", "log_r_F_sigma", "log_m_F_sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate parameters for new flasks
    NH4_base = exp( log_NH4_base ),
    NH4_prod = exp( rnorm( n() , log_NH4_prod_T , log_NH4_prod_F_sigma ) ),
    r = exp( rnorm( n() , log_r_T , log_r_F_sigma ) ),
    m = exp( rnorm( n() , log_m_T , log_m_F_sigma ) )
  ) %>%
  filter(Treatment == "Control" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    Treatment = if_else(
      distribution == "prior", "Prior", Treatment
    ) %>% fct()
  ) %>%
  select(starts_with("."), Treatment, NH4_base, 
         NH4_prod, r, m, theta, log_NH4_prod_F_sigma) %T>%
  print()


# Save parameter distributions
NH4_prior_posterior %>%
  write_rds(here("Alkalinity", "RDS", "NH4_prior_posterior.rds"))

# 2.1.6 Prediction ####
NH4_prediction <- NH4_prior_posterior %>%
  spread_continuous(
    data = NH4_Exp2,
    predictor_name = "Day",
    group_name = "Treatment"
  ) %>%
  mutate( 
    mu = NH4_prod * plogis( r * ( Day - m ) ) + NH4_base,
    NH4 = rgamma( n() , mu / theta , 1 / theta )
  ) %>%
  group_by(Treatment, Day) %>%
  median_qi(mu, NH4, .width = c(.5, .8, .9)) %T>%
  print()

NH4_prediction %>%
  write_rds(here("Alkalinity", "RDS", "NH4_prediction.rds"))


# Figures
# Concentration over time
Fig_1a <- TA_prediction %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_point(data = TA_Exp2,
               aes(Day, TA / 1e3, colour = Treatment), 
               shape = 16, size = 2.5, alpha = 0.7) +
    # geom_line(data = TA_Exp2,
    #           aes(Day, TA / 1e3, group = Flask, colour = Treatment),
    #           alpha = 0.7) + 
    geom_line(aes(Day, TA / 1e3, colour = Treatment)) +
    geom_ribbon(aes(Day, ymin = TA.lower / 1e3, 
                    ymax = TA.upper / 1e3,
                    alpha = factor(.width),
                    fill = Treatment)) +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time since kelp addition (days)",
         y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 80), 
                    expand = FALSE, clip = "off") +
    mytheme +
    # Strip text creates a lot of space
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"))

Fig_1a

# TA production rate
Fig_1b <- TA_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_density(aes(r, fill = Treatment),
                 n = 2^10, bw = 0.3 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 0.3),
                       breaks = seq(0, 0.3, 0.1),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 3))),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = expression("Total alkalinity production (day"^-1*")")) +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_text(margin = margin(t = 0)), # counteract superscript
          plot.margin = margin(0.2, 0.5, 0, 0.2, unit = "cm"))

Fig_1b

# Time of maximal production rate
Fig_1c <- TA_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_density(aes(m, fill = Treatment),
                 n = 2^10, bw = 120 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 120),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time of maximal production (days)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(0, 0.5, 0, 0.2, unit = "cm"))

Fig_1c

# Produced TA
# When scales are different a slightly different approach is needed:
# 1. facet_wrap to give treatments different distribution heights
# 2. facetted_pos_scales to give treatments different scales
# 3. stat_density_ridges to give treatments different bandwidths

require(ggh4x)
require(ggridges)
Fig_1d <- TA_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  mutate(Treatment = Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) %>%
  ggplot() +
    stat_density_ridges(aes(TA_prod / 1e3, y = 0, fill = Treatment),
                        from = 0, to = c(9, rep(90, 3)),
                        n = 2^10, bandwidth = c(9, rep(90, 3)) * 0.02, 
                        colour = NA) +
    scale_fill_manual(values = c("#6f5229", "#627d0e", "#7030a5", "#f1c700"),
                      guide = "none") +
    facet_wrap(~ Treatment, scales = "free", nrow = 1) +
    facetted_pos_scales(
      x = list(
        Treatment == "Blank" ~ scale_x_continuous(
          limits = c(0, 9),
          breaks = seq(0, 9, 3),
          oob = scales::oob_keep
        ),
        Treatment != "Blank" ~ scale_x_continuous(
          limits = c(0, 90),
          breaks = seq(0, 90, 30),
          oob = scales::oob_keep
        )
      )
    ) +
    labs(x = "Produced total alkalinity (mM)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          # Space needed on top because all denisties have same height
          plot.margin = margin(0.2, 0.5, 0, 0.2, unit = "cm"))

Fig_1d

# Surplus TA (produced TA - blank TA)
TA_surplus <- TA_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  select(starts_with("."), Treatment, TA_prod) %>%
  pivot_wider(names_from = Treatment, values_from = TA_prod) %>%
  mutate(Surplus_Control = Control - Blank,
         Surplus_Sulfate = Sulfate - Blank,
         Surplus_Sulfur = Sulfur - Blank) %>%
  select(starts_with("."), starts_with("Surplus")) %>%
  pivot_longer(cols = starts_with("Surplus"),
               names_to = "Treatment",
               values_to = "TA_surplus",
               names_prefix = "Surplus_") %>%
  mutate(Treatment = Treatment %>% fct()) %T>%
  print()


CN <- read_csv("Carbon.csv") %T>%
  print()

# Add kelp mass and carbon content to TA_surplus
TA_surplus %<>%
  full_join(
    TA_Exp2 %>%
      distinct(Treatment, Mass) %>%
      filter(Treatment != "Blank") %>%
      droplevels() %>%
      summarise(across(Mass, list(mean = mean, sd = sd)),
                .by = Treatment) %>%
      mutate(Mass = list( rnorm( 8e4 , Mass_mean , Mass_sd ) )) %>%
      unnest(Mass) %>%
      mutate(.draw = 1:8e4, .by = Treatment) %>%
      select(-c(Mass_mean, Mass_sd))
  ) %>%
  full_join(
    CN %>% 
      filter(Experiment == 2) %>%
      select(C) %>%
      summarise(across(C, list(mean = mean, sd = sd))) %>%
      mutate(C = list( rnorm( 8e4 , C_mean , C_sd ) )) %>%
      unnest(C) %>%
      mutate(.draw = 1:8e4) %>%
      select(-c(C_mean, C_sd))
  ) %T>%
  print()
  
# Calculate equivalent carbon buffering and % sequestration
TA_surplus %<>%
  mutate(
    g_C = Mass * C / 100,
    mol_C = g_C / 12.011,
    mol_TA = TA_surplus * 1e-6 * 2 * 0.84, # M * 2 L = mol, eta = 0.84 (C/TA)
    Percentage = mol_TA / mol_C * 100
  ) %T>%
  print()

# Surplus TA
Fig_1e <- TA_surplus %>%
  add_row(Treatment = "Blank" %>% fct()) %>% # Add Blank with NAs
  ggplot() +
    geom_density(aes(TA_surplus / 1e3, fill = Treatment),
                 n = 2^10, bw = 90 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 90),
                       breaks = seq(0, 90, 30),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Surplus total alkalinity (mM)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(0, 0.5, 0, 0.2, unit = "cm"))

Fig_1e # warning expected due to introduced NAs

# Sequestered kelp
Fig_1f <- TA_surplus %>%
  add_row(Treatment = "Blank" %>% fct()) %>%
  ggplot() +
    geom_density(aes(Percentage, fill = Treatment),
                 n = 2^10, bw = 60 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 60),
                       breaks = seq(0, 60, 20),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Sequestered kelp carbon (%)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"))

Fig_1f


require(patchwork)
Fig_1 <- ( Fig_1a / Fig_1b / Fig_1c / Fig_1d / Fig_1e / Fig_1f ) +
          plot_layout(heights = c(1, rep(0.2, 5)))
Fig_1

Fig_1 %>%
  ggsave(filename = "Fig_1.pdf", path = "Figures",
         device = cairo_pdf, height = 18, width = 20, units = "cm")




# Concentration over time
Fig_2a <- NH4_prediction %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_point(data = NH4_Exp2,
               aes(Day, NH4_uM / 1e3, colour = Treatment), 
               shape = 16, size = 2.5, alpha = 0.7) +
    # geom_line(data = NH4_Exp2,
    #           aes(Day, NH4_uM / 1e3, group = Flask, colour = Treatment),
    #           alpha = 0.7) + 
    geom_line(aes(Day, NH4 / 1e3, colour = Treatment)) +
    geom_ribbon(aes(Day, ymin = NH4.lower / 1e3, 
                    ymax = NH4.upper / 1e3,
                    alpha = factor(.width),
                    fill = Treatment)) +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    scale_y_continuous(breaks = seq(0, 2.4, 0.6),
                       labels = scales::label_number(accuracy = c(1, rep(0.1, 4)))) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time since kelp addition (days)",
         y = "Ammonium (mM)") +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 2.4), 
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"))

Fig_2a

# NH4 production rate
Fig_2b <- NH4_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_density(aes(r, fill = Treatment),
                 n = 2^10, bw = 0.12 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 0.12),
                       breaks = seq(0, 0.12, 0.04),
                       labels = scales::label_number(accuracy = c(1, rep(0.01, 3))),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = expression("Ammonium production (day"^-1*")")) +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          axis.title.x = element_text(margin = margin(t = 0)), # counteract superscript
          plot.margin = margin(0.2, 0.5, 0, 0.2, unit = "cm"))

Fig_2b

# Time of maximal production rate
Fig_2c <- NH4_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  mutate(Treatment = Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) %>%
  ggplot() +
    stat_density_ridges(aes(m, y = 0, fill = Treatment),
                        from = 0, to = c(900, rep(240, 3)),
                        n = 2^10, bandwidth = c(900, rep(240, 3)) * 0.02, 
                        colour = NA) +
    scale_fill_manual(values = c("#6f5229", "#627d0e", "#7030a5", "#f1c700"),
                      guide = "none") +
    facet_wrap(~ Treatment, scales = "free", nrow = 1) +
    facetted_pos_scales(
      x = list(
        Treatment == "Blank" ~ scale_x_continuous(
          limits = c(0, 900),
          breaks = seq(0, 900, 300),
          oob = scales::oob_keep
        ),
        Treatment != "Blank" ~ scale_x_continuous(
          limits = c(0, 240),
          breaks = seq(0, 240, 60),
          oob = scales::oob_keep
        )
      )
    ) +
    labs(x = "Time of maximal production (days)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank(),
          plot.margin = margin(0.2, 0.5, 0, 0.2, unit = "cm"))

Fig_2c

# Produced NH4
Fig_2d <- NH4_prior_posterior %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_density(aes(NH4_prod / 1e3, fill = Treatment),
                 n = 2^10, bw = 3 * 0.02, colour = NA) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0, 3),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Produced ammonium (mM)") +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.text = element_blank())

Fig_2d

Fig_2 <- ( Fig_2a / Fig_2b / Fig_2c / Fig_2d ) +
  plot_layout(heights = c(1, rep(0.2, 3)))
Fig_2

18/2*1.6 # 14.4 cm height, round down because fewer titles/space

Fig_2 %>%
  ggsave(filename = "Fig_2.pdf", path = "Figures",
         device = cairo_pdf, height = 14, width = 20, units = "cm")


# Calculate DIC
require(seacarb)
TA %<>%
  mutate(
    # Convert nutrients from µg N or P to M
    NH4_M = NH4 / 14.007 * 1e-6,
    PO4_M = PO4 / 30.974 * 1e-6,
    # Infer H2S from TA
    H2S_M = TA * 0.5 * 1e-6, # based on 2CH2O + SO42− -> 2HCO3− + H2S
    # Calculate DIC three ways (TA must be given in M)
    DIC_naive = carb(
      flag = 8, var1 = pH, var2 = TA*1e-6, pHscale = "F", 
      S = 35, T = 18, Patm = 1, P = 0
    )$DIC*1e6, # convert DIC from M to µM
    DIC_nutri = if_else( # carbfull will not return NA if nutrients are NA
      !is.na(NH4) & !is.na(PO4),
      carbfull(
        flag = 8, var1 = pH, var2 = TA*1e-6, pHscale = "F", 
        S = 35, T = 18, Patm = 1, P = 0, Pt = PO4_M, NH4t = NH4_M
      )$DIC*1e6,
      NA
    ),
    DIC_sulf = if_else(
      !is.na(NH4) & !is.na(PO4) & !is.na(H2S_M),
      carbfull(
        flag = 8, var1 = pH, var2 = TA*1e-6, pHscale = "F", 
        S = 35, T = 18, Patm = 1, P = 0, Pt = PO4_M, 
        NH4t = NH4_M, HSt = H2S_M
      )$DIC*1e6,
      NA
    )
  ) %T>% 
  print()

DICTA_Exp2 <- TA %>%
  filter(Experiment == 2) %>%
  drop_na(TA) %>%
  mutate(Treatment = Treatment %>% fct(),
         Flask = Flask %>% as_factor()) %T>%
  print()

# Experiment 2 models
DICTA_Exp2_model <- here("Alkalinity", "Stan", "DICTA_Exp2.stan") %>% 
  read_file() %>%
  write_stan_file() %>%
  cmdstan_model()

DICTA_Exp2_naive_samples <- DICTA_Exp2_model$sample(
          data = DICTA_Exp2 %>%
            filter(Day != 8 | Treatment == "Blank") %>% # remove organic acid outliers
            select(DIC_naive, TA, Treatment, Flask) %>%
            rename(DIC = DIC_naive) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4,
          adapt_delta = 0.95
        ) %T>%
  print()

DICTA_Exp2_nutri_samples <- DICTA_Exp2_model$sample(
          data = DICTA_Exp2 %>%
            filter(Day != 8 | Treatment == "Blank") %>% # remove organic acid outliers
            select(DIC_nutri, TA, Treatment, Flask) %>%
            drop_na(DIC_nutri) %>% # drop rows where nutrients are NA
            rename(DIC = DIC_nutri) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4,
          adapt_delta = 0.95
        ) %T>%
  print()

DICTA_Exp2_sulf_samples <- DICTA_Exp2_model$sample(
          data = DICTA_Exp2 %>%
            filter(Day != 8 | Treatment == "Blank") %>% # remove organic acid outliers
            select(DIC_sulf, TA, Treatment, Flask) %>%
            drop_na(DIC_sulf) %>%
            rename(DIC = DIC_sulf) %>%
            compose_data(),
          chains = 8,
          parallel_chains = parallel::detectCores(),
          iter_warmup = 1e4,
          iter_sampling = 1e4,
          adapt_delta = 0.95
        ) %T>%
  print()


# Save draws
DICTA_Exp2_naive_samples$draws() %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_naive_samples.rds"))
DICTA_Exp2_naive_samples$draws(format = "df") %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_naive_samples_df.rds"))
DICTA_Exp2_nutri_samples$draws() %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_nutri_samples.rds"))
DICTA_Exp2_nutri_samples$draws(format = "df") %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_nutri_samples_df.rds"))
DICTA_Exp2_sulf_samples$draws() %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_sulf_samples.rds"))
DICTA_Exp2_sulf_samples$draws(format = "df") %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_sulf_samples_df.rds"))

# 2.1.3 Model checks ####
# Rhat
DICTA_Exp2_naive_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000614.

DICTA_Exp2_nutri_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000878.

DICTA_Exp2_sulf_samples$summary() %>%
  summarise(rhat_1.001 = mean( rhat > 1.001 ),
            rhat_mean = mean(rhat),
            rhat_sd = sd(rhat))
# No rhat above 1.001. rhat = 1.00 ± 0.0000629.

# Chains
DICTA_Exp2_naive_samples$draws(format = "df") %>%
  mcmc_rank_overlay()
DICTA_Exp2_nutri_samples$draws(format = "df") %>%
  mcmc_rank_overlay()
DICTA_Exp2_sulf_samples$draws(format = "df") %>%
  mcmc_rank_overlay()

# Pairs
DICTA_Exp2_naive_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_mu", "q_mu"))
DICTA_Exp2_nutri_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_mu", "q_mu"))
DICTA_Exp2_sulf_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_mu", "q_mu"))
# No correlation between intercept and slope

DICTA_Exp2_naive_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_T[1]", "A0_F[1]", "A0_T[2]", "A0_F[2]"))
DICTA_Exp2_naive_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("q_T[1]", "q_F[1]", "q_T[2]", "q_F[2]"))
DICTA_Exp2_nutri_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("q_T[1]", "q_F[1]", "q_T[2]", "q_F[2]"))
DICTA_Exp2_nutri_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_T[1]", "A0_F[1]", "A0_T[2]", "A0_F[2]"))
DICTA_Exp2_sulf_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("q_T[1]", "q_F[1]", "q_T[2]", "q_F[2]"))
DICTA_Exp2_sulf_samples$draws(format = "df") %>%
  mcmc_pairs(pars = c("A0_T[1]", "A0_F[1]", "A0_T[2]", "A0_F[2]"))
# Strong correlation between flasks and treatments, meaning they are hardly
# distinguishable. Nonetheless, I choose these as the optimal models.

# 2.1.4 Prior-posterior comparison ####
DICTA_Exp2_prior <- prior_samples(
  model = DICTA_Exp2_model,
  data = DICTA_Exp2 %>%
    select(DIC_naive, TA, Treatment, Flask) %>%
    rename(DIC = DIC_naive) %>%
    compose_data()
)

DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_naive_samples,
    group = DICTA_Exp2 %>% select(Treatment, Flask),
    parameters = c("A0_mu", "q_mu", "sigma", 
                   "A0_T[Treatment]", "A0_F[Flask]", 
                   "q_T[Treatment]", "q_F[Flask]", 
                   "A0_T_sigma", "A0_F_sigma",
                   "q_T_sigma", "q_F_sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(
    group_name = "Treatment",
    second_group_name = "Flask"
  )

DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_nutri_samples,
    group = DICTA_Exp2 %>% select(Treatment, Flask),
    parameters = c("A0_mu", "q_mu", "sigma", 
                   "A0_T[Treatment]", "A0_F[Flask]", 
                   "q_T[Treatment]", "q_F[Flask]", 
                   "A0_T_sigma", "A0_F_sigma",
                   "q_T_sigma", "q_F_sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(
    group_name = "Treatment",
    second_group_name = "Flask"
  )

DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_sulf_samples,
    group = DICTA_Exp2 %>% select(Treatment, Flask),
    parameters = c("A0_mu", "q_mu", "sigma", 
                   "A0_T[Treatment]", "A0_F[Flask]", 
                   "q_T[Treatment]", "q_F[Flask]", 
                   "A0_T_sigma", "A0_F_sigma",
                   "q_T_sigma", "q_F_sigma"),
    format = "long"
    ) %>%
  prior_posterior_plot(
    group_name = "Treatment",
    second_group_name = "Flask"
  )

# 2.1.5 Parameters ####
DICTA_Exp2_naive_prior_posterior <- DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_naive_samples,
    group = DICTA_Exp2 %>% select(Treatment),
    parameters = c("A0_T[Treatment]", "A0_F_sigma", 
                   "q_T[Treatment]", "q_F_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate( # Calculate parameters for new flasks
    A0 = rnorm( n() , A0_T , A0_F_sigma ),
    q = rnorm( n() , q_T , q_F_sigma )
  ) %>% # Embed prior in treatments
  filter(Treatment == "Control" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    Treatment = if_else(
      distribution == "prior", "Prior", Treatment
    ) %>% fct()
  ) %>%
  select(starts_with("."), Treatment, A0, q, sigma, q_F_sigma) %T>%
  print()

DICTA_Exp2_nutri_prior_posterior <- DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_nutri_samples,
    group = DICTA_Exp2 %>% select(Treatment),
    parameters = c("A0_T[Treatment]", "A0_F_sigma", 
                   "q_T[Treatment]", "q_F_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    A0 = rnorm( n() , A0_T , A0_F_sigma ),
    q = rnorm( n() , q_T , q_F_sigma )
  ) %>% 
  filter(Treatment == "Control" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    Treatment = if_else(
      distribution == "prior", "Prior", Treatment
    ) %>% fct()
  ) %>%
  select(starts_with("."), Treatment, A0, q, sigma, q_F_sigma) %T>%
  print()

DICTA_Exp2_sulf_prior_posterior <- DICTA_Exp2_prior %>% 
  prior_posterior_draws(
    posterior_samples = DICTA_Exp2_sulf_samples,
    group = DICTA_Exp2 %>% select(Treatment),
    parameters = c("A0_T[Treatment]", "A0_F_sigma", 
                   "q_T[Treatment]", "q_F_sigma", "sigma"),
    format = "short"
  ) %>% 
  mutate(
    A0 = rnorm( n() , A0_T , A0_F_sigma ),
    q = rnorm( n() , q_T , q_F_sigma )
  ) %>% 
  filter(Treatment == "Control" & distribution == "prior" |
           distribution == "posterior") %>%
  mutate(
    Treatment = if_else(
      distribution == "prior", "Prior", Treatment
    ) %>% fct()
  ) %>%
  select(starts_with("."), Treatment, A0, q, sigma, q_F_sigma) %T>%
  print()

# Save parameter distributions
DICTA_Exp2_naive_prior_posterior %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_naive_prior_posterior.rds"))
DICTA_Exp2_nutri_prior_posterior %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_nutri_prior_posterior.rds"))
DICTA_Exp2_sulf_prior_posterior %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_sulf_prior_posterior.rds"))

# 2.1.6 Prediction ####
DICTA_Exp2_naive_prediction <- DICTA_Exp2_naive_prior_posterior %>%
  spread_continuous(
    data = DICTA_Exp2 %>% 
      filter(Day != 8 | Treatment == "Blank"),
    predictor_name = "DIC_naive",
    group_name = "Treatment"
  ) %>%
  mutate( 
    mu = A0 + q * DIC_naive,
    TA = rnorm( n() , mu , sigma )
  ) %>%
  group_by(Treatment, DIC_naive) %>%
  median_qi(mu, TA, .width = c(.5, .8, .9)) %T>%
  print()

DICTA_Exp2_nutri_prediction <- DICTA_Exp2_nutri_prior_posterior %>%
  spread_continuous(
    data = DICTA_Exp2 %>% 
      filter(Day != 8 | Treatment == "Blank") %>% 
      drop_na(DIC_nutri),
    predictor_name = "DIC_nutri",
    group_name = "Treatment"
  ) %>%
  mutate( 
    mu = A0 + q * DIC_nutri,
    TA = rnorm( n() , mu , sigma )
  ) %>%
  group_by(Treatment, DIC_nutri) %>%
  median_qi(mu, TA, .width = c(.5, .8, .9)) %T>%
  print()

DICTA_Exp2_sulf_prediction <- DICTA_Exp2_sulf_prior_posterior %>%
  spread_continuous(
    data = DICTA_Exp2 %>% 
      filter(Day != 8 | Treatment == "Blank") %>%
      drop_na(DIC_sulf),
    predictor_name = "DIC_sulf",
    group_name = "Treatment"
  ) %>%
  mutate( 
    mu = A0 + q * DIC_sulf,
    TA = rnorm( n() , mu , sigma )
  ) %>%
  group_by(Treatment, DIC_sulf) %>%
  median_qi(mu, TA, .width = c(.5, .8, .9)) %T>%
  print()


DICTA_Exp2_naive_prediction %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_naive_prediction.rds"))
DICTA_Exp2_nutri_prediction %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_nutri_prediction.rds"))
DICTA_Exp2_sulf_prediction %>%
  write_rds(here("Alkalinity", "RDS", "DICTA_Exp2_sulf_prediction.rds"))

DICTA_Exp2 %<>%
  select(Treatment, Flask, Day, TA, starts_with("DIC")) %>%
  pivot_longer(cols = starts_with("DIC"),
               names_to = "Calculation",
               values_to = "DIC",
               names_prefix = "DIC_") %>%
  mutate(
    Calculation = Calculation %>% 
      fct_recode(Naive = "naive", Nutrients = "nutri", Bisulfide = "sulf") %>%
      fct_relevel("Naive", "Nutrients")
  ) %T>%
  print()

Fig_3a <- bind_rows(
  Naive = DICTA_Exp2_naive_prediction %>% rename(DIC = DIC_naive),
  Nutrients = DICTA_Exp2_nutri_prediction %>% rename(DIC = DIC_nutri),
  Bisulfide = DICTA_Exp2_sulf_prediction %>% rename(DIC = DIC_sulf),
  .id = "Calculation"
) %>%
  mutate(Calculation = Calculation %>% fct_relevel("Naive", "Nutrients")) %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    geom_textabline(slope = rep(1, 12), label = "1:1",
                    family = "Futura", size = 3.5, hjust = 1) +
    geom_point(data = DICTA_Exp2 %>% drop_na(DIC),
               aes(DIC / 1e3, TA / 1e3, colour = Treatment),
               shape = 16, size = 2.5, alpha = 0.7) +
    geom_path(data = DICTA_Exp2 %>% drop_na(DIC),
              aes(DIC / 1e3, TA / 1e3, 
                  colour = Treatment, group = Flask), 
              # arrow = arrow(length = unit(0.2, "cm"), 
              #               type = "closed", angle = 20),
              alpha = 0.7, lineend = "round", linejoin = "round") +
    geom_line(aes(DIC / 1e3, TA / 1e3, colour = Treatment)) +
    geom_ribbon(aes(DIC / 1e3, ymin = TA.lower / 1e3, 
                    ymax = TA.upper / 1e3,
                    alpha = factor(.width),
                    fill = Treatment)) +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_alpha_manual(values = c(0.5, 0.4, 0.3), guide = "none") +
    facet_grid(Calculation ~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate"),
                switch = "y") +
    labs(x = "Dissolved inorganic carbon (mM)",
         y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 80), ylim = c(0, 80),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          strip.text.y = element_text(face = "plain"),
          axis.title.y = element_text(margin = margin(r = -0.1, unit = "cm")))

Fig_3a

# q
Fig_3b <- bind_rows(
  Naive = DICTA_Exp2_naive_prior_posterior,
  Nutrients = DICTA_Exp2_nutri_prior_posterior,
  Bisulfide = DICTA_Exp2_sulf_prior_posterior,
  .id = "Calculation"
) %>%
  mutate(Calculation = Calculation %>% fct_relevel("Bisulfide", "Nutrients")) %>%
  filter(Treatment != "Prior") %>%
  ggplot() +
    stat_density_ridges(aes(q, y = Calculation, fill = Treatment),
                        from = 0.5, to = 2.5, n = 2^10, bandwidth = 2 * 0.02, 
                        scale = 1, colour = NA) +
    geom_vline(xintercept = 1) +
    scale_fill_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                      guide = "none") +
    scale_x_continuous(limits = c(0.5, 2.5),
                       labels = scales::label_number(accuracy = c(0.1, 1, 0.1, 1, 0.1)),
                       oob = scales::oob_keep) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = expression(italic("q")*" (TA/DIC)")) +
    coord_cartesian(expand = FALSE, clip = "off") +
    mytheme +
    theme(axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(size = 12, vjust = -0.2, hjust = 0,
                                     margin = margin(r = -0.8, unit = "cm")),
          strip.text = element_blank())

Fig_3b


Fig_3 <- ( Fig_3a / Fig_3b ) +
  plot_layout(heights = c(1, 0.2))
Fig_3

Fig_3 %>%
  ggsave(filename = "Fig_3.pdf", path = "Figures",
         device = cairo_pdf, height = 18.9, width = 20, units = "cm")






# Figure S1
O2 <- here("Oxygen", "Oxygen.csv") %>%
  read_csv() %>%
  mutate(Treatment = if_else(Channel == 1, "Control", "Sulfate")) %T>%
  print()

require(geomtextpath)
Fig_S1 <- O2 %>%
  ggplot(aes(delta_t/60, Value, colour = Treatment)) +
    geom_point(shape = 16, alpha = 0.2) +
    geom_textvline(data = . %>%
                     group_by(Treatment) %>%
                     filter(Value <= 0) %>%
                     slice(1) %>%
                     mutate(label = (delta_t/60) %>% signif(2) %>%
                              str_c("h", sep = " ")),
                   aes(xintercept = delta_t/60, colour = Treatment,
                       label = label),
                   family = "Futura", size = 3.5, hjust = 0.6,
                   show.legend = FALSE) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    labs(x = "Time since kelp addition (hours)", 
         y = "Oxygen (µM)") +
    scale_colour_manual(values = c("#627d0e", "#7030a5")) +
    scale_x_continuous(breaks = seq(0, 70, 10)) +
    scale_y_continuous(breaks = seq(0, 210, 70)) +
    coord_cartesian(xlim = c(0, 70), ylim = c(0, 210),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(legend.position = c(0.86, 0.92),
          plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm"))

Fig_S1

Fig_S1 %>%
  ggsave(filename = "Fig_S1.pdf", path = "Figures",
         device = cairo_pdf, height = 8, width = 16, units = "cm")


Fig_S2a <- TA %>%
  filter(Experiment == 1) %>%
  mutate(Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(Day, TA / 1e3, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    scale_y_continuous(breaks = seq(0, 60, 20)) +
    facet_grid(~ Treatment) +
    labs(x = "Time since kelp addition (days)",
         y = "TA (mM)") +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 60),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.4, margin = margin(l = -0.3, unit = "cm")))

Fig_S2a

Fig_S2b <- TA %>%
  filter(Experiment == 1) %>%
  mutate(Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(Day, pH, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    facet_grid(~ Treatment) +
    labs(x = "Time since kelp addition (days)",
         y = "pH") +
    coord_cartesian(xlim = c(0, 90), ylim = c(4, 8),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.4, margin = margin(l = -0.3, unit = "cm")))

Fig_S2b

Fig_S2c <- TA %>%
  filter(Experiment == 1 & !is.na(NH4)) %>%
  mutate(NH4_uM = NH4 / 14.007, # µg N in NH4 / 14.007 is µM N in NH4 = µM NH4
         Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(Day, NH4_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    scale_y_continuous(breaks = seq(0, 180, 60)) +
    facet_grid(~ Treatment) +
    labs(x = "Time since kelp addition (days)",
         y = expression("NH"[4]^"+"*" (µM)")) +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 180),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0, margin = margin(l = -0.3, unit = "cm")))

Fig_S2c

Fig_S2d <- TA %>%
  filter(Experiment == 1 & !is.na(NOx)) %>%
  mutate(NOx_uM = NOx / 14.007, # µg N in NOx / 14.007 is µM N in NOx = µM NOx
         Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(Day, NOx_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    facet_grid(~ Treatment) +
    labs(x = "Time since kelp addition (days)",
         y = expression("NO"[italic("x")]*" (µM)")) +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 20),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.07, margin = margin(l = -0.3, unit = "cm")))

Fig_S2d

Fig_S2e <- TA %>%
  filter(Experiment == 1 & !is.na(PO4)) %>%
  mutate(PO4_uM = PO4 / 30.974, # µg P in PO4 / 30.974 is µM P in PO4 = µM PO4
         Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(Day, PO4_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    scale_y_continuous(breaks = seq(0, 450, 150)) +
    facet_grid(~ Treatment) +
    labs(x = "Time since kelp addition (days)",
         y = expression("PO"[4]^"3−"*" (µM)")) +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 450),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.03, margin = margin(l = -0.3, unit = "cm")))

Fig_S2e

Fig_S2 <- ( ( Fig_S2a + 
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank()) ) / 
              ( Fig_S2b + 
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        strip.text = element_blank(),
                        plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) /
              ( Fig_S2c + 
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        strip.text = element_blank(),
                        plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) /
              ( Fig_S2d + 
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        strip.text = element_blank(),
                        plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) /
              Fig_S2e + 
                theme(strip.text = element_blank(),
                      plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) # + 
  # plot_annotation(tag_levels = "a") &
  # theme(plot.tag = element_text(family = "Futura", size = 15, face = "bold"),
  #       plot.tag.position = c(0.008, 1.01))

Fig_S2

Fig_S2 %>%
  ggsave(filename = "Fig_S2.pdf", path = "Figures",
         device = cairo_pdf, height = 20, width = 20, units = "cm")




Fig_S3a <- TA %>%
  filter(Experiment == 1 & !is.na(DIC_naive)) %>%
  mutate(Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(DIC_naive / 1e3, TA / 1e3, colour = Treatment)) +
    geom_textabline(slope = rep(1, 3), label = "1:1", 
                    family = "Futura", size = 3.5, hjust = 1) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_path(aes(group = Flask), alpha = 0.7,
              lineend = "round", linejoin = "round",
              arrow = arrow(length = unit(0.2, "cm"), 
                            type = "closed", angle = 20)) +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    scale_y_continuous(breaks = seq(0, 90, 30)) +
    facet_grid(~ Treatment) +
    labs(x = "Dissolved inorganic carbon (mM)",
         y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 90),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"))

Fig_S3a

Fig_S3b <- TA %>%
  filter(Experiment == 1 & !is.na(DIC_nutri)) %>%
  mutate(Treatment = Treatment %>% fct_relevel("Control", "Sulfate")) %>%
  ggplot(aes(DIC_nutri / 1e3, TA / 1e3, colour = Treatment)) +
    geom_textabline(slope = rep(1, 3), label = "1:1", 
                    family = "Futura", size = 3.5, hjust = 1) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_path(aes(group = Flask), alpha = 0.7,
              lineend = "round", linejoin = "round",
              arrow = arrow(length = unit(0.2, "cm"), 
                            type = "closed", angle = 20)) +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#b3061e"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 90, 30)) +
    scale_y_continuous(breaks = seq(0, 90, 30)) +
    facet_grid(~ Treatment) +
    labs(x = "Dissolved inorganic carbon (mM)",
         y = "Total alkalinity (mM)") +
    coord_cartesian(xlim = c(0, 90), ylim = c(0, 90),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"))

Fig_S3b

Fig_S3 <- ( ( Fig_S3a + 
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank()) ) / 
              Fig_S3b + 
                theme(strip.text = element_blank(),
                      plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) + 
plot_annotation(tag_levels = "a") &
theme(plot.tag = element_text(family = "Futura", size = 15, face = "bold"),
      plot.tag.position = c(0.008, 1.01))

Fig_S3

Fig_S3 %>%
  ggsave(filename = "Fig_S3.pdf", path = "Figures",
         device = cairo_pdf, height = 13.8, width = 20, units = "cm")



Fig_S4a <- TA_Exp2 %>%
  ggplot(aes(Day, pH, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time since kelp addition (days)",
         y = "pH") +
    coord_cartesian(xlim = c(0, 240), ylim = c(4, 8),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.4, margin = margin(l = -0.3, unit = "cm")))

Fig_S4a

Fig_S4b <- NH4_Exp2 %>%
  mutate(NOx_uM = NOx / 14.007) %>%
  ggplot(aes(Day, NOx_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time since kelp addition (days)",
         y = expression("NO"[italic("x")]*" (µM)")) +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 150),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.07, margin = margin(l = -0.3, unit = "cm")))

Fig_S4b

Fig_S4c <- NH4_Exp2 %>%
  mutate(PO4_uM = PO4 / 30.974) %>%
  ggplot(aes(Day, PO4_uM, colour = Treatment)) +
    geom_point(shape = 16, size = 2.5, alpha = 0.7) +
    geom_line(aes(group = Flask), alpha = 0.7, 
              lineend = "round", linejoin = "round") +
    scale_colour_manual(values = c("#627d0e", "#7030a5", "#f1c700", "#6f5229"),
                        guide = "none") +
    scale_x_continuous(breaks = seq(0, 240, 60)) +
    scale_y_continuous(breaks = seq(0, 90, 30)) +
    facet_grid(~ Treatment %>% fct_relevel("Blank", "Control", "Sulfate")) +
    labs(x = "Time since kelp addition (days)",
         y = expression("PO"[4]^"3−"*" (µM)")) +
    coord_cartesian(xlim = c(0, 240), ylim = c(0, 90),
                    expand = FALSE, clip = "off") +
    mytheme +
    theme(plot.margin = margin(0, 0.5, 0.2, 0.2, unit = "cm"),
          axis.title.y = element_text(vjust = 0.03, margin = margin(l = -0.3, unit = "cm")))

Fig_S4c

Fig_S4 <- ( ( Fig_S4a + 
                theme(axis.title.x = element_blank(),
                      axis.text.x = element_blank()) ) / 
              ( Fig_S4b + 
                  theme(axis.title.x = element_blank(),
                        axis.text.x = element_blank(),
                        strip.text = element_blank(),
                        plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) ) /
              Fig_S4c + 
                theme(strip.text = element_blank(),
                      plot.margin = margin(0.5, 0.5, 0.2, 0.2, unit = "cm")) )

Fig_S4

Fig_S4 %>%
  ggsave(filename = "Fig_S4.pdf", path = "Figures",
         device = cairo_pdf, height = 12.7, width = 20, units = "cm")

# Tables
require(glue)
Table_1 <- CN %>%
  mutate(C_N = C / 12.011 / (N / 14.007)) %>% # molar ratio
  summarise(
    across(
      c(C, N, C_N, d13C, d15N),
      list(mean = mean, sd = sd)
    ),
    n = n(),
    .by = c(Experiment, Location)
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~signif(.x, 2)
    ),
    C = glue("{C_mean} ± {C_sd}"),
    N = glue("{N_mean} ± {N_sd}"),
    C_N = glue("{C_N_mean} ± {C_N_sd}"),
    d13C = glue("{d13C_mean} ± {d13C_sd}"),
    d15N = glue("{d15N_mean} ± {d15N_sd}")
  ) %>%
  select(!c(ends_with("mean"), ends_with("sd"))) %T>%
  print()


Table_1 %>%
  write_csv(here("Tables", "Table_1.csv"))

require(officer)
read_docx() %>%
  body_add_table(value = Table_1) %>%
  print(target = here("Tables", "Table_1.docx"))



Table_2 <- TA_prior_posterior %>%
  filter(!Treatment %in% c("Prior", "Blank")) %>%
  droplevels() %>%
  full_join(TA_surplus) %>%
  mutate(TA_prod = TA_prod * 1e-3, # convert µM to mM
         TA_surplus = TA_surplus * 1e-3) %>%
  summarise(
    across(
      c(TA_prod, TA_surplus, g_C, mol_C, Percentage),
      list(mean = mean, sd = sd)
    ),
    n = n(),
    .by = Treatment
  ) %>%
  mutate(
    across(where(is.numeric), ~signif(.x, 2)),
    g_C = glue("{g_C_mean} ± {g_C_sd}"),
    # mol_C = glue("{mol_C_mean} ± {mol_C_sd}"),
    TA_prod = glue("{TA_prod_mean} ± {TA_prod_sd}"),
    TA_surplus = glue("{TA_surplus_mean} ± {TA_surplus_sd}"),
    Percentage = glue("{Percentage_mean} ± {Percentage_sd}")
  ) %>%
  select(-c(ends_with("mean"), ends_with("sd"))) %T>%
  print()



Table_2 %>%
  write_csv(here("Tables", "Table_2.csv"))

read_docx() %>%
  body_add_table(value = Table_2) %>%
  print(target = here("Tables", "Table_2.docx"))

TA %>%
  drop_na(Day) %>%
  filter(Experiment == 3 & Day == max(Day),
         .by = Experiment) %>%
  summarise(
    TA_mean = mean(TA/1e3),
    TA_sd = sd(TA/1e3),
    n = n(),
    .by = Treatment
  ) %>%
  mutate(
    across(where(is.numeric), ~signif(.x, 2)),
    TA = glue("{TA_mean} ± {TA_sd}")
  ) %>%
  select(!c(ends_with("mean"), ends_with("sd")))




############



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
