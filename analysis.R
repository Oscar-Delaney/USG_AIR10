library(tidyverse)
library(jimbilben) # devtools::install_github("Jimbilben/jimbilben")
library(brms)
library(tidybayes)
library(glue)
library(tidystats)
library(rmetalog)
library(ggtext)
library(extraDistr)

theme_set(theme_jimbilben(10))
set_colors()

data <-
  read_csv("data/main_data.csv")

data <-
  data %>% 
  mutate(beta_main = transform_beta(main, sample_size = nrow(data)))

# Create a mapping of question codes to descriptive names
question_names <- c(
  "q0" = "Overall",
  "q1" = "Consortium", 
  "q2" = "Government lab",
  "q3" = "Nationalization",
  "q4" = "Private contractor",
  "q5" = "Legal compulsion",
  "q6" = "Military"
)

# Create a vector to specify the desired factor level order
question_order <- question_names[paste0("q", 0:6)]

# Apply the mapping and set as factor with specified order
data <- data %>%
  mutate(question = recode(question, !!!question_names)) %>%
  mutate(question = factor(question, levels = question_order))

# Calculate the expected sum for each participant
participant_checks <- data %>%
  # For each usercode, calculate what "Overall" should approximately equal
  group_by(usercode, type) %>%
  summarize(
    overall_estimate = main[question == "Overall"],
    sum_of_components = sum(main[question %in% c("Consortium", "Government lab", "Nationalization", 
                                                 "Private contractor", "Legal compulsion")]),
    difference = overall_estimate - sum_of_components,
    percent_difference = (difference / overall_estimate) * 100,
    .groups = "drop"
  ) %>%
  # Sort by the absolute percentage difference to see who was closest
  arrange(abs(percent_difference))

#### basic plot ####
# you can put totally custom colors in here
# e.g., from https://htmlcolorcodes.com/
# or you can use some of my presets from set_colors()
my_fills <-
  c("expert" = teal,
    "forecaster" = fire_red)

my_colors <-
  c("all" = "black",
    "expert" = teal,
    "forecaster" = fire_red)

# arrange participants by their overall estimate
my_order <-
  data %>% 
  filter(question == "Overall") %>%  # Only look at the Overall question
  arrange(type, main) %>%            # Sort first by type, then by main estimate
  pull(usercode)                     # Extract just the usercode column

data <-
  data %>% 
  mutate(usercode = factor(usercode,
                           levels = my_order))

# !!! NOTE - if you only want to look at the plot directly, don't run j_png, just run the plot part 
# j_png is for actually making a png image that will save for you in the png folder
j_png("iaps - raw estimates plot example",
      height = 7.5)
data %>% 
  ggplot(aes(x = main, y = usercode, color = type)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = low, xmax = high), linewidth = .33, height = .5) +
  geom_point(shape = 16, fill = "white", size = 1.5) +
  scale_color_manual(values = my_colors) +
  facet_wrap(~question, ncol = 2) + # the facet names are just q0 etc. now, but you can add a column to the data with an appropriate name and facet_wrap using that column instead
  labs(
    title = "Raw estimates for probabilities of each outcome",
    x = "Estimated probability and 90% range",
    y = "",
    color = "Respondent type:"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# you can also organise it by rater:
j_png("iaps - raw estimates plot example flipped",
      height = 7.5)
data %>% 
  ggplot(aes(x = main, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = low, xmax = high), linewidth = .33, height = .5) +
  geom_point(shape = 16, fill = "white", size = 1.5) +
  scale_color_manual(values = my_colors) +
  facet_wrap(~usercode, ncol = 3) + # the facet names are just q0 etc. now, but you can add a column to the data with an appropriate name and facet_wrap using that column instead
  labs(
    title = "Raw estimates for probabilities of each outcome",
    x = "Estimated probability and 90% range",
    y = "",
    color = "Respondent type:"
  ) +
  theme(
    legend.position = "top",
    panel.spacing.x = unit(1, "lines")
  )
dev.off()

#### metalog it ####
make_metalog <- function(input_tibble, n_samples = 25000) {
  
  metalog_object <-
    metalog(x = c(input_tibble$low, input_tibble$main, input_tibble$high),
            probs = c(.05, .5, .95),
            term_limit = 3,
            boundedness = "b",
            bounds = c(-.001, 100.001))
  
  samples <-
    rmetalog(metalog_object,
             n = n_samples,
             term = 3)
  
  output <-
    tibble(
      sample = 1:n_samples,
      value = samples,
      question = input_tibble$question,
      usercode = input_tibble$usercode,
      low = input_tibble$low,
      main = input_tibble$main,
      high = input_tibble$high,
      type = input_tibble$type
    ) %>% 
    mutate(
      value = case_when(value < 0 ~ 0,
                        value > 100 ~ 100,
                        TRUE ~ value)
    )
  
  return(output)
  
}

data_samples <-
  data %>% 
  group_split(question, usercode) %>% 
  map_df(.f = make_metalog,
         n_samples = 25000)

##### get summary information from distributions #####
# we can now just describe/summarise the resultant pooled distributions
data_samples_grouped_summary <-
  data_samples %>% 
  group_by(question, type) %>% 
  summarise(
    median = median(value),
    mean = mean(value),
    highest_point = hdp(value),
    quant_05 = quantile(value, .05),
    quant_95 = quantile(value, .95),
  )

data_samples_summary <-
  data_samples %>% 
  group_by(question) %>% 
  summarise(
    median = median(value),
    mean = mean(value),
    highest_point = hdp(value),
    quant_05 = quantile(value, .05),
    quant_95 = quantile(value, .95),
  )

data_samples_combined_summary <- bind_rows(
  data_samples_grouped_summary,
  data_samples_summary %>%
    mutate(type = "all")
)

j_png("iaps - metalog plot example no breakdown v2",
      height = 7.5)
data_samples %>% 
  ggplot(aes(x = value)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  geom_histogram(breaks = seq(min(0),
                              max(100),
                              length.out = 49 + 1),
                 linewidth = .25,
                 alpha = .5,
                 position = position_identity(),
                 color = NA,
                 fill = sky_blue) +
  
  geom_errorbarh(data = data_samples_summary, aes(xmin = quant_05, xmax = quant_95, y = 0, x = as.numeric(NA)), linewidth = .33, height = .5) +
  geom_point(data = data_samples_summary, aes(x = median, y = 0), shape = 16, fill = "white", size = 1.5) +
  
  facet_wrap(~question, ncol = 2, scales = "free_y") +
  labs(
    title = "Pooled probabilities based on metalog distributions for each respondent's estimate",
    x = "Estimated probability",
    y = ""
  ) +
  theme(
    axis.text.y = element_blank()
  )
dev.off()

j_png("iaps - metalog plot example v2",
      height = 7.5)
data_samples_grouped_summary %>% 
  ggplot(aes(x = median, color = type)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  
  geom_histogram(data = data_samples,
                 aes(fill = type, x = value),
                 breaks = seq(min(0),
                              max(100),
                              length.out = 49 + 1),
                 linewidth = .25,
                 alpha = .35,
                 position = position_identity(),
                 color = NA) +
  
  geom_errorbarh(aes(xmin = quant_05, xmax = quant_95, y = 0), linewidth = .4, height = .5, position = position_nudge(y = c(0, 500, 0, 1200, 0, 2400, 0, 2400, 0, 1200, 0, 3000, 0, 300)), show.legend = FALSE) +
  geom_point(aes(y = 0), shape = 16, fill = "white", size = 1.5, position = position_nudge(y = c(0, 500, 0, 1200, 0, 2400, 0, 2400, 0, 1200, 0, 3000, 0, 300)), show.legend = FALSE) +
  
  
  scale_fill_manual(values = my_fills) +
  scale_color_manual(values = my_colors) +
  facet_wrap(~question, ncol = 2, scales = "free_y") +
  labs(
    title = "Pooled probabilities based on metalog distributions for each respondent's estimate",
    x = "Estimated probability",
    fill = "Respondent type:",
    y = ""
  ) +
  theme(
    axis.text.y = element_blank(),
    legend.position = "top"
  )
dev.off()

# Create labels (using quantiles)
metalog_summary_labels_quant <-
  data_samples_combined_summary %>%
  mutate(label = glue::glue("**{nice_num(median, 0, FALSE)}%** [{nice_num(quant_05, 1, FALSE)}; {nice_num(quant_95, 1, FALSE)}]")) %>%
  select(question, type, label)

data_samples_combined_summary_quant <-
  data_samples_combined_summary %>%
  left_join(metalog_summary_labels_quant, by = c("question", "type")) %>%
  mutate(type = factor(type, levels = c("forecaster", "expert", "all")))


j_png("iaps - metalog plot example v4",
      height = 5)

data_samples_combined_summary_quant %>%
  filter(question != "Military") %>%  # Remove Military question
  ggplot(aes(x = median, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), expand = expansion(add = c(1,1))) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = quant_05, xmax = quant_95), position = position_dodge(.8), height =.25) +  # Use quant_05 and quant_95
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), fill = "grey99", color = "grey98", linewidth =.1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = label, alpha = type), fill = NA, text.color = "black", color = NA, position = position_dodge(.8), size = 2.4, family = "Jost", show.legend = FALSE) +  # Use label (quantiles)
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Estimates from Metalog Distributions (Quantiles)",  # Updated title
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

#### beta regression modelling ####
# I've added an interaction between question and respondent type for the mean,
# but not for the phi parameter: there just isn't really enough data to get any
# level of estimation for the spread (phi) of the data from so few people per group,
# so for the spread of the data I am proceeding here with treating the spread as 
# equivalent for experts and for forecasters
beta_form_1 <-
  bf(beta_main ~ 0 + question:type + (1 | usercode),
     phi ~ 0 + question)

get_prior(formula = beta_form_1,
          family = Beta,
          data = data)

beta_prior_1 <-
  c(set_prior("normal(0 , 1.33)", class = "b"), # this will convert to a ~flat prior on the probability tapering off at 0 and 1
    set_prior("normal(0, 1.5)", ub = 6, class = "b", dpar = "phi"),
    set_prior("exponential(3)", class = "sd"))

beta_fit_1 <-
  brm(formula = beta_form_1,
      family = Beta(),
      data = data,
      control = list(adapt_delta = 0.99, max_treedepth = 15),
      prior = beta_prior_1,
      chains = 4,
      cores = 4,
      iter = 5000,
      warmup = 1000,
      init = 0,
      # backend = 'cmdstanr',
      # threads = threading(4),
      # #seed = 1111,
      # silent = 0,
      # refresh = 100,
      # stan_model_args = list(stanc_options = list('O1'))
  )

##### extract key info for plotting #####
beta_epred_1 <-
  add_epred_draws(newdata = crossing(question = question_order,
                                     type = c("expert", "forecaster")),
                  re_formula = NA,
                  object = beta_fit_1,
                  value = "probability") %>% 
  ungroup() %>% 
  select(-c(.row, .chain, .iteration)) %>% 
  rename(draw = .draw)

beta_phi_parameters <-
  beta_fit_1 %>% 
  as_draws_df() %>%
  as_tibble() %>% 
  select(contains("phi"), .draw) %>% 
  rename(draw = .draw) %>% 
  pivot_longer(cols = contains("phi"),
               names_to = "question",
               values_to = "log_phi") %>% 
  mutate(question = case_when(
                  str_detect(question, "Overall") ~ "Overall",
                  str_detect(question, "Consortium") ~ "Consortium",
                  str_detect(question, "Governmentlab") ~ "Government lab",
                  str_detect(question, "Nationalization") ~ "Nationalization",
                  str_detect(question, "Privatecontractor") ~ "Private contractor",
                  str_detect(question, "Legalcompulsion") ~ "Legal compulsion",
                  str_detect(question, "Military") ~ "Military"),
         # Make sure question is a factor with correct levels
         phi = exp(log_phi)
         )

beta_epred_1 <-
  beta_epred_1 %>% 
  left_join(
    beta_phi_parameters,
    by = c("question", "draw")
  ) %>%
  mutate(question = factor(question, levels = question_order))

beta_summary_1 <-
  beta_epred_1 %>% 
  pivot_longer(cols = c(probability, phi),
               names_to = "parameter",
               values_to = "estimate") %>% 
  group_by(question, type, parameter) %>% 
  summarise(
    mean = mean(estimate),
    median = median(estimate),
    mode = hdp(estimate),
    lower_quant_90 = quantile(estimate, .05),
    upper_quant_90 = quantile(estimate, .95)
  ) %>% 
  mutate(perc_mean = case_when(parameter == "probability" ~ mean * 100),
         perc_median = case_when(parameter == "probability" ~ median * 100),
         perc_mode = case_when(parameter == "probability" ~ mode * 100),
         perc_lower_quant_90 = case_when(parameter == "probability" ~ lower_quant_90 * 100),
         perc_upper_quant_90 = case_when(parameter == "probability" ~ upper_quant_90 * 100),
         ) %>% 
  mutate(label = case_when(parameter == "probability" ~ glue::glue("**{nice_num(perc_median, 0, FALSE)}%** [{nice_num(perc_lower_quant_90, 1, FALSE)}; {nice_num(perc_upper_quant_90, 1, FALSE)}]"),
                           TRUE ~ glue::glue("**{nice_num(median, 1)}** [{nice_num(lower_quant_90, 1)}; {nice_num(upper_quant_90, 1)}]"))
         )

beta_summary_1_v2 <-
  beta_summary_1 %>% 
  select(question, type, parameter, label) %>% 
  pivot_wider(names_from = parameter,
              values_from = label) %>% 
  mutate(full_label = glue::glue("{probability}, *phi:* {phi}"))

beta_summary_1 <-
  beta_summary_1 %>% 
  left_join(
    beta_summary_1_v2 %>% 
      select(question, type, full_label),
    by = c("question", "type")
  )
  
j_png("iaps - beta regression example",
      height = 5)
beta_summary_1 %>%
  filter(parameter == "probability") %>% 
  ggplot(aes(x = median * 100, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = lower_quant_90 * 100, xmax = upper_quant_90 * 100), position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = full_label, alpha = type), fill = NA, text.color = "black", color = NA, position = position_dodge(.8), size = 2.4, family = "Jost", show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1)) + # alpha here is just used so that we can dodge the text above, otherwise it will overlap
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Estimates from beta regression model",
    color = "Respondent type:"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()
