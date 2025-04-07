library(tidyverse)
library(jimbilben) # devtools::install_github("Jimbilben/jimbilben")
library(brms)
library(tidybayes)
library(glue)
library(tidystats)
library(rmetalog)
library(ggtext)
library(extraDistr)
library(betareg)

# set the theme and colors
theme_set(theme_jimbilben(10))
set_colors()

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

# Load and process the data
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

# !!! NOTE - if you only want to look at the plot directly, don't run j_png, just run the plot part 
# j_png is for actually making a png image that will save for you in the png folder
j_png("raw data by question",
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
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# you can also organise it by rater:
j_png("raw data by user",
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
    color = "Respondent type"
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

j_png("metalog distributions",
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

j_png("metalog distributions by type",
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
    fill = "Respondent type",
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


j_png("metalog summary plot",
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
    title = "Metalog distributions group estimates",
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

# Compute summary statistics separately for each type
beta_summary_1_grouped <- beta_epred_1 %>% 
  pivot_longer(cols = c(probability, phi),
               names_to = "parameter",
               values_to = "estimate") %>% 
  group_by(question, type, parameter) %>% 
  summarise(
    mean = mean(estimate),
    median = median(estimate),
    mode = hdp(estimate),
    lower_quant_90 = quantile(estimate, .05),
    upper_quant_90 = quantile(estimate, .95),
    .groups = "drop"
  )

# Compute overall summary statistics (ignoring type)
beta_summary_1_all <- beta_epred_1 %>% 
  pivot_longer(cols = c(probability, phi),
               names_to = "parameter",
               values_to = "estimate") %>% 
  group_by(question, parameter) %>%  # No 'type' grouping here
  summarise(
    type = "all",
    mean = mean(estimate),
    median = median(estimate),
    mode = hdp(estimate),
    lower_quant_90 = quantile(estimate, .05),
    upper_quant_90 = quantile(estimate, .95),
    .groups = "drop"
  )

# Combine both data frames
beta_summary_1 <- bind_rows(beta_summary_1_grouped, beta_summary_1_all) %>%
  mutate(perc_mean = case_when(parameter == "probability" ~ mean * 100),
         perc_median = case_when(parameter == "probability" ~ median * 100),
         perc_mode = case_when(parameter == "probability" ~ mode * 100),
         perc_lower_quant_90 = case_when(parameter == "probability" ~ lower_quant_90 * 100),
         perc_upper_quant_90 = case_when(parameter == "probability" ~ upper_quant_90 * 100)
  ) %>% 
  mutate(label = case_when(parameter == "probability" ~ glue::glue("**{nice_num(perc_median, 0, FALSE)}%** [{nice_num(perc_lower_quant_90, 1, FALSE)}; {nice_num(perc_upper_quant_90, 1, FALSE)}]"),
                           TRUE ~ glue::glue("**{nice_num(median, 1)}** [{nice_num(lower_quant_90, 1)}; {nice_num(upper_quant_90, 1)}]"))
  ) %>%
  mutate(type = factor(type, levels = c("forecaster", "expert", "all")))  # Ensure correct order for plotting

# Create labels for full summary display
beta_summary_1_v2 <- beta_summary_1 %>% 
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

# Generate the plot
j_png("beta regression summary plot",
      height = 5)
beta_summary_1 %>%
  filter(parameter == "probability", question != "Military") %>% 
  ggplot(aes(x = median * 100, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = lower_quant_90 * 100, xmax = upper_quant_90 * 100), position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = full_label, alpha = type), fill = NA, text.color = "black", color = NA, position = position_dodge(.8), size = 2.4, family = "Jost", show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1, 1)) + # Ensure 'all' category is properly dodged
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Estimates from beta regression model",
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()


##### Add new section for predicted draws #####
# This section generates predictions for what a new forecaster/expert would estimate
# (rather than just the uncertainty in the mean)

# Generate predicted draws (full predictive distribution including the phi parameter)
beta_pred_1 <-
  add_predicted_draws(newdata = crossing(question = question_order,
                                        type = c("expert", "forecaster")),
                     re_formula = NA,  # Exclude random effects
                     object = beta_fit_1,
                     value = "probability") %>% 
  ungroup() %>% 
  select(-c(.row, .chain, .iteration)) %>% 
  rename(draw = .draw)

# Compute summary statistics separately for each type
beta_pred_summary_grouped <- beta_pred_1 %>% 
  group_by(question, type) %>% 
  summarise(
    mean = mean(probability),
    median = median(probability),
    mode = hdp(probability),
    lower_quant_90 = quantile(probability, .05),
    upper_quant_90 = quantile(probability, .95),
    .groups = "drop"
  )

# Compute overall summary statistics (ignoring type)
beta_pred_summary_all <- beta_pred_1 %>% 
  group_by(question) %>%
  summarise(
    type = "all",
    mean = mean(probability),
    median = median(probability),
    mode = hdp(probability),
    lower_quant_90 = quantile(probability, .05),
    upper_quant_90 = quantile(probability, .95),
    .groups = "drop"
  )

# Combine both data frames
beta_pred_summary <- bind_rows(beta_pred_summary_grouped, beta_pred_summary_all) %>%
  mutate(
    perc_mean = mean * 100,
    perc_median = median * 100,
    perc_mode = mode * 100,
    perc_lower_quant_90 = lower_quant_90 * 100,
    perc_upper_quant_90 = upper_quant_90 * 100,
    label = glue::glue("**{nice_num(perc_median, 0, FALSE)}%** [{nice_num(perc_lower_quant_90, 1, FALSE)}; {nice_num(perc_upper_quant_90, 1, FALSE)}]")
  ) %>%
  mutate(type = factor(type, levels = c("forecaster", "expert", "all")),  # Ensure correct order for plotting
    question = factor(question, levels = question_order))  # Ensure correct question ordering

  
# Generate the plot for predicted distribution
j_png("beta regression predicted distribution plot",
      height = 5)
beta_pred_summary %>%
  filter(question != "Military") %>% 
  ggplot(aes(x = perc_median, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), 
                    labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), 
                    expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = perc_lower_quant_90, xmax = perc_upper_quant_90), 
                position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), 
           fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = label, alpha = type), 
               fill = NA, text.color = "black", color = NA, 
               position = position_dodge(.8), size = 2.4, family = "Jost", 
               show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Predicted distribution of individual forecasts from beta regression model",
    subtitle = "Showing what a new forecaster/expert's estimate would likely be (full predictive distribution)",
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# Compare with EPRED results (posterior over the mean)
j_png("beta regression comparison plot",
      height = 7)

# Combine both datasets for comparison
comparison_data <- bind_rows(
  beta_summary_1 %>% 
    filter(parameter == "probability") %>%
    mutate(distribution_type = "Posterior distribution of the mean") %>%
    select(question, type, perc_median, perc_lower_quant_90, perc_upper_quant_90, distribution_type),
  
  beta_pred_summary %>%
    mutate(distribution_type = "Full predictive distribution") %>%
    select(question, type, perc_median, perc_lower_quant_90, perc_upper_quant_90, distribution_type)
) %>%
  filter(question != "Military")

# Add raw data points for comparison
raw_data_points <- data %>%
  filter(question != "Military") %>%
  mutate(
    perc_median = main,  # Already in percentage format
    distribution_type = "Raw individual estimates"
  ) %>%
  select(question, type, usercode, perc_median, distribution_type)

# Create the comparison plot
comparison_data %>%
  ggplot(aes(x = perc_median, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  scale_y_discrete(limits = rev) +
  # Add raw data points
  geom_point(data = raw_data_points, 
             aes(shape = "Individual estimates"), 
             position = position_jitter(height = 0.2), 
             alpha = 0.7, size = 2) +
  # Add intervals for both distribution types
  geom_errorbarh(aes(xmin = perc_lower_quant_90, xmax = perc_upper_quant_90, 
                    linetype = distribution_type),
                position = position_dodge(width = 0.8), height = 0.2) +
  geom_point(aes(shape = distribution_type), 
            position = position_dodge(width = 0.8)) +
  scale_color_manual(values = my_colors) +
  scale_shape_manual(values = c("Posterior distribution of the mean" = 16, 
                              "Full predictive distribution" = 17,
                              "Individual estimates" = 1)) +
  labs(
    x = "Estimated probability (%)",
    y = "",
    title = "Comparison of different distribution types from beta regression",
    subtitle = "Showing both the uncertainty in the mean and the full predictive distribution",
    color = "Respondent type",
    shape = "Distribution type",
    linetype = "Distribution type"
  ) +
  theme(
    legend.position = "right"
  ) +
  facet_wrap(~ distribution_type, ncol = 1)
dev.off()

##### Frequentist Beta Regression Analysis #####
# This implements a standard frequentist beta regression model
# without Bayesian priors that might skew the results

# Prepare data for frequentist beta regression
# The response variable must be strictly between 0 and 1 (not inclusive)
freq_data <- data %>%
  mutate(
    # Apply a small adjustment to ensure values are strictly between 0 and 1
    beta_response = ifelse(beta_main == 0, 0.002, 
                   ifelse(beta_main == 1, 0.998, beta_main))
  )

# Create dummy variables for question:type interactions
# This replicates the model structure from the Bayesian version
freq_data <- freq_data %>%
  mutate(
    question_type = paste(question, type, sep = "_")
  )

# Fit frequentist beta regression model
# Model has the same structure as the Bayesian model (question:type interaction for mean, question for precision)
freq_model <- betareg(
  beta_response ~ 0 + question_type | question, # 0 + removes intercept, | separates mean model from precision model
  data = freq_data
)

# Display model summary
summary_freq_model <- summary(freq_model)
print(summary_freq_model)

# Function to generate predictions for a new observation
predict_beta <- function(model, newdata) {
  # Get predictions for mean
  pred_mean <- predict(model, newdata = newdata, type = "response")
  
  # Get precision parameters (phi)
  phi_coeffs <- coef(model, model = "precision")
  
  # Extract question from newdata and match to phi parameter
  questions <- newdata$question
  phis <- numeric(length(questions))
  
  for (i in 1:length(questions)) {
    q <- as.character(questions[i])
    # Find the corresponding phi parameter
    phi_name <- paste0("(phi)_", q)
    # If exact match not found, use intercept
    if (phi_name %in% names(phi_coeffs)) {
      phis[i] <- exp(phi_coeffs[phi_name]) # Phi is on log scale in betareg
    } else {
      phis[i] <- exp(phi_coeffs["(Intercept)"])
    }
  }
  
  # For each prediction, generate a sample from the corresponding beta distribution
  results <- data.frame(
    question = questions,
    mean = pred_mean,
    phi = phis
  )
  
  return(results)
}

# Create newdata for prediction
newdata_freq <- crossing(
  question = question_order,
  type = c("expert", "forecaster")
)

# Add the question_type column needed for prediction
newdata_freq <- newdata_freq %>%
  mutate(
    question_type = paste(question, type, sep = "_")
  )

# Generate predictions
freq_predictions <- predict_beta(freq_model, newdata_freq)

# Convert predictions to percentages for easier interpretation
freq_predictions <- freq_predictions %>%
  mutate(
    mean_percent = mean * 100,
    # Calculate alpha and beta parameters for the beta distribution
    alpha = mean * phi,
    beta = (1 - mean) * phi,
    # Calculate 90% confidence intervals using qbeta
    lower_90 = qbeta(0.05, alpha, beta) * 100,
    upper_90 = qbeta(0.95, alpha, beta) * 100
  )

# Simulate from the fitted beta distributions to get full predictive distributions
set.seed(123) # for reproducibility
n_sims <- 10000

# Create a dataframe to store simulations
freq_simulations <- data.frame()

for (i in 1:nrow(freq_predictions)) {
  alpha_i <- freq_predictions$alpha[i]
  beta_i <- freq_predictions$beta[i]
  
  # Generate random samples from Beta distribution
  samples <- rbeta(n_sims, alpha_i, beta_i)
  
  # Add to results dataframe
  sim_df <- data.frame(
    question = rep(freq_predictions$question[i], n_sims),
    type = rep(newdata_freq$type[i], n_sims),
    value = samples
  )
  
  freq_simulations <- rbind(freq_simulations, sim_df)
}

# Calculate summary statistics from simulations
freq_sim_summary <- freq_simulations %>%
  group_by(question, type) %>%
  summarise(
    mean = mean(value),
    median = median(value),
    mode = hdp(value),
    lower_quant_90 = quantile(value, 0.05),
    upper_quant_90 = quantile(value, 0.95),
    .groups = "drop"
  ) %>%
  mutate(
    # Convert to percentages
    mean_percent = mean * 100,
    median_percent = median * 100,
    mode_percent = mode * 100,
    lower_percent = lower_quant_90 * 100,
    upper_percent = upper_quant_90 * 100,
    # Create label for plot
    label = glue::glue("**{nice_num(median_percent, 0, FALSE)}%** [{nice_num(lower_percent, 1, FALSE)}; {nice_num(upper_percent, 1, FALSE)}]")
  )

# Add "all" category by combining samples from both types
freq_sim_all <- freq_simulations %>%
  group_by(question) %>%
  summarise(
    type = "all",
    mean = mean(value),
    median = median(value),
    mode = hdp(value),
    lower_quant_90 = quantile(value, 0.05),
    upper_quant_90 = quantile(value, 0.95),
    .groups = "drop"
  ) %>%
  mutate(
    mean_percent = mean * 100,
    median_percent = median * 100,
    mode_percent = mode * 100,
    lower_percent = lower_quant_90 * 100,
    upper_percent = upper_quant_90 * 100,
    label = glue::glue("**{nice_num(median_percent, 0, FALSE)}%** [{nice_num(lower_percent, 1, FALSE)}; {nice_num(upper_percent, 1, FALSE)}]")
  )

# Combine summaries
freq_sim_combined <- bind_rows(freq_sim_summary, freq_sim_all) %>%
  mutate(type = factor(type, levels = c("forecaster", "expert", "all")))

# Create plot for frequentist beta regression predictions
j_png("frequentist beta regression plot",
      height = 5)
freq_sim_combined %>%
  filter(question != "Military") %>%
  mutate(question = factor(question, levels = question_order)) %>%
  ggplot(aes(x = median_percent, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), 
                    breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), 
                    labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), 
                    expand = expansion(add = 0)) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = lower_percent, xmax = upper_percent), 
                position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), 
           fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = label, alpha = type), 
               fill = NA, text.color = "black", color = NA, 
               position = position_dodge(.8), size = 2.4, family = "Jost", 
               show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Frequentist Beta Regression Model Predictions",
    subtitle = "Predictive distribution without Bayesian priors",
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# Compare raw medians with frequentist model predictions
raw_vs_freq_comparison <- data %>%
  group_by(question, type) %>%
  summarise(
    raw_mean = mean(main) / 100,  # Convert to proportion
    raw_median = median(main) / 100,
    raw_min = min(main) / 100,
    raw_max = max(main) / 100,
    n = n(),
    .groups = "drop"
  ) %>%
  left_join(
    freq_sim_summary %>% select(question, type, mean, median, lower_quant_90, upper_quant_90),
    by = c("question", "type")
  ) %>%
  mutate(
    diff_mean = (mean - raw_mean) * 100,  # Difference in percentage points
    diff_median = (median - raw_median) * 100
  )

# Print comparison to see if frequentist approach better matches raw data
print(raw_vs_freq_comparison %>% 
      select(question, type, raw_median, median, diff_median, lower_quant_90, upper_quant_90))

# Create a plot to compare Bayesian and Frequentist approaches
# First, prepare data from both approaches
bayesian_data <- beta_pred_summary %>%
  select(question, type, perc_median, perc_lower_quant_90, perc_upper_quant_90) %>%
  rename(
    median = perc_median,
    lower = perc_lower_quant_90,
    upper = perc_upper_quant_90
  ) %>%
  mutate(model = "Bayesian")

frequentist_data <- freq_sim_combined %>%
  select(question, type, median_percent, lower_percent, upper_percent) %>%
  rename(
    median = median_percent,
    lower = lower_percent,
    upper = upper_percent
  ) %>%
  mutate(model = "Frequentist")

# Combine the datasets
comparison_data <- bind_rows(bayesian_data, frequentist_data) %>%
  mutate(
    question = factor(question, levels = question_order),
    type = factor(type, levels = c("forecaster", "expert", "all")),
    model = factor(model, levels = c("Bayesian", "Frequentist"))
  )

# Create raw data for plotting
raw_data_for_plot <- data %>%
  group_by(question, type) %>%
  summarise(
    median = median(main),
    .groups = "drop"
  ) %>%
  mutate(
    model = "Raw data",
    lower = NA,  # No CIs for raw data
    upper = NA,
    question = factor(question, levels = question_order),
    type = factor(type, levels = c("forecaster", "expert"))
  )

# Add individual points - add a model column to avoid the error
individual_points <- data %>%
  select(question, type, main) %>%
  rename(value = main) %>%
  mutate(
    question = factor(question, levels = question_order),
    type = factor(type, levels = c("forecaster", "expert")),
    model = "Individual data"  # Add this line to create the model column
  )

# Create comparison plot
j_png("bayesian vs frequentist comparison",
      height = 7)
comparison_data %>%
  filter(question != "Military", type != "all") %>%  # Focus on forecaster vs expert
  ggplot(aes(x = median, y = question, color = model, shape = model)) +
  # Add raw data points - now specify aesthetics differently to avoid conflict
  geom_point(data = individual_points %>% filter(question != "Military"),
            aes(x = value, y = question), 
            position = position_jitter(height = 0.2, width = 0),
            alpha = 0.4, size = 2, color = "gray50", shape = 16) + # Fixed by setting color and shape directly
  # Add model predictions
  geom_errorbarh(aes(xmin = lower, xmax = upper), 
                position = position_dodge(width = 0.8), height = 0.3) +
  geom_point(position = position_dodge(width = 0.8), size = 3) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(add = c(1, 1))) +
  scale_y_discrete(limits = rev) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(~ type) +
  labs(
    x = "Estimated probability (%)",
    y = "",
    title = "Comparison of Bayesian vs. Frequentist Beta Regression",
    subtitle = "With individual data points in gray",
    color = "Model type",
    shape = "Model type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()
