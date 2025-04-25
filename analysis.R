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

# Simple aggregation method using arithmetic means
# This is a simpler alternative to the metalog distribution approach

# Calculate simple aggregate statistics directly from the raw data
simple_aggregation <- function(data) {
  # Aggregate by question (across all participant types)
  all_participants <- data %>%
    group_by(question) %>%
    summarize(
      median = mean(main, na.rm = TRUE),        # Mean of medians
      quant_05 = mean(low, na.rm = TRUE),       # Mean of lower bounds
      quant_95 = mean(high, na.rm = TRUE),      # Mean of upper bounds
      type = "all",
      .groups = "drop"
    )
  
  # Aggregate by question and participant type
  by_type <- data %>%
    group_by(question, type) %>%
    summarize(
      median = mean(main, na.rm = TRUE),        # Mean of medians
      quant_05 = mean(low, na.rm = TRUE),       # Mean of lower bounds
      quant_95 = mean(high, na.rm = TRUE),      # Mean of upper bounds
      .groups = "drop"
    )
  
  # Combine the results
  combined_results <- bind_rows(all_participants, by_type) %>%
    # Create labels for the plot
    mutate(label = glue::glue("**{round(median, 1)}%** [{round(quant_05, 1)}; {round(quant_95, 1)}]")) %>%
    # Ensure type has the desired order
    mutate(type = factor(type, levels = c("forecaster", "expert", "all")))
  
  return(combined_results)
}

# Apply the function to your data
simple_agg_results <- simple_aggregation(data)

# Create a similar summary plot using the simple aggregation method
j_png("simple_aggregation_summary_plot",
      height = 5)

simple_agg_results %>%
  filter(question != "Military") %>%  # Remove Military question to match the original plot
  ggplot(aes(x = median, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), 
                     breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), 
                     labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), 
                     expand = expansion(add = c(1,1))) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = quant_05, xmax = quant_95), position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = label, alpha = type), 
               fill = NA, text.color = "black", color = NA, 
               position = position_dodge(.8), size = 2.4, 
               family = "Jost", show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Simple arithmetic mean aggregation of estimates",
    subtitle = "Using mean of medians and confidence bounds",
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# Read the Bayesian model data
bayesian_data <- read_csv("data/bayesian.csv")

# Apply the factor ordering
bayesian_data <- bayesian_data %>%
  mutate(question = factor(question, levels = question_order)) %>%
  mutate(type = factor(type, levels = c("forecaster", "expert", "all")))

# Create labels for the plot
bayesian_data <- bayesian_data %>%
  mutate(label = glue::glue("**{round(median*100, 1)}%** [{round(quant_05*100, 1)}; {round(quant_95*100, 1)}]"))

# Create a similar summary plot using the simple aggregation method
j_png("bayesian_hierarchical_model",
      height = 5)

bayesian_data %>%
  filter(question != "Military") %>%  # Remove Military question to match the original plot
  ggplot(aes(x = median * 100, y = question, color = type)) +
  scale_x_continuous(limits = c(0, 152.5), 
                     breaks = c(seq(0, 100, 20), mean(c(100, 152.5))), 
                     labels = c(as.character(seq(0, 100, 20)), "**Parameter<br>estimates**"), 
                     expand = expansion(add = c(1,1))) +
  scale_y_discrete(limits = rev) +
  geom_errorbarh(aes(xmin = quant_05 * 100, xmax = quant_95 * 100), position = position_dodge(.8), height = .25) +
  geom_point(position = position_dodge(.8)) +
  geom_rect(aes(xmin = 100, xmax = 152.5, ymin = -Inf, ymax = Inf), fill = "grey99", color = "grey98", linewidth = .1) +
  geom_richtext(aes(x = mean(c(100, 152.5)), label = label, alpha = type), 
               fill = NA, text.color = "black", color = NA, 
               position = position_dodge(.8), size = 2.4, 
               family = "Jost", show.legend = FALSE) +
  scale_alpha_manual(values = c(1, 1, 1)) +
  scale_color_manual(values = my_colors) +
  guides(color = guide_legend(reverse = TRUE)) +
  labs(
    x = "",
    y = "",
    title = "Bayesian Hierarchical Model Estimates",
    subtitle = "Posterior medians with 90% credible intervals",
    color = "Respondent type"
  ) +
  theme(
    legend.position = "top"
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
                    expand = expansion(add = c(1,1))) +
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