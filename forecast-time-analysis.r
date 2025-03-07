# Load required libraries
library(tidyverse)
library(lubridate)
library(ggtext)
library(patchwork)
library(jimbilben) # devtools::install_github("Jimbilben/jimbilben")

# Aesthetics
theme_set(theme_jimbilben(10))
set_colors()
# you can put totally custom colors in here
# e.g., from https://htmlcolorcodes.com/
# or you can use some of my presets from set_colors()
my_colors <-
  c("expert" = teal,
    "forecaster" = fire_red)

# Read transaction data
transactions <- read_csv("data/transactions.csv")

# Initial data exploration and processing
transactions <- transactions %>%
  # Create a mapping of question codes to descriptive names similar to your existing code
  mutate(
    question = recode(question,
      "q0" = "Overall",
      "q1" = "Consortium", 
      "q2" = "Government lab",
      "q3" = "Nationalization",
      "q4" = "Private contractor",
      "q5" = "Legal compulsion",
      "q6" = "Military"
    ),
    # Create a factor with the desired order
    question = factor(question, levels = c(
      "Overall", "Consortium", "Government lab", "Nationalization", 
      "Private contractor", "Legal compulsion", "Military"
    ))
  )

# Add a phase variable based on the specified time windows
transactions <- transactions %>%
  mutate(
    # hour = hour(time),
    # minute = minute(time),
    decimal_time = hour(time) + minute(time)/60 + second(time)/3600,
    phase = case_when(
      # Convert comparison times to decimal for easier comparison
      decimal_time < 13 + 40/60 ~ "Initial",
      decimal_time >= 13 + 40/60 & decimal_time <= 14 + 5/60 ~ "Ref Classes",
      TRUE ~ "Final"
    ),
    phase = factor(phase, levels = c("Initial", "Ref Classes", "Final"))
  )

# Count the number of updates in each phase
phase_update_counts <- transactions %>%
  count(phase) %>%
  arrange(phase)

# Calculate the number of updates made by each participant
update_counts <- transactions %>%
  count(usercode, question) %>%
  group_by(usercode) %>%
  summarize(
    total_updates = sum(n),
    questions_updated = n_distinct(question)
  )

# Use the phase classification to identify forecasts at key points
# Get the latest update from each phase for each user and question
initial_forecasts <- transactions %>%
  filter(phase == "Initial") %>%
  group_by(usercode, question) %>%
  slice(which.max(time)) %>%  # Take the last one from the initial phase
  ungroup() %>%
  mutate(checkpoint = "Initial")

update_forecasts <- transactions %>%
  filter(phase == "Ref Classes") %>%
  group_by(usercode, question) %>%
  slice(which.max(time)) %>%  # Take the last one from the update phase
  ungroup() %>%
  mutate(checkpoint = "Ref Classes")

final_forecasts <- transactions %>%
  filter(phase == "Final") %>%
  group_by(usercode, question) %>%
  slice(which.max(time)) %>%  # Take the last one from the final phase
  ungroup() %>%
  mutate(checkpoint = "Final")

# Combine all checkpoints
checkpoints <- bind_rows(initial_forecasts, update_forecasts, final_forecasts) %>%
  mutate(checkpoint = factor(checkpoint, levels = c("Initial", "Ref Classes", "Final")))

# Handle cases where a participant didn't make updates in all phases
# by creating a complete grid and filling in missing values with the most recent previous entry
all_combinations <- expand.grid(
  usercode = unique(transactions$usercode),
  question = unique(transactions$question),
  checkpoint = c("Initial", "Ref Classes", "Final"),
  stringsAsFactors = FALSE
) %>% as_tibble()

# Convert checkpoint to a factor with the correct levels
all_combinations$checkpoint <- factor(all_combinations$checkpoint, 
                                     levels = c("Initial", "Ref Classes", "Final"))

# Right join to keep only combinations that exist in the transactions data
checkpoints_complete <- all_combinations %>%
  left_join(checkpoints, by = c("usercode", "question", "checkpoint"))

# Fill forward missing values (if a user didn't update in a particular phase)
checkpoints_filled <- checkpoints_complete %>%
  arrange(usercode, question, checkpoint) %>%
  group_by(usercode, question) %>%
  fill(type, time, low, main, high, phase, decimal_time, .direction = "down") %>%
  ungroup()

# Replace checkpoints with this filled version for the analysis
checkpoints <- checkpoints_filled

# Calculate the change in forecasts between the Initial and Ref Classes phases
checkpoint_changes <- checkpoints %>%
  filter(checkpoint %in% c("Initial", "Ref Classes")) %>%
  select(usercode, question, checkpoint, main) %>%
  pivot_wider(names_from = checkpoint, values_from = main, names_prefix = "main_") %>%
  rename(main_Ref_Classes = `main_Ref Classes`) %>%
  mutate(change = case_when(
    main_Ref_Classes > main_Initial ~ "Up",
    main_Ref_Classes < main_Initial ~ "Down",
    TRUE ~ "Same"
  )) %>%
  count(question, change) %>%
  pivot_wider(names_from = change, values_from = n, values_fill = 0)

# Calculate the change in forecasts between the Ref Classes and Final phases
checkpoint_changes2 <- checkpoints %>%
  filter(checkpoint %in% c("Ref Classes", "Final")) %>%
  select(usercode, question, checkpoint, main) %>%
  pivot_wider(names_from = checkpoint, values_from = main, names_prefix = "main_") %>%
  rename(main_Ref_Classes = `main_Ref Classes`) %>%
  mutate(change = case_when(
    main_Ref_Classes < main_Final ~ "Up",
    main_Ref_Classes > main_Final ~ "Down",
    TRUE ~ "Same"
  )) %>%
  count(question, change) %>%
  pivot_wider(names_from = change, values_from = n, values_fill = 0)

# ==================== VISUALIZATIONS ====================
# 1. Distribution of when participants made updates
j_png("update_time_distribution",
      height = 6, width = 8)
ggplot(transactions, aes(x = decimal_time)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "darkblue", alpha = 0.7) +
  # Add vertical lines at the phase boundaries
  geom_vline(xintercept = 13 + 40/60, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 14 + 5/60, linetype = "dashed", color = "red") +
  # Add annotations for the phases
  annotate("text", x = 13, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                         geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Initial Phase", color = "darkblue", hjust = 0.5) +
  annotate("text", x = 13.87, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                           geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Ref Classes Phase", color = "darkblue", hjust = 0.5) +
  annotate("text", x = 14.5, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                           geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Final Phase", color = "darkblue", hjust = 0.5) +
  scale_x_continuous(
    breaks = seq(13, 15, by = 0.25),
    labels = function(x) {
      hour <- floor(x)
      minute <- round((x - hour) * 60)
      sprintf("%02d:%02d", hour, minute)
    }
  ) +
  labs(
    title = "Distribution of Forecast Updates Over Time",
    x = "Time of Day",
    y = "Number of Updates"
  )
dev.off()

# 2. Heatmap of updates by user and question by phase
heatmap_data <- transactions %>%
  count(usercode, question, phase) %>%
  group_by(usercode) %>%
  mutate(user_total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(user_total)) %>%
  mutate(usercode = factor(usercode, levels = unique(usercode)))

# Create a heatmap showing updates by participant, question, and phase
j_png("heatmap",
      height = 6, width = 8)
ggplot(heatmap_data, aes(x = question, y = usercode, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(
    option = "plasma",
    begin = 0.1, end = 0.9,
    name = "Updates"
  ) +
  geom_text(aes(label = n), color = "white", size = 3) +
  facet_wrap(~phase) +
  labs(
    title = "Number of Updates by Participant and Question",
    x = "Question",
    y = "Participant"
  ) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )
dev.off()

# 3. Comparing forecasts at checkpoints

j_png("checkpoints",
      height = 4, width = 7)
checkpoints %>%
  filter(!question %in% c("Military")) %>%
  ggplot(aes(x = checkpoint, y = main, group = usercode, shape = type, color = usercode)) +
  geom_line(alpha = 0.6, linewidth = 0.33) +
  geom_point(size = 1.5) +
  facet_wrap(~question, scales = "free_y") +
  guides(
    color = guide_legend(override.aes = list(shape = 15)), # Set default shape for all colors
    shape = guide_legend(title = "Respondent type")
  ) +
  labs(
    title = "Evolution of Forecasts Across Workshop Phases",
    x = "Workshop Phase",
    y = "Estimate (%)",
    shape = "Respondent type",
    color = "Participant"
  ) +
  theme(
    legend.position = "right"
  )
dev.off()

# 4. Initial vs final forecast

# Reshape data to have initial and final forecasts in separate columns
forecast_comparison <- checkpoints %>%
  filter(question != "Military") %>%
  select(usercode, question, checkpoint, main, type) %>%
  pivot_wider(names_from = checkpoint, values_from = main)

make_corr_plot <- function(forecast_comparison, x, y) {
  # Calculate correlation coefficients for each question
  correlations <- forecast_comparison %>%
    group_by(question) %>%
    summarize(
      r = cor(!!sym(x), !!sym(y), use = "complete.obs"),
      r_squared = r^2,
      .groups = "drop"
    )
  
  # Create correlation label for each question
  question_labels <- correlations %>%
    mutate(label = sprintf("R² = %.2f", r_squared))

  # Create a scatter plot with correlation text
  ggplot(forecast_comparison, aes(x = !!sym(x), y = !!sym(y))) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray70") +
    geom_point(aes(color = type), size = 1.5, alpha = 0.7) +
    geom_text(data = question_labels, 
              aes(x = Inf, y = -Inf, label = label),
              hjust = 1.1, vjust = -0.5, size = 3) +
    scale_color_manual(values = my_colors) +
    # Facet by question - REMOVE the free scales option
    facet_wrap(~question, scales = "free") +
    labs(
      # extract title and labels from x and y
      title = "Correlations between stages",
      x = paste0(x," forecast"),
      y = paste0(y," forecast"),
      color = "Respondent type"
    )
}

# Create the three figures
j_png("initial_final_forecasts",
      height = 5)
make_corr_plot(forecast_comparison, "Initial", "Final")
dev.off()

j_png("initial_ref_classes_forecasts",
      height = 5)
make_corr_plot(forecast_comparison, "Initial", "Ref Classes")
dev.off()

j_png("ref_classes_final_forecasts",
      height = 5)
make_corr_plot(forecast_comparison, "Ref Classes", "Final")
dev.off()

# Calculate a regression model for all questions combined
overall_model <- forecast_comparison %>%
  # Fit linear model
  lm(Final ~ Initial, data = .)

# Print model summary
summary(overall_model)

# 5. Tracking estimates over time for selected questions

# Add vertical lines for the phase transitions
phase_boundaries <- data.frame(
  time = c(hms("13:40:00"), hms("14:05:00"))
)

j_png("time_series_selected",
      height = 6, width = 8)
transactions %>%
  filter(!question %in% c("Military"), main != 0) %>%
  ggplot(aes(x = time, y = main, color = usercode, group = usercode)) +
  geom_vline(data = phase_boundaries, aes(xintercept = as.numeric(time)), 
             linetype = "dashed", color = "darkgrey") +
  geom_line(alpha = 0.7) +
  geom_point(size = 1.5) +
  facet_wrap(~question) +
  # Manually set breaks at 30-minute intervals
  scale_x_time(
    breaks = hms(c("13:00:00", "13:30:00", "14:00:00", "14:30:00")),
    labels = c("13:00", "13:30", "14:00", "14:30")
  ) +
  labs(
    title = "Estimate Evolution Over Time",
    x = "Time",
    y = "Main Estimate"
  ) +
  theme_light() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold")
  )
dev.off()
