# Load required libraries
library(tidyverse)
library(lubridate)
library(ggtext)
library(patchwork)
library(jimbilben) # devtools::install_github("Jimbilben/jimbilben")

# Aesthetics
# theme_set(theme_jimbilben(10))
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
    hour = hour(time),
    minute = minute(time),
    decimal_time = hour + minute/60,
    phase = case_when(
      # Convert comparison times to decimal for easier comparison
      decimal_time < 13 + 40/60 ~ "Initial",
      decimal_time >= 13 + 40/60 & decimal_time <= 14 ~ "Ref Classes",
      decimal_time > 14 ~ "Discussion",
      TRUE ~ "Other"
    ),
    phase = factor(phase, levels = c("Initial", "Ref Classes", "Discussion"))
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
  filter(phase == "Discussion") %>%
  group_by(usercode, question) %>%
  slice(which.max(time)) %>%  # Take the last one from the final phase
  ungroup() %>%
  mutate(checkpoint = "Discussion")

# Combine all checkpoints
checkpoints <- bind_rows(initial_forecasts, update_forecasts, final_forecasts) %>%
  mutate(checkpoint = factor(checkpoint, levels = c("Initial", "Ref Classes", "Discussion")))

# Handle cases where a participant didn't make updates in all phases
# by creating a complete grid and filling in missing values with the most recent previous entry
all_combinations <- expand.grid(
  usercode = unique(transactions$usercode),
  question = unique(transactions$question),
  checkpoint = c("Initial", "Ref Classes", "Discussion"),
  stringsAsFactors = FALSE
) %>% as_tibble()

# Convert checkpoint to a factor with the correct levels
all_combinations$checkpoint <- factor(all_combinations$checkpoint, 
                                     levels = c("Initial", "Ref Classes", "Discussion"))

# Right join to keep only combinations that exist in the transactions data
checkpoints_complete <- all_combinations %>%
  left_join(checkpoints, by = c("usercode", "question", "checkpoint"))

# Fill forward missing values (if a user didn't update in a particular phase)
checkpoints_filled <- checkpoints_complete %>%
  arrange(usercode, question, checkpoint) %>%
  group_by(usercode, question) %>%
  fill(type, time, low, main, high, phase, hour, minute, decimal_time, .direction = "down") %>%
  ungroup()

# Replace checkpoints with this filled version for the analysis
checkpoints <- checkpoints_filled

# ==================== VISUALIZATIONS ====================
# 1. Distribution of when participants made updates
j_png("update_time_distribution",
      height = 6, width = 8)
ggplot(transactions, aes(x = decimal_time)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "darkblue", alpha = 0.7) +
  # Add vertical lines at the phase boundaries
  geom_vline(xintercept = 13 + 40/60, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 14, linetype = "dashed", color = "red") +
  # Add annotations for the phases
  annotate("text", x = 13, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                         geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Initial Phase", color = "darkblue", hjust = 0.5) +
  annotate("text", x = 13.8, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                           geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Ref Classes Phase", color = "darkblue", hjust = 0.5) +
  annotate("text", x = 14.5, y = max(ggplot_build(ggplot(transactions, aes(x = decimal_time)) + 
                                           geom_histogram(binwidth = 0.1))$data[[1]]$count) * 0.9, 
           label = "Discussion Phase", color = "darkblue", hjust = 0.5) +
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
  ) +
  theme_light()
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
      height = 5, width = 7)
checkpoints %>%
  filter(!question %in% c("Military")) %>%
  ggplot(aes(x = checkpoint, y = main, group = usercode, color = type)) +
  geom_line(alpha = 0.6, linewidth = 0.33) +
  geom_point(shape = 16, fill = "white", size = 1.5) +
  facet_wrap(~question, scales = "free_y") +
  scale_color_manual(values = my_colors) +
  labs(
    title = "Evolution of Forecasts Across Workshop Phases",
    x = "Workshop Phase",
    y = "Forecast Estimate",
    color = "Respondent type:"
  ) +
  theme(
    legend.position = "top"
  )
dev.off()

# 5. Tracking estimates over time for selected questions

# Add vertical lines for the phase transitions
phase_boundaries <- data.frame(
  time = c(hms("13:40:00"), hms("14:00:00"))
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
