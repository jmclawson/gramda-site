
library(tidyverse)

vocped <- read_csv("data/Vocped2022.csv") |> 
  rename(sc_hz = `SC(Hz)`) |> 
  mutate(sc_hz = sc_hz |> 
           str_replace_all("--undefined--", 
                           as.character(NA)) |> 
           as.numeric()) |> 
  mutate(sample = paste(Source, "-", ImitatedInst))

###### Summarize ######

#### Table ####

# Make meaningful summaries

summary_data <- vocped |> 
  mutate(
    status = case_when(
      is.na(ImitatedInst) ~ "actual",
      TRUE ~ "learned"
      ),
    instrument = case_when(
      is.na(ImitatedInst) ~ Source,
      TRUE ~ tolower(ImitatedInst))) |> 
  group_by(status, instrument, Melody, Source) |> 
  summarize(average = mean(sc_hz, na.rm = TRUE)) |> 
  pivot_wider(names_from = "status",
              values_from = "average")

summary_data_actual <- summary_data |> 
  select(-learned) |> 
  filter(!is.na(actual))

summary_data_learned <- summary_data |> 
  select(-actual) |> 
  filter(!is.na(learned))

summary_combo <-
  left_join(summary_data_learned, 
            summary_data_actual |> 
              select(-Source), 
            by = c("instrument", "Melody")) |> 
  filter(!is.na(actual), !is.na(learned)) |> 
  select(singer = Source, 
         melody = Melody, 
         training_instrument = instrument,
         voice_value = learned,
         instrument_value = actual)

# Export the summary table

write_csv(summary_combo, "summary-table.csv")

#### Visualization ####

summary_combo |> 
  ggplot(aes(instrument_value, voice_value)) +
  geom_point(aes(color = training_instrument,
                 shape = as.factor(melody))) +
  labs(shape = "Melody",
       color = "Instrument",
       x = "Instrument recording SC value",
       y = "Singer SC value") +
  theme_minimal()

# Save as a PDF

ggsave("summary-points.pdf", height = 5.5, width = 8)


###### Correlation ######

# Test the Pearson correlation

correlation <- 
  cor.test(summary_combo$instrument_value, 
           summary_combo$voice_value,
           method = "pearson")

print(correlation)

###### Visualizing ######

#### linear over time - by instrument ####

# First, make charts for both melodies and Piano.

vocped |>
  mutate(ImitatedInst = case_when(
    Source == "piano" ~ "Piano",
    Source == "sax" ~ "Sax",
    TRUE ~ ImitatedInst
  )) |> 
  mutate(Melody = paste("Melody", Melody)) |> 
  filter(ImitatedInst == "Piano") |> 
  ggplot(aes(x = tmin, 
             y = sc_hz,
             group = Source, 
             color = as.factor(Melody))) +
  geom_line() +
  facet_grid(cols = vars(Melody),
             rows = vars(Source)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "SC over time for Piano samples",
       x = "time (second)",
       y = "spectral centroid (Hz)")

# Now save as PDF

ggsave("piano-samples.pdf", 
       height = 11, 
       width = 8.5)

# Then make charts for both melodies and Sax.

vocped |>
  mutate(ImitatedInst = case_when(
    Source == "piano" ~ "Piano",
    Source == "sax" ~ "Sax",
    TRUE ~ ImitatedInst
  )) |> 
  mutate(Melody = paste("Melody", Melody)) |> 
  filter(ImitatedInst == "Sax") |> 
  ggplot(aes(x = tmin, y = sc_hz,
             group = Source, color = as.factor(Melody))) +
  geom_line() +
  facet_grid(cols = vars(Melody),
             rows = vars(Source)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "SC over time for Sax samples",
       x = "time (second)",
       y = "spectral centroid (Hz)")

# Save as PDF

ggsave("sax-samples.pdf", 
       height = 11, 
       width = 8.5)

#### linear over time - by melody ####

# First, make charts for both instruments and melody 1.

vocped |>
  mutate(ImitatedInst = case_when(
    Source == "piano" ~ "Piano",
    Source == "sax" ~ "Sax",
    TRUE ~ ImitatedInst
  )) |> 
  mutate(Source = case_when(
    Source == "piano" ~ "Instrument",
    Source == "sax" ~ "Instrument",
    TRUE ~ Source
  )) |> 
  filter(Melody == 1) |> 
  ggplot(aes(x = tmin, 
             y = sc_hz,
             group = Source, 
             color = ImitatedInst)) +
  geom_line() +
  facet_grid(cols = vars(ImitatedInst),
             rows = vars(Source)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "SC over time for Melody 1 samples",
       x = "time (second)",
       y = "spectral centroid (Hz)")

# Now save as PDF

ggsave("melody1-samples.pdf", 
       height = 11, 
       width = 8.5)

# Then make charts for both instruments and melody 2.

vocped |>
  mutate(ImitatedInst = case_when(
    Source == "piano" ~ "Piano",
    Source == "sax" ~ "Sax",
    TRUE ~ ImitatedInst
  )) |> 
  mutate(Source = case_when(
    Source == "piano" ~ "Instrument",
    Source == "sax" ~ "Instrument",
    TRUE ~ Source
  )) |> 
  filter(Melody == 2) |> 
  ggplot(aes(x = tmin, 
             y = sc_hz,
             group = Source, 
             color = ImitatedInst)) +
  geom_line() +
  facet_grid(cols = vars(ImitatedInst),
             rows = vars(Source)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "SC over time for Melody 2 samples",
       x = "time (second)",
       y = "spectral centroid (Hz)")

# Save as PDF

ggsave("melody2-samples.pdf", 
       height = 11, 
       width = 8.5)

#### box plots ####

vocped |> 
  mutate(ImitatedInst = case_when(
    ImitatedInst == "Piano" ~ "Voice \nlearned from Piano",
    ImitatedInst == "Sax" ~ "Voice \nlearned from Sax",
    TRUE ~ ImitatedInst
  )) |> 
  mutate(ImitatedInst = case_when(
    Source == "piano" ~ "Piano",
    Source == "sax" ~ "Sax",
    TRUE ~ ImitatedInst
  ) |> 
    factor(levels = c("Piano", "Voice \nlearned from Piano", "Voice \nlearned from Sax", "Sax"))) |> 
  ggplot(
    aes(x = ImitatedInst, 
        y = sc_hz)) +
  geom_boxplot(aes(color = ImitatedInst),
               show.legend = FALSE) +
  theme_minimal() +
  labs(title = "Voice spectral centroid values follow the instrument from which they learned the melody.",
       x = element_blank(),
       y = "spectral centroid (Hz)") +
  theme(plot.title.position = "plot")

# Save as PDF

ggsave("voice-spectral.pdf", height = 5.5, width = 8)
