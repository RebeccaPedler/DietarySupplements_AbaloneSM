install.packages(c("patchwork", "ggplot2", "tidyverse", "rnaturalearth", "rnaturalearthdata", "sf", "ggplot2", "dplyr", "grid", "readr", "here"))

library(tidyverse)
library(rnaturalearth)   # for map data
library(rnaturalearthdata)
library(sf)              # for spatial handling
library(ggplot2)
library(dplyr)
library(grid)
library(patchwork)
library(here)
library(readr)

###Please download GitHub repository and then run the following
here()
data <- read_csv(here("GitHub", "DietarySupplements_AbaloneSM","Data", "Systematic Evidence Map - Dietary supplements and abalone performance.csv"))
head(data)

# To identify NA in any column
na_intervention_rows <- data %>% 
  filter(is.na(intervention_category)) %>% 
  select(study_ID, intervention_category)
print(na_intervention_rows)

# Get unique study IDs
unique_ids <- unique(data$study_ID)
print(unique_ids)
total_studies <- length(unique(data$study_ID))
print(total_studies)

# Summarise unique study_IDs by publication type
pubtype_summary <- data %>%
  distinct(study_ID, publication_type) %>%
  group_by(publication_type) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100,
         label = paste0(publication_type, "\n", round(percentage, 1), "%"))
print(pubtype_summary)

# Create label: "65% (13)"
pubtype_summary <- pubtype_summary %>%
  mutate(
    percentage = 100 * count / total_studies,
    label = paste0(round(percentage, 1), "% (", count, ")")
  )

# Donut plot for publication_type
ggplot(pubtype_summary, aes(x = 2, y = count, fill = publication_type)) +
  geom_col(color = "black") +               # border around slices
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage,1), "% (", count, ")")),
            position = position_stack(vjust = 0.5),
            color = "black",
            fontface = "bold") +
  scale_fill_manual(values = c("lightblue", "steelblue")) +
  labs(title = "A",
       fill = "Publication Type") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.1,vjust = -8.0)
  ) +
  xlim(1.0, 2.5) 

# Summarise number and percentage of unique studies per publication year
publication_summary <- data %>%
  distinct(study_ID, publication_year) %>%
  group_by(publication_year) %>%
  summarise(unique_study_count = n()) %>%
  mutate(
    percentage = 100 * unique_study_count / total_studies,
    label_percent = paste0(round(percentage, 1), "%"),
    label_count = unique_study_count
  )
print(publication_summary, n=30)

# Define a named vector with full journal names and their abbreviations
abbrev_lookup <- c(
  "Aquaculture" = "Aquaculture",
  "Fish and Shellfish Immunology" = "Fish Shellfish Immunol",
  "Aquaculture Reports" = "Aquac Rep",
  "Journal of Shellfish Research" = "J Shellfish Res",
  "Aquaculture International" = "Aquac Int",
  "Aquaculture Research" = "Aquac Res",
  "International Journal of Biological Macromolecules" = "Int J Biol Macromol",
  "Animal Feed Science and Technology" = "Anim Feed Sci Technol",
  "Aquaculture Nutrition" = "Aquac Nutr",
  "Animals" = "Animals (Basel)",
  "New Zealand Journal of Marine and Freshwater Research" = "NZ J Mar Freshw Res",
  "Journal of the World Aquaculture Society" = "J World Aquac Soc",
  "Journal of Marine Science and Engineering" = "J Mar Sci Eng",
  "Journal of Applied Phycology" = "J Appl Phycol",
  "Fisheries Science" = "Fish Sci",
  "Bioflux" = "Bioflux",
  "Antioxidants" = "Antioxidants (Basel)",
  "Journal of Experimental Zoology Part A: Comparative Experimental Biology" = "J Exp Zool A",
  "Veterinaria Mexico" = "Vet Mex",
  "Journal of Experimental Marine Biology and Ecology" = "J Exp Mar Biol Ecol",
  "Journal of Oceanology and Limnology" = "J Oceanol Limnol",
  "African Journal of Marine Science" = "Afr J Mar Sci",
  "Journal of Nutrition" = "J Nutr",
  "AIP Conference Proceedings" = "AIP Conf Proc",
  "Cell Biology and Toxicology" = "Cell Biol Toxicol",
  "Chinese Journal of Oceanology and Limnology" = "Chin J Oceanol Limnol",
  "Comparative Biochemistry and Physiology - C Toxicology and Pharmacology" = "Comp Biochem Physiol C",
  "Hidrobiologica" = "Hidrobiologica",
  "Journal of Applied Polymer Science" = "J Appl Polym Sci",
  "Lipids" = "Lipids",
  "PLos ONE" = "PLoS ONE",
  "Cells" = "Cells",
  "Journal of the Science of Food and Agriculture" = "J Sci Food Agric"
)

# Replace NA in Journal with "Grey Literature" and create abbreviated column
data <- data %>%
  mutate(
    Journal = ifelse(is.na(Journal), "Grey Literature", Journal),
    journal_abbrev = ifelse(
      Journal == "Grey Literature",
      "Grey Literature",
      abbrev_lookup[Journal]
    )
  )
head(data)

# Step 1: Summarise number of unique studies per journal-year combo
bubble_data <- data %>%
  distinct(study_ID, journal_abbrev, publication_year) %>%
  group_by(journal_abbrev, publication_year) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 2: Calculate total and percentage
total_studies <- n_distinct(data$study_ID)

bubble_data <- bubble_data %>%
  mutate(percentage = (unique_study_count / total_studies) * 100)

# Step 3: Plot (count as bubble size)
ggplot(bubble_data, aes(y = journal_abbrev, x = as.factor(publication_year))) +
  geom_point(aes(size = unique_study_count), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(
    name = "Number of Articles",
    range = c(3, 15)
  ) +
  scale_x_discrete(limits = as.character(1995:2025)) +  # ensures all years appear
  labs(
    title = "",
    y = "Journal",
    x = "Publication Year"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    
    # Add axis lines
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    
    # Add external tick marks
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.ticks.length.y = unit(0.2, "cm"),
    panel.background = element_blank()
  )


# List validity metrics
validity_metrics <- c(
  "random_assignment",
  "blind_measurement",
  "protocol_registration",
  "raw_data_availability",
  "source_code_availability",
  "funding_disclosed",
  "funding_potential_COI",
  "COI_disclosed"
)

# Loop through each metric and summarise
validity_summary <- data %>%
  select(study_ID, all_of(validity_metrics)) %>%
  distinct() %>%
  pivot_longer(
    cols = all_of(validity_metrics),
    names_to = "metric",
    values_to = "value"
  ) %>%
  group_by(metric, value) %>%
  summarise(
    study_count = n(),
    percentage = 100 * study_count / total_studies,
    .groups = "drop"
  ) %>%
  arrange(metric, desc(study_count))

print(validity_summary)

#Geographical contribution
international_yes_count <- data %>%
  filter(international_collaboration == "Yes") %>%
  summarise(unique_studies = n_distinct(study_ID))

print(international_yes_count)

# Recode country names to match rnaturalearth conventions
data <- data %>%
  mutate(country_standardised = case_when(
    country == "Australia"     ~ "Australia",
    country == "China"         ~ "China",
    country == "Hawaii"        ~ "United States of America",  # Hawaii is part of USA
    country == "Indonesia"     ~ "Indonesia",
    country == "Ireland"       ~ "Ireland",
    country == "Japan"         ~ "Japan",
    country == "Korea"         ~ "South Korea",               # or "Republic of Korea"
    country == "New Zealand"   ~ "New Zealand",
    country == "South Africa"  ~ "South Africa",
    country == "Taiwan"        ~ "Taiwan",                    # not always present in shapefile
    TRUE                       ~ country                      # default: keep original
  ))

country_summary <- data %>%
  distinct(study_ID, country) %>%
  group_by(country) %>%
  summarise(
    study_count = n(),
    percentage = 100 * study_count / total_studies,
    .groups = "drop"
  ) %>%
  rename(name = country)

print(country_summary)

# Get world map shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Join country summary with world shapefile
world_data <- world %>%
  left_join(country_summary, by = "name")

# Step 4: Plot heat map using percentage
ggplot(world_data) +
  geom_sf(aes(fill = percentage)) +
    scale_fill_gradient(
    low = "#deebf7",
    high = "#08306b", na.value = "grey90", name = "% of Articles") +
  labs(
    title = "A",
    caption = "Grey areas = no studies"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
 panel.grid = element_blank(),           # remove all gridlines
  )

# Abbreviate species names: "Haliotis laevigata" -> "H. laevigata"
data <- data %>%
  mutate(species = gsub("Haliotis", "H.", species))

#species summary
species_summary <- data %>%
  group_by(species) %>%
  summarise(count = n_distinct(study_ID)) %>%
  mutate(percentage = 100 * count / total_studies)
print(species_summary)

#Plot
ggplot(species_summary, aes(x = reorder(species, percentage), y = percentage)) +
  geom_col(fill = "steelblue", color = "black") +
  
  # Percentage label above bar
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            hjust = -0.1,
            color = "black",
            fontface = "bold") +

  # Count label inside bar
  geom_text(aes(label = count),
            hjust = 1.3,
            color = "white",
            fontface = "bold",
		size = 4) +

  scale_y_continuous(limits = c(0, 60), expand = c(0, 0)) +
  
  labs(
    title = "",
    x = "Species",
    y = "Percentage of Articles"
  ) +
  
  coord_flip() +
  
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.y = element_text(face = "italic", size = 14, margin = margin(r = 0.5)),
axis.text.x = element_text(size = 12),
axis.title.x = element_text(size = 12),
axis.title.y = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

# Summarise by experimental_unit
experimental_unit_summary <- data %>%
  distinct(study_ID, experimental_unit) %>%
  group_by(experimental_unit) %>%
  summarise(
    study_count = n(),
    percentage = 100 * study_count / total_studies,
    .groups = "drop"
  ) %>%
  arrange(desc(study_count))

# Summarise by experimental_system
experimental_system_summary <- data %>%
  distinct(study_ID, experimental_system) %>%
  group_by(experimental_system) %>%
  summarise(
    study_count = n(),
    percentage = 100 * study_count / total_studies,
    .groups = "drop"
  ) %>%
  arrange(desc(study_count))

# View results
print(experimental_unit_summary)
print(experimental_system_summary)

# INTERVENTION
# Summarise number and percentage of unique studies per intervention category
intervention_summary <- data %>%
  distinct(study_ID, intervention_category) %>%
  group_by(intervention_category) %>%
  summarise(unique_study_count = n(), .groups = "drop") %>%
  mutate(
    percentage = 100 * unique_study_count / total_studies,
    label_count = unique_study_count,                        # number inside bar
    label_percent = paste0(round(percentage, 1), "%")       # percentage outside bar
  )
print(intervention_summary)

# Plot
ggplot(intervention_summary, aes(x = percentage, y = reorder(intervention_category, percentage))) +
  geom_col(fill = "steelblue", color = "black") +
  
  geom_text(aes(label = label_count),
            hjust = 2.0,
            color = "white",
            fontface = "bold") +
  
  geom_text(aes(label = label_percent),
            hjust = -0.1,
            color = "black",
            fontface = "bold") +
  
  labs(
    title = "A",
    x = "Percentage of Articles (%)",
    y = "Intervention Category"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm"),  # increase tick length
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  xlim(0, max(intervention_summary$percentage) + 5)

# Step 1: Filter relevant intervention categories
algal_data <- data %>%
  filter(intervention_category %in% c("Algal or diatom extract", "Algal or diatom meal"))

algal_data <- algal_data %>%
  mutate(algal_species = str_trim(str_to_title(algal_species)))

# Step 2: Count unique study_IDs per species and intervention type
algal_summary <- algal_data %>%
  distinct(study_ID, algal_species, intervention_category) %>%
  group_by(algal_species, intervention_category) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 4: Add percentage and total per species for ordering
total_studies_algae <- sum(algal_summary$unique_study_count)
algal_summary <- algal_summary %>%
  mutate(percentage = 100 * unique_study_count / total_studies_algae) %>%
  group_by(algal_species) %>%
  mutate(total_species_pct = sum(percentage)) %>%
  ungroup()
print(algal_summary)

# Create a label with line break to avoid crowding
algal_summary <- algal_summary %>%
  mutate(label_text = paste0(round(percentage, 1), "%\n(", unique_study_count, ")"))

# Plot
algal_plot <- ggplot(algal_summary, aes(x = percentage, y = reorder(algal_species, total_species_pct), fill = intervention_category)) +
  geom_col(color = "black") +
  geom_text(aes(label = label_text),
            position = position_stack(vjust = 0.5),
            color = "black",
            fontface = "bold",
            size = 1.6) +
  scale_fill_manual(values = c(
    "Algal or diatom meal" = "steelblue",
    "Algal or diatom extract" = "lightblue"
  )) +
  labs(
    title = "",
    x = "Percentage of Articles (%)",
    y = "Algal Species",
    fill = "Intervention Type"
  ) +
  scale_x_continuous(
    limits = c(0, 32),
    breaks = seq(0, 30, 10)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.15),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.y = element_text(face = "italic", size = 8),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = c(0.95, 0.05),       
    legend.justification = c("right","bottom"),  
    legend.background = element_rect(fill = "transparent"), 
    legend.key.size = unit(0.075, "cm"),
    legend.title = element_text(size = 8, face = "bold"),   
    legend.text = element_text(size = 8)    
  )

# Step 1: Filter only Vitamin studies
vitamin_data <- data %>%
  filter(intervention_category == "Vitamin")

# Step 2: Count unique study_IDs per vitamin
vitamin_summary <- vitamin_data %>%
  distinct(study_ID, vitamin) %>%
  group_by(vitamin) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 3: Calculate total mentions and percentages
total_mentions_vitamin <- sum(vitamin_summary$unique_study_count)
vitamin_summary <- vitamin_summary %>%
  mutate(
    percentage = 100 * unique_study_count / total_mentions_vitamin,
    label = paste0(round(percentage, 0.5), "% (", unique_study_count, ")")
  )
print(vitamin_summary)

# Step 4: Plot
vitamin_plot <- ggplot(vitamin_summary, aes(x = percentage, y = reorder(vitamin, percentage))) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = label),
            hjust = 1.1,
            color = "black",
            fontface = "bold",
	size = 1.6) +
  labs(
    title = "",
    x = "Percentage of Articles (%)",
    y = "Vitamin"
  ) + scale_x_continuous(
    limits = c(0, 32),          # axis goes to 45
    breaks = seq(0, 30, 10)     # tick marks every 10
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.15),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.y = element_text(size = 8),
	axis.text.x = element_text(size = 8),
	axis.title.x = element_text(size = 8),
	axis.title.y = element_text(size = 8),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  )

# Step 1: Filter only Mineral studies
mineral_data <- data %>%
  filter(intervention_category == "Mineral")

# Step 2: Count unique study_IDs per mineral
mineral_summary <- mineral_data %>%
  distinct(study_ID, mineral) %>%
  group_by(mineral) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 3: Calculate total and percentages
total_studies_mineral <- sum(mineral_summary$unique_study_count)
mineral_summary <- mineral_summary %>%
  mutate(
    percentage = 100 * unique_study_count / total_studies_mineral,
    label = paste0(round(percentage, 0.5), "% (", unique_study_count, ")")
  )
print(mineral_summary)

# Step 4: Plot
mineral_plot <- ggplot(mineral_summary, aes(x = percentage, y = reorder(mineral, percentage))) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = label),
            hjust = 1.1,
            color = "black",
            fontface = "bold",
size = 1.6) +
  labs(
    title = "",
    x = "Percentage of Articles (%)",
    y = "Mineral"
  ) +  
scale_x_continuous(
    limits = c(0, 42),          # axis goes to 45
    breaks = seq(0, 40, 10)     # tick marks every 10
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.15),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_blank(),
	axis.text.y = element_text(size = 8),
	axis.text.x = element_text(size = 8),
	axis.title.x = element_text(size = 8),
	axis.title.y = element_text(size = 8),
    plot.title = element_text(face = "bold")
  )

# Step 1: Filter only Probiotic studies
probiotic_data <- data %>%
  filter(intervention_category == "Probiotic")

# Step 2: Count unique study_IDs per probiotic
probiotic_summary <- probiotic_data %>%
  distinct(study_ID, probiotic) %>%
  group_by(probiotic) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 3: Calculate total and percentages (format with one decimal place)
total_studies_probiotic <- sum(probiotic_summary$unique_study_count)
probiotic_summary <- probiotic_summary %>%
  mutate(
    percentage = round((unique_study_count / total_studies_probiotic) * 100, 1),
    percentage = sprintf("%.1f", percentage),  # force display to one decimal
    label = paste0(percentage, "% (", unique_study_count, ")")
  )

print(probiotic_summary)

# Step 4: Plot
probiotic_plot <- ggplot(probiotic_summary, aes(x = as.numeric(percentage), y = reorder(probiotic, as.numeric(percentage)))) +
  geom_col(fill = "steelblue", color = "black") +
  geom_text(aes(label = label),
            hjust = 1.1,
            color = "black",
            fontface = "bold",
size = 1.6) +
  labs(
    title = "",
    x = "Percentage of Articles (%)",
    y = "Probiotic"
  ) +
  scale_x_continuous(
    limits = c(0, 32),                 # axis goes to 32
    breaks = seq(0, 30, 10)            # tick marks every 10
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black", size = 0.15),
    axis.ticks.length = unit(0.15, "cm"),
    axis.text.y = element_text(face = "italic", size = 8),
	axis.text.x = element_text(size = 8),
	axis.title.x = element_text(size = 8),
	axis.title.y = element_text(size = 8),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  )

intervention_plots <- (algal_plot + vitamin_plot) / (mineral_plot + probiotic_plot) +
  plot_annotation(
    tag_levels = 'A',
    tag_prefix = "", 
    theme = theme(
      plot.tag = element_text(face = "bold", size = 14) 
    )
  ) +
  plot_layout(
    heights = c(1, 1),
    widths = c(1, 1),
    byrow = TRUE
  )

# Reduce spacing between plots
intervention_plots <- intervention_plots & theme(
  plot.margin = margin(2, 2, 2, 2)
)

intervention_plots

# OUTCOME
outcome_summary <- data %>%
  distinct(study_ID, outcome_category) %>%
  group_by(outcome_category) %>%
  summarise(unique_study_count = n(), .groups = "drop") %>%
  mutate(
    percentage = 100 * unique_study_count / total_studies,
    label_count = unique_study_count,
    label_percent = paste0(round(percentage, 1), "%")
  )
print(outcome_summary)
print(total_studies)

ggplot(outcome_summary, aes(x = percentage, y = reorder(outcome_category, percentage))) +
  geom_col(fill = "steelblue", color = "black") +
  
  geom_text(aes(label = label_count),
            hjust = 2.0,
            color = "white",
            fontface = "bold") +
  
  geom_text(aes(label = label_percent),
            hjust = -0.1,
            color = "black",
            fontface = "bold") +
  
  labs(
    title = "B",
    x = "Percentage of Articles (%)",
    y = "Outcome Category"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.25, "cm"),  # external tick length
    axis.ticks.y = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  xlim(0, max(outcome_summary$percentage) + 5)

# Step 1: Summarise number of unique studies per intervention-outcome combo
bubble_data_OI <- data %>%
  distinct(study_ID, intervention_category, outcome_category) %>%
  group_by(intervention_category, outcome_category) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 2: Calculate total and percentage
total_studies <- n_distinct(data$study_ID)

bubble_data_OI <- bubble_data_OI %>%
  mutate(percentage = (unique_study_count / total_studies) * 100)

# Step 2: Reorder axes by total frequency
# Calculate total counts for ordering
intervention_order <- bubble_data_OI %>%
  group_by(intervention_category) %>%
  summarise(total = sum(unique_study_count)) %>%
  arrange(desc(total)) %>%
  pull(intervention_category)

outcome_order <- bubble_data_OI %>%
  group_by(outcome_category) %>%
  summarise(total = sum(unique_study_count)) %>%
  arrange(desc(total)) %>%
  pull(outcome_category)

# Step 3: Plot
ggplot(bubble_data_OI, aes(
  x = factor(outcome_category, levels = outcome_order),
  y = factor(intervention_category, levels = intervention_order)
)) +
  geom_point(aes(size = unique_study_count), color = "steelblue", alpha = 0.7) +
  scale_size_continuous(
    name = "Number of Articles",
    range = c(3, 15)
  ) +
  labs(
    title = "",
    x = "Outcome Category",
    y = "Intervention Category"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),

    # Add axis lines
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),

    # Add external tick marks
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    panel.background = element_blank()
  )

#Return full citation list and save as a txt file
# Extract unique study_ID and full_citation, ordered alphabetically by citation
unique_citations <- data %>%
  distinct(study_ID, full_citation) %>%
  arrange(full_citation)
print(unique_citations$full_citation)
writeLines(unique_citations$full_citation, "unique_citations.txt")

########OTHER RANDOM CODE TO ASSIST IN DISCUSSION
# Step 1: Filter only Probiotic studies
probiotic_country <- data %>%
  filter(intervention_category == "Probiotic")

# Step 2: Count unique study_IDs per country
probiotic_country_summary <- probiotic_country %>%
  distinct(study_ID, country) %>%
  group_by(country) %>%
  summarise(unique_study_count = n(), .groups = "drop")

# Step 3: Calculate total and percentages (format with one decimal place)
total_studies_probiotic <- n_distinct(probiotic_country$study_ID)

probiotic_country_summary <- probiotic_country_summary %>%
  mutate(
    percentage = round((unique_study_count / total_studies_probiotic) * 100, 1),
    label = paste0(percentage, "% (", unique_study_count, ")")  # optional label
  )

# View
print(probiotic_country_summary)

# Filter for Australia and summarize by literature type
australia_summary <- data %>%
  filter(country == "Australia") %>%
  distinct(study_ID, publication_type) %>%  # keep unique study_ID x literature_type
  group_by(publication_type) %>%
  summarise(
    study_count = n(),
    proportion = study_count / sum(study_count) * 100,
    .groups = "drop"
  )

print(australia_summary)





