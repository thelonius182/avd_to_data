pacman::p_load(gtools, tidyr, dplyr)  # for permutations

# Define the mappings
cities <- c("Haamstede", "Nice", "Noordwijk", "Normandië", "Schoorl")
accommodations <- c("appartement", "B&B", "hotel", "huisje", "tent")
durations <- c("8 days", "10 days", "11 days", "13 days", "14 days")

# Generate all permutations
city_permutations <- permutations(n = length(cities), r = length(cities), v = cities, repeats.allowed = FALSE)
accommodation_permutations <- permutations(n = length(accommodations), r = length(accommodations), v = accommodations, repeats.allowed = FALSE)
duration_permutations <- permutations(n = length(durations), r = length(durations), v = durations, repeats.allowed = FALSE)

# Create all combinations
combinations <- expand.grid(
  city_perm = apply(city_permutations, 1, paste0, collapse = ", "),
  accommodation_perm = apply(accommodation_permutations, 1, paste0, collapse = ", "),
  duration_perm = apply(duration_permutations, 1, paste0, collapse = ", ")
)

# Split each column into separate variables, ensure the columns are character vectors
combinations$city_perm <- as.character(combinations$city_perm)
combinations$accommodation_perm <- as.character(combinations$accommodation_perm)
combinations$duration_perm <- as.character(combinations$duration_perm)

split_permutations <- function(perm_column) {
  do.call(rbind, strsplit(perm_column, ", "))
}

cities_combined <- split_permutations(combinations$city_perm)
accommodations_combined <- split_permutations(combinations$accommodation_perm)
durations_combined <- split_permutations(combinations$duration_perm)

# Combine into a single data frame, adding column names
result <- data.frame(cities_combined, accommodations_combined, durations_combined)

colnames(result) <- c(
  paste0("city_", 1:ncol(cities_combined)),
  paste0("accommodation_", 1:ncol(accommodations_combined)),
  paste0("duration_", 1:ncol(durations_combined))
)

# make it a tidy table
result_long <- pivot_longer(
  result, 
  cols = everything(), 
  names_to = c(".value", "property"),
  names_sep = "_"
)

# add family names
result_fam <- result_long |> mutate(family = case_when(property == "1" ~ "De Boer",
                                                       property == "2" ~ "Van Dijk",
                                                       property == "3" ~ "Jansen",
                                                       property == "4" ~ "De Jong",
                                                       TRUE ~ "de Vries"),
                                    combi_id = if_else(property == "1", row_number(), NA_integer_),
                                    bi_bu = if_else(city %in% c("Normandië", "Nice"), "buitenland", "binnenland")) |> 
  fill(combi_id, .direction = c("down")) |> 
  select(combi_id, duration, accommodation, family, destination = city, bi_bu)
