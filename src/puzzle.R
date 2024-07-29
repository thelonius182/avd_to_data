pacman::p_load(gtools, tidyr, dplyr)  # for permutations

# Define the mappings
cities <- c("Haamstede", "Nice", "Noordwijk", "Normandië", "Schoorl")
accs <- c("appartement", "B&B", "hotel", "huisje", "tent")
durations <- c("8 days", "10 days", "11 days", "13 days", "14 days")

# Generate all permutations
city_pms <- permutations(n = length(cities), r = length(cities), v = cities, repeats.allowed = FALSE)
acc_pms <- permutations(n = length(accs), r = length(accs), v = accs, repeats.allowed = FALSE)
duration_pms <- permutations(n = length(durations), r = length(durations), v = durations, repeats.allowed = FALSE)

# Create all combis
combis <- expand.grid(
  city_combi = apply(city_pms, 1, paste0, collapse = ", "),
  accommodation_combi = apply(acc_pms, 1, paste0, collapse = ", "),
  duration_combi = apply(duration_pms, 1, paste0, collapse = ", ")
)

# Split each column into separate variables, ensure the columns are character vectors
combis$city_combi <- as.character(combis$city_combi)
combis$accommodation_combi <- as.character(combis$accommodation_combi)
combis$duration_combi <- as.character(combis$duration_combi)

split_pms <- function(perm_column) {
  do.call(rbind, strsplit(perm_column, ", "))
}

cities_combined <- split_pms(combis$city_combi)
accs_combined <- split_pms(combis$accommodation_combi)
durations_combined <- split_pms(combis$duration_combi)

# Combine into a single data frame, adding column names
result <- data.frame(cities_combined, accs_combined, durations_combined)

colnames(result) <- c(
  paste0("city_", 1:ncol(cities_combined)),
  paste0("acc_", 1:ncol(accs_combined)),
  paste0("duration_", 1:ncol(durations_combined))
)

# make it a tidy table
result_long <- pivot_longer(
  result, 
  cols = everything(), 
  names_to = c(".value", "property"),
  names_sep = "_"
)

# add family names, combi_id's and abroad Y/N
result_fam <- result_long |> mutate(family = case_when(property == "1" ~ "De Boer",
                                                       property == "2" ~ "Van Dijk",
                                                       property == "3" ~ "Jansen",
                                                       property == "4" ~ "De Jong",
                                                       TRUE ~ "de Vries"),
                                    combi_id = if_else(property == "1", row_number(), NA_integer_),
                                    bi_bu = if_else(city %in% c("Normandië", "Nice"), "buitenland", "binnenland")) |> 
  fill(combi_id, .direction = c("down")) |> 
  select(combi_id, duration, accommodation = acc, family, destination = city, bi_bu)
