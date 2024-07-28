library(gtools)  # for permutations

# Step 1: Define the mappings
cities <- c("Haamstede", "Nice", "Noordwijk", "Normandië", "Schoorl")
accommodations <- c("appartement", "B&B", "hotel", "huisje", "tent")
durations <- c("8 days", "10 days", "11 days", "13 days", "14 days")

in_NL <- c("binnenland", "buitenland", "binnenland", "buitenland", "binnenland")

# Step 2: Generate all permutations of the digits 1 to 5 for each property
city_permutations <- permutations(n = length(cities), r = length(cities), v = cities, repeats.allowed = FALSE)
accommodation_permutations <- permutations(n = length(accommodations), r = length(accommodations), v = accommodations, repeats.allowed = FALSE)
duration_permutations <- permutations(n = length(durations), r = length(durations), v = durations, repeats.allowed = FALSE)

# Step 3: Create all combinations
combinations <- expand.grid(
  city_perm = apply(city_permutations, 1, paste0, collapse = ", "),
  accommodation_perm = apply(accommodation_permutations, 1, paste0, collapse = ", "),
  duration_perm = apply(duration_permutations, 1, paste0, collapse = ", ")
)

# Step 4: View the result
head(combinations)
