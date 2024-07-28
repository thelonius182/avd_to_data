library(gtools)  # for permutations

# Step 1: Define the mappings
cities <- c("Amsterdam", "Berlin", "Copenhagen", "Dublin", "Edinburgh")
accommodations <- c("Hotel", "Hostel", "Apartment", "B&B", "Camping")
durations <- c("8 days", "10 days", "12 days", "14 days", "16 days")

# Step 2: Generate all permutations of the digits 1 to 5
digits <- 1:5
all_permutations <- permutations(n = length(digits), r = length(digits), v = digits, repeats.allowed = FALSE)
set <- apply(all_permutations, 1, paste0, collapse = "")

# Convert to integer type if necessary
set <- as.integer(set)

# Step 3: Use expand.grid to generate combinations
combinations <- expand.grid(set1 = set, set2 = set, set3 = set)

# Step 4: Apply mappings to combinations
map_digits <- function(digit, mapping) {
  sapply(strsplit(as.character(digit), ""), function(x) mapping[as.numeric(x)])
}

combinations_mapped <- data.frame(
  set1 = apply(combinations["set1"], 1, map_digits, mapping = cities),
  set2 = apply(combinations["set2"], 1, map_digits, mapping = accommodations),
  set3 = apply(combinations["set3"], 1, map_digits, mapping = durations)
)

# Print a few rows to see the result
head(combinations_mapped)
