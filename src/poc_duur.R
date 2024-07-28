# Load required libraries
library(dplyr, tidyr)

# Your initial tibble
duur <- tibble(fam_1 = 3, fam_2 = 2, fam_3 = 1, fam_4 = 5, fam_5 = 4)

# Vector of duur labels
duur_labels <- paste0(c(8, 10, 11, 13, 14), " dagen")
fam_labels <- c("De Boer", "Van Dijk", "Jansen", "De Jong", "De Vries")

# Define a named vector to match the values to the labels
duur_lookup <- setNames(c(8, 10, 11, 13, 14), 1:5)

# Convert the tibble to long format and add the correct labels
result <- duur |> 
  pivot_longer(everything(), names_to = "familie", values_to = "index") |> 
  mutate(n_dagen = duur_lookup[as.character(index)],
         familie = fam_labels[index]) |> 
  select(familie, n_dagen)

# Print the result
print(result)
