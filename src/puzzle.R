pacman::p_load(gtools, tidyr, dplyr, stringr)

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

rm(city_pms, acc_pms, duration_pms)

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

rm(combis)

# Combine into a single data frame, adding column names
result <- data.frame(cities_combined, accs_combined, durations_combined)

colnames(result) <- c(
  paste0("city_", 1:ncol(cities_combined)),
  paste0("acc_", 1:ncol(accs_combined)),
  paste0("duration_", 1:ncol(durations_combined))
)

rm(cities_combined, accs_combined, durations_combined)

# make it a tidy table
result_long <- pivot_longer(
  result, 
  cols = everything(), 
  names_to = c(".value", "property"),
  names_sep = "_"
)

rm(result)

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

rm(result_long)

# rule 1
# De vakantie naar Normandië duurt 3 dagen korter dan die in een hotel
# n_nrm = n_htl - 3
result_rule1a <- result_fam |> filter(destination == "Normandië") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_dagen)
result_rule1b <- result_fam |> filter(accommodation == "hotel") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}")) - 3L) |> select(combi_id, n_dagen)
result_rule1c <- result_rule1a |> inner_join(result_rule1b, by = join_by(combi_id, n_dagen))  
result_fam1 <- result_fam |> filter(combi_id %in% result_rule1c$combi_id)
rm(result_rule1a, result_rule1b, result_rule1c)

# rule 2
# Van Dijk: huisje in eigen land en blijft 2 dagen langer weg dan de familie die met een tent naar het buitenland gaat
result_rule2a <- result_fam1 |> filter(family == "Van Dijk" & accommodation == "huisje" & bi_bu == "binnenland") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_dagen)
result_rule2b <- result_fam1 |> filter(accommodation == "tent" & bi_bu == "buitenland") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}")) + 2) |> select(combi_id, n_dagen)
result_rule2c <- result_rule2a |> inner_join(result_rule2b, by = join_by(combi_id, n_dagen))  
result_fam2 <- result_fam1 |> filter(combi_id %in% result_rule2c$combi_id)
rm(result_rule2a, result_rule2b, result_rule2c)

# rule 3
# De familie die naar Nice gaat, niet De Vries, blijft langer weg dan de families die naar een b&b en hotel gaan
result_rule3a <- result_fam2 |> filter(destination == "Nice" & family != "de Vries") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_dagen)
result_rule3b <- result_fam2 |> filter(accommodation %in% c("B&B", "hotel")) |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_dagen)
result_rule3c <- result_rule3a |> inner_join(result_rule3b, by = join_by(combi_id)) |> group_by(combi_id) |> 
  mutate(n_dagen_y_max = max(n_dagen.y)) |> filter(n_dagen.x > n_dagen_y_max)
result_fam3 <- result_fam2 |> filter(combi_id %in% result_rule3c$combi_id)
rm(result_rule3a, result_rule3b, result_rule3c)

# rule 4
# De Jong gaat nooit naar het buitenland of een B&B. Zij gaan langer dan De Boer en korter dan De Vries
result_rule4a <- result_fam3 |> filter(family == "De Jong" & (bi_bu == "buitenland" | accommodation == "B&B")) |> 
  select(combi_id)
result_fam4a <- result_fam3 |> anti_join(result_rule4a, by = join_by(combi_id))

result_rule4b <- result_fam4a |> filter(family == "De Jong") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_de_jong = n_dagen)
result_rule4c <- result_fam4a |> filter(family == "De Boer") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_de_boer = n_dagen)
result_rule4d <- result_fam4a |> filter(family == "de Vries") |> 
  mutate(n_dagen = as.integer(str_extract(duration, "^\\d{1,2}"))) |> select(combi_id, n_de_vries = n_dagen)
result_rule4e <- result_rule4b |> 
  inner_join(result_rule4c, by = join_by(combi_id)) |>
  inner_join(result_rule4d, by = join_by(combi_id)) |> 
  filter(n_de_jong > n_de_boer & n_de_jong < n_de_vries)
result_fam4 <- result_fam4a |> filter(combi_id %in% result_rule4e$combi_id)
rm(result_rule4a, result_rule4b, result_rule4c, result_rule4d, result_rule4e, result_fam4a)

