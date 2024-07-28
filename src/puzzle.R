pacman::p_load(gtools, dplyr, tidyr)

fam_labels <- c(fam_1 = "De Boer", fam_2 = "Van Dijk", fam_3 = "Jansen", fam_4 = "De Jong", fam_5 = "De Vries")
duur_labels <- c("8", "10", "11", "13", "14")
acc_labels <- c("appartement", "b&b", "hotel", "huisje", "tent")
best_labels <- c("Haamstede", "Nice", "Noordwijk", "Normandië", "Schoorl")
in_NL <- c(T, F, T, F, T)
bi_bu_labels <- if_else(in_NL, "binnenland", "buitenland")

combineer <- function(pm_duur, pm_acc, pm_best) {
  
  cur_duur <- pm_duur |> 
    pivot_longer(everything(), names_to = "familie", values_to = "index") |> 
    mutate(n_dagen = duur_labels[index],
           familie = fam_labels[familie]) |> 
    select(familie, n_dagen)
  
  cur_acc <- pm_acc |> 
    pivot_longer(everything(), names_to = "familie", values_to = "index") |> 
    mutate(accomodatie = acc_labels[index],
           familie = fam_labels[familie]) |> 
    select(familie, accomodatie)
  
  cur_best <- pm_best |> 
    pivot_longer(everything(), names_to = "familie", values_to = "index") |> 
    mutate(bestemming = best_labels[index],
           familie = fam_labels[familie],
           bi_bu = bi_bu_labels[index]) |> 
    select(familie, bestemming, bi_bu)
  
  cur_duur |> inner_join(cur_acc, by = join_by("familie")) |> 
    inner_join(cur_best, by = join_by("familie")) |> 
    select(n_dagen, accomodatie, familie, bestemming, bi_bu)
}

gen_perms <- function() {
  families <- 1:5
  perms <- permutations(n = length(families), r = 5, v = families) |> as_tibble(rownames = NA)
  names(perms) <- paste0("fam_", 1:5)
  return(perms)
}

duren <- gen_perms()
accomodaties <- gen_perms()
bestemmingen <- gen_perms()

n_perms <- nrow(duren)

for (p1 in 1:n_perms) {
  duur <- duren[p1,]
  
  for (p2 in 1:n_perms) {
    accomodatie <- accomodaties[p2,]
    
    for (p3 in 1:n_perms) {
      bestemming <- bestemmingen[p3,]  
      combi <- combineer(pm_duur = duur, pm_acc = accomodatie, pm_best = bestemming)
      # resultaat <- beoordeel(combi)
    }
  }
}
