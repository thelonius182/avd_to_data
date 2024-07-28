pacman::p_load(gtools, dplyr, tidyr)

fam_labels <- c(fam_1 = "De Boer", fam_2 = "Van Dijk", fam_3 = "Jansen", fam_4 = "De Jong", fam_5 = "De Vries")
duur_labels <- c("8", "10", "11", "13", "14")
acc_labels <- c("appartement", "b&b", "hotel", "huisje", "tent")
best_labels <- c("Haamstede", "Nice", "Noordwijk", "Normandië", "Schoorl")
in_NL <- c(T, F, T, F, T)
bi_bu_labels <- if_else(in_NL, "binnenland", "buitenland")

gen_perms <- function() {
  families <- 1:5
  perms <- permutations(n = length(families), r = 5, v = families) |> as_tibble(.name_repair = "unique", rownames = NA)
  names(perms) <- paste0("fam_", 1:5)
  return(perms |> as_tibble())
}

duren <- gen_perms()
accomodaties <- gen_perms()
bestemmingen <- gen_perms()

n_perms <- nrow(duren)
t1 <- tibble(onderdeel = character(),
             fam_1 = integer(),
             fam_2 = integer(),
             fam_3 = integer(),
             fam_4 = integer(),
             fam_5 = integer())
c1 <- 0L

for (p1 in 1:n_perms) {
  duur <- duren[p1,]
  d1 <- duur |> mutate(onderdeel = "duur")
  
  for (p2 in 1:n_perms) {
    accomodatie <- accomodaties[p2,]
    a1 <- accomodatie |> mutate(onderdeel = "accomodatie")
    
    for (p3 in 1:n_perms) {
      bestemming <- bestemmingen[p3,]
      b1 <- bestemming |> mutate(onderdeel = "bestemming")
      c1 <- c1 + 1L
      dab1 <- d1 |> bind_rows(a1) |> bind_rows(b1) |> mutate(conbi = c1)
      t1 <- t1 |> bind_rows(dab1)
    }
  }
}
