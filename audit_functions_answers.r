# Materia: Contabilidad y Auditoría de Seguros
# Caso 3
# Universidad La Salle México
# Profesor: José Enrique Pérez Salvador

apv_benefit <- function(id_pol, lt, i, x, prod, nyears, ben_amt, t) {
  # Purpose: Calculate the actuarial present value of the benefit for each policy # nolint
  # Arguments:
  #   id_pol: vector of policy IDs
  #   lt: life table
  #   i: interest rate
  #   x: vector of ages at the beginning of the policy
  #   prod: vector of product types
  #   nyears: vector of number of years for the policy (NA for whole life)
  #   ben_amt: vector of benefit amounts
  #   t: vector of time since the start of the policy at the base date
  # Recycle shorter vectors if needed
  n <- length(id_pol)
  t <- rep_len(t, n)
  # Force of interest
  d <- log(1 + i)
  # Outputs
  apv_ben <- rep_len(0, n)
  prod1 <- prod == "h-payment years, whole life insurance"
  prod2 <- prod == "n-year term insurance"
  prod3 <- prod == "n-year endowment insurance"
  # Update the term
  n <- nyears - t
  # Calculate the actuarial present value of the benefit for each product type
  if (any(prod1)) {
    apv_ben[prod1] <- ben_amt[prod1] * i / d * Axn(lt, x = x[prod1] + t[prod1], i = i, type = "EV") # nolint
  }
  pos_tmp2 <- (n > 0) & prod2
  if (any(pos_tmp2)) {
    apv_ben[pos_tmp2] <- ben_amt[pos_tmp2] * i / d * Axn(lt, x = x[pos_tmp2] + t[pos_tmp2], n = n[pos_tmp2], i = i, type = "EV") # nolint
  }
  pos_tmp3 <- (n > 0) & prod3
  if (any(pos_tmp3)) {
    apv_ben[pos_tmp3] <- ben_amt[pos_tmp3] * ( i / d * Axn(lt, x = x[pos_tmp3] + t[pos_tmp3], n = n[pos_tmp3], i = i, type = "EV") + pxt(lt, x = x[pos_tmp3] + t[pos_tmp3], t = n[pos_tmp3], fractional = "linear") * (1+i)^(-n[pos_tmp3])) # nolint
  }
  # Output
  data.frame(id_pol, apv_ben)
}

apv_annuity <- function(id_pol, lt, i, x, prod, nyears, t) {
  # Purpose: Calculate the actuarial present value of the annuity for each policy # nolint
  # Arguments:
  #   id_pol: vector of policy IDs
  #   lt: life table
  #   i: interest rate
  #   x: vector of ages at the beginning of the policy
  #   prod: vector of product types
  #   nyears: vector of number of years for the policy (NA for whole life)
  #   t: vector of time since the start of the policy at the base date
  # Recycle shorter vectors if needed
  n <- length(id_pol)
  t <- rep_len(t, n)
  # Outputs
  apv_ann <- rep_len(0, n)
  prod1 <- prod == "h-payment years, whole life insurance"
  prod2 <- prod == "n-year term insurance"
  prod3 <- prod == "n-year endowment insurance"
  # Update the term
  n <- nyears - t
  # Calculate the actuarial present value of the annuity for each product type
  if (any(prod1)) {
    apv_ann[prod1] <- axn(lt, x = x[prod1], i = i, n = 65 - x[prod1], type = "EV", k = 12, payment = "advance") # nolint
  }
  pos_tmp2 <- (n > 0) & prod2
  if (any(pos_tmp2)) {
    apv_ann[pos_tmp2] <- axn(lt, x = x[pos_tmp2] + t[pos_tmp2], n = n[pos_tmp2], i = i, type = "EV", k = 12, payment = "advance") # nolint
  }
  pos_tmp3 <- (n > 0) & prod3
  if (any(pos_tmp3)) {
    apv_ann[pos_tmp3] <- axn(lt, x = x[pos_tmp3] + t[pos_tmp3], n = n[pos_tmp3], i = i, type = "EV", k = 12, payment = "advance") # nolint
  }
  # Output
  data.frame(id_pol, apv_ann)
}

sim_pv_benefit <- function(i, prod, nyears, ben_amt, t, sim) {
  # Purpose: Calculate the present value of the benefit for each policy using the simulated future lifetime # nolint
  # Arguments:
  #   i: interest rate
  #   prod: vector of product types
  #   nyears: vector of number of years for the policy (original term, NA for whole life)
  #   ben_amt: vector of benefit amounts
  #   t: vector of time since the start of the policy at the base date
  #   sim: matrix of simulated future lifetimes (every column is a person and every row is a simulation)
  n <- length(id_pol)
  t <- rep_len(t, n)
  # Outputs
  sim_pv_ben <- matrix(0, nrow = n, ncol = ncol(sim))
  prod1 <- prod == "h-payment years, whole life insurance"
  prod2 <- prod == "n-year term insurance"
  prod3 <- prod == "n-year endowment insurance"
  # Update the term
  n <- nyears - t
  # Calculate the actuarial present value of the benefit for each product type
  if (any(prod1)) {
    Tx1 <- sim[prod1,]
    sim_pv_ben[prod1,] <- ben_amt[prod1] * (1 + i)^(-Tx1)
  }
  pos_tmp2 <- (n > 0) & prod2
  if (any(pos_tmp2)) {
    Tx2 <- sim[pos_tmp2, ]
    sim_pv_ben[pos_tmp2,] <- ben_amt[pos_tmp2] * (1 + i)^(-Tx2) * ifelse(n[pos_tmp2] >= Tx2, 1, 0) # nolint
  }
  pos_tmp3 <- (n > 0) & prod3
  if (any(pos_tmp3)) {
    # apv_ben[pos_tmp3] <- ben_amt[pos_tmp3] * ( i / d * Axn(lt, x = x[pos_tmp3] + t[pos_tmp3], n = n[pos_tmp3], i = i, type = "EV") + pxt(lt, x = x[pos_tmp3] + t[pos_tmp3], t = n[pos_tmp3], fractional = "linear") * (1+i)^(-n[pos_tmp3])) # nolint
    Tx3 <- sim[pos_tmp3, ]
    sim_pv_ben[pos_tmp3,] <- ben_amt[pos_tmp3] * (1 + i)^(-pmin(n[pos_tmp3], Tx3)) # nolint
  }
  # Output
  sim_pv_ben
}


sim_pv_annuity <- function(i, x, prod, nyears, t, sim) {
  # Purpose: Calculate the actuarial present value of the annuity for each policy # nolint
  # Arguments:
  #   i: interest rate
  #   x: vector of ages 
  #   prod: vector of product types
  #   nyears: vector of number of years for the policy (original term, NA for whole life)
  #   t: vector of time since the start of the policy at the base date
  #   sim: matrix of simulated future lifetimes (every column is a person and every row is a simulation)
  # Recycle shorter vectors if needed
  n <- length(id_pol)
  t <- rep_len(t, n)
  # Discount rate convertible m times per year
  d12 <- -12 * ((1 + i)^(-1 / 12) - 1)
  # Outputs
  sim_pv_ann <- matrix(0, nrow = n, ncol = ncol(sim))
  prod1 <- prod == "h-payment years, whole life insurance"
  prod2 <- prod == "n-year term insurance"
  prod3 <- prod == "n-year endowment insurance"
  # Update the term
  n <- nyears - t
  # Calculate the actuarial present value of the annuity for each product type
  if (any(prod1)) {
    Tx1 <- sim[prod1, ]
    sim_pv_ann[prod1, ] <- (1-(1 + i)^(-pmin(65 - x[prod1], floor(Tx1)))) / d12 # nolint
  }
  pos_tmp2 <- (n > 0) & prod2
  if (any(pos_tmp2)) {
    Tx2 <- sim[pos_tmp2, ]
    sim_pv_ann[pos_tmp2, ] <- (1-(1 + i)^(-pmin(n[pos_tmp2], floor(Tx2)))) / d12 # nolint
  }
  pos_tmp3 <- (n > 0) & prod3
  if (any(pos_tmp3)) {
    Tx3 <- sim[pos_tmp3, ]
    sim_pv_ann[pos_tmp3, ] <- (1-(1 + i)^(-pmin(n[pos_tmp3], floor(Tx3)))) / d12 # nolint
  }
  # Output
  sim_pv_ann
}
