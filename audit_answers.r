# Materia: Contabilidad y Auditoría de Seguros
# Caso 3
# Universidad La Salle México
# Profesor: José Enrique Pérez Salvador

start <- proc.time()

# Download and load the required packages
options(repos = c(CRAN = "https://cloud.r-project.org/"))

if (!require("lifecontingencies", quietly = TRUE)) {
  install.packages("lifecontingencies")
  library("lifecontingencies")
}

if (!require("this.path", quietly = TRUE)) {
  install.packages("this.path")
  library("this.path")
}

if (!require("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
  library("dplyr")
}

if (!require("readxl", quietly = TRUE)) {
  install.packages("readxl")
  library("readxl")
}

if (!require("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
  library("lubridate")
}




# Path of this file and the input files
path <- dirname(this.path::this.path())

# Load the customized functions
source(paste0(path, "/audit_functions_answers.R"))



# Read the mortality table
# Male
e_m <- read.csv(paste0(path, "/Export_m.csv"), skip = 23)
colnames(e_m) <- c("x", "qx")
# Female
e_f <- read.csv(paste0(path, "/Export_f.csv"), skip = 23)
colnames(e_f) <- c("x", "qx")
# Read the parameters of the valuation
params <- read_excel(paste0(path, "/params.xlsx"))
# Read the Excel file
people <- read_excel(paste0(path, "/20251231_policies.xlsx"), sheet = "people")
pol <- read_excel(paste0(path, "/20251231_policies.xlsx"), sheet = "policies") # nolint
cats <- read_excel(paste0(path, "/20251231_policies.xlsx"), sheet = "cats")

# Create the actuarial table
# Male
omega_m <- max(e_m$x)
x <- as.data.frame(0:omega_m)
colnames(x) <- "x"
e2_m <- left_join(x, e_m, by = c("x")) %>% mutate_if(is.numeric, coalesce, 0)
name_lt_m <- as.character(params[params$gender == "m", c("name_lt")])
radix_m <- as.integer(params[params$gender == "m", c("radix")])
lt_m <- probs2lifetable(pmin(1.05*e2_m$qx, 1), radix = radix_m, type = "qx", name = name_lt_m) # nolint
# Female
omega_f <- max(e_f$x)
x <- as.data.frame(0:omega_f)
colnames(x) <- "x"
e2_f <- left_join(x, e_f, by = c("x")) %>% mutate_if(is.numeric, coalesce, 0)
name_lt_f <- as.character(params[params$gender == "f", c("name_lt")])
radix_f <- as.integer(params[params$gender == "f", c("radix")])
lt_f <- probs2lifetable(pmin(1.05*e2_f$qx, 1), radix = radix_f, type = "qx", name = name_lt_f) # nolint

# Create the data frame with all the information of the policies
pol2 <- pol %>% left_join(people, by = "id_person") %>% left_join(params %>% select(int_rate,base_date,gender), by = "gender") # nolint
# Age of the insured at the start of the policy
pol2$x <- time_length(interval(pol2$birthday,pol2$start_date), "years") %>% floor() # nolint
# Age of the insured at the base date
pol2$x_base <- time_length(interval(pol2$birthday,pol2$base_date), "years") %>% floor() # nolint
# Maturity date of the policy
pol2$maturity_date <- pol2$start_date + years(pol2$`n-year`)
# Time since the start of the policy at the base date
pol2$t <- time_length(interval(pol2$start_date,pol2$base_date), "years") %>% floor() # nolint

# Premiums
i <- unique(pol2$int_rate)
# Male
id_pol_m <- pol2$id_policy[pol2$gender == "m"]
x_m <- pol2$x[pol2$gender == "m"]
prod_m <- pol2$product[pol2$gender == "m"]
nyears_m <- pol2$`n-year`[pol2$gender == "m"]
# Female
id_pol_f <- pol2$id_policy[pol2$gender == "f"]
x_f <- pol2$x[pol2$gender == "f"]
prod_f <- pol2$product[pol2$gender == "f"]
nyears_f <- pol2$`n-year`[pol2$gender == "f"]

# Actuarial present value of the benefit
# Male
ben_amt_m <- pol2$`benefit_amount`[pol2$gender == "m"]
apv_ben_m <- apv_benefit(id_pol = id_pol_m, lt = lt_m,
                         i = i, x = x_m, prod = prod_m,
                         nyears = nyears_m, ben_amt = ben_amt_m, t = 0) # nolint
# Female
ben_amt_f <- pol2$`benefit_amount`[pol2$gender == "f"]
apv_ben_f <- apv_benefit(id_pol = id_pol_f, lt = lt_f,
                         i = i, x = x_f, prod = prod_f,
                         nyears = nyears_f, ben_amt = ben_amt_f, t = 0) # nolint
apv_ben_0 <- rbind(apv_ben_m, apv_ben_f)
colnames(apv_ben_0) <- c("id_policy", "apv_ben_0")
pol2 <- pol2 %>% left_join(apv_ben_0, by = c("id_policy")) # nolint

# Unit annuity to pay the benefit
# Male
apv_ann_m <- apv_annuity(id_pol = id_pol_m, lt = lt_m,
                         i = i, x = x_m, prod = prod_m,
                         nyears = nyears_m, t = 0) # nolint
# Female
apv_ann_f <- apv_annuity(id_pol = id_pol_f, lt = lt_f,
                         i = i, x = x_f, prod = prod_f,
                         nyears = nyears_f, t = 0) # nolint
apv_ann_0 <- rbind(apv_ann_m, apv_ann_f)
colnames(apv_ann_0) <- c("id_policy", "apv_ann_0")
pol2 <- pol2 %>% left_join(apv_ann_0, by = c("id_policy")) # nolint

pol2$pure_premium <- pol2$apv_ben_0 / pol2$apv_ann_0
pol2$gross_premium <- round(pol2$pure_premium / 0.75, 2)

# Reserves
# Actuarial present value of the benefit at time t
# Male
t_m <- pol2$t[pol2$gender == "m"]
apv_ben_m_t <- apv_benefit(id_pol = id_pol_m, lt = lt_m,
                           i = i, x = x_m, prod = prod_m,
                           nyears = nyears_m, ben_amt = ben_amt_m, t = t_m) # nolint
# Female
t_f <- pol2$t[pol2$gender == "f"]
apv_ben_f_t <- apv_benefit(id_pol = id_pol_f, lt = lt_f,
                           i = i, x = x_f, prod = prod_f,
                           nyears = nyears_f, ben_amt = ben_amt_f, t = t_f) # nolint
# Combine the results
apv_ben_t <- rbind(apv_ben_m_t, apv_ben_f_t)
colnames(apv_ben_t) <- c("id_policy", "apv_ben_t")
pol2 <- pol2 %>% left_join(apv_ben_t, by = c("id_policy")) # nolint
# Actuarial present value of the 1-unit annuity at time t
# Male
apv_ann_m_t <- apv_annuity(id_pol = id_pol_m, lt = lt_m,
                           i = i, x = x_m, prod = prod_m,
                           nyears = nyears_m, t = t_m) # nolint
# Female
apv_ann_f_t <- apv_annuity(id_pol = id_pol_f, lt = lt_f,
                           i = i, x = x_f, prod = prod_f,
                           nyears = nyears_f, t = t_f) # nolint
# Combine the results
apv_ann_t <- rbind(apv_ann_m_t, apv_ann_f_t)
colnames(apv_ann_t) <- c("id_policy", "apv_ann_t")
pol2 <- pol2 %>% left_join(apv_ann_t, by = c("id_policy")) # nolint

# Reserve at time t
pol2$res_t <- pol2$apv_ben_t + pol2$apv_ann_t * 0.25 * pol2$gross_premium - pol2$apv_ann_t * pol2$gross_premium # nolint
# Summary of the reserves by product
sum_res <- aggregate(res_t ~ product, data = pol2, FUN = sum)
tot_res <- sum(pol2$res_t)

# Using the mortality experience of the company to calculate the reserves

# Read the Experience mortality table
exp <- read_excel(paste0(path, "/20251231_res_prem_exp.xlsx"), sheet = "Experience", skip = 4) # nolint
colnames(exp) <- c("x", "qx")
# Create the actuarial table
omega <- max(exp$x)
x <- as.data.frame(0:omega)
colnames(x) <- "x"
exp2 <- left_join(x, exp, by = c("x")) %>% mutate_if(is.numeric, coalesce, 0)
name_lt <- "Experience of Vida Plena"
radix <- as.integer(params[params$gender == "m", c("radix")])
lt_exp <- probs2lifetable(pmin(exp2$qx, 1), radix = radix, type = "qx", name = name_lt) # nolint
# Reserves
# Actuarial present value of the benefit at time t
t <- pol2$t
id_pol <- pol2$id_policy
x <- pol2$x
prod <- pol2$product
nyears <- pol2$`n-year`
ben_amt <- pol2$`benefit_amount`
apv_ben_exp_t <- apv_benefit(id_pol = id_pol, lt = lt_exp,
                             i = i, x = x, prod = prod,
                             nyears = nyears, ben_amt = ben_amt, t = t) # nolint
colnames(apv_ben_exp_t) <- c("id_policy", "apv_ben_exp_t")
pol2 <- pol2 %>% left_join(apv_ben_exp_t, by = c("id_policy")) # nolint
# Actuarial present value of the 1-unit annuity at time t
apv_ann_exp_t <- apv_annuity(id_pol = id_pol, lt = lt_exp,
                             i = i, x = x, prod = prod,
                             nyears = nyears, t = t) # nolint
colnames(apv_ann_exp_t) <- c("id_policy", "apv_ann_exp_t")
pol2 <- pol2 %>% left_join(apv_ann_exp_t, by = c("id_policy")) # nolint

# Reserve at time t
pol2$res_exp_t <- pol2$apv_ben_exp_t + pol2$apv_ann_exp_t * 0.25 * pol2$gross_premium - pol2$apv_ann_exp_t * pol2$gross_premium # nolint
# Summary of the reserves by product
sum_res_exp <- aggregate(res_exp_t ~ product, data = pol2, FUN = sum)
tot_res_exp <- sum(pol2$res_exp_t)

# Write the results in a csv file
write.table(pol2, file = paste0(path, "/pol2.csv"), sep = ",", row.names = FALSE, col.names = TRUE, quote=TRUE) # nolint


# Sufficiency of the reserves
# Simulations
# Number of simulations
n_sim <- 601000
# Simulate the future lifetime of the insured at time t using the Experience mortality table # nolint
# x_base is the age of the insured at the base date, which is the starting point of the simulation # nolint
# Every row is a policyholder and every column is a simulation # nolint
# It is simulated the future lifetime of the insured at time t (the age at the base date) # nolint
sim <- t(mapply(function(x) rLife(n_sim, lt_exp, x, k = 1, type = "Tx"), pol2$x_base)) # nolint
# Simulation of the present value of the benefit at time t using the simulated future lifetime # nolint
sim_pv_ben_t <- sim_pv_benefit(i = i, prod = prod, nyears = nyears, ben_amt = ben_amt, t =t, sim = sim) # nolint
# Simulation of the present value of the annuity at time t using the simulated future lifetime # nolint
sim_pv_ann_t <- sim_pv_annuity(i = i, x = pol2$x_base, prod = prod, nyears = nyears, t = t, sim = sim) # nolint
# Simulation of the present value of the losses at time t using the simulated future lifetime # nolint
sim_pv_loss_t <- sim_pv_ben_t + sim_pv_ann_t * 0.25 * pol2$gross_premium - sim_pv_ann_t * pol2$gross_premium # nolint
sim_pv_totloss_t <- colSums(sim_pv_loss_t)
hist(sim_pv_totloss_t, breaks = 50, main = "Distribution of the present value of the total losses at time t", xlab = "Total losses at time t") # nolint
# Probability of lossing more than the total reserves
count <- sum(sim_pv_totloss_t > tot_res)
prob_loss <- count / n_sim
print(paste0("Probability of losing more than the total reserves: ", round(prob_loss, 6)))

# Write the results in a csv file
write.table(sim_pv_totloss_t, file = paste0(path, "/sim_pv_totloss_t.csv"), sep = ",", row.names = FALSE, col.names = TRUE, quote=TRUE) # nolint

# Delete the objects that are not needed
rm(sim, sim_pv_ben_t, sim_pv_ann_t, sim_pv_loss_t)
gc()


# End timing
end <- proc.time()
print(end - start)
