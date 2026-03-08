# Materia: Cálculo Actuarial para Personas
# Examen Casero 1
# Universidad La Salle México
# Profesor: José Enrique Pérez Salvador

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

# Path of this file and the input files
path <- dirname(this.path::this.path())
# Read the mortality table
# Male
e_m <- read.csv(paste0(path, "/Export_m.csv"), skip = 23)
colnames(e_m) <- c("x", "qx")
# Female
e_f <- read.csv(paste0(path, "/Export_f.csv"), skip = 23)
colnames(e_f) <- c("x", "qx")
# Read the parameters of the valuation
params <- read.csv(paste0(path, "/params.csv"))
# Create the actuarial table
# Male
omega_m <- max(e_m$x)
x <- as.data.frame(0:omega_m)
colnames(x) <- "x"
e2_m <- left_join(x, e_m, by = c("x")) %>% mutate_if(is.numeric, coalesce, 0)
name_lt_m <- params[params$gender == "m", c("name_lt")]
radix_m <- params[params$gender == "m", c("radix")]
lt_m <- probs2lifetable(e2_m$qx, radix = radix_m, type = "qx", name = name_lt_m) # nolint
# Female
omega_f <- max(e_f$x)
x <- as.data.frame(0:omega_f)
colnames(x) <- "x"
e2_f <- left_join(x, e_f, by = c("x")) %>% mutate_if(is.numeric, coalesce, 0)
name_lt_f <- params[params$gender == "f", c("name_lt")]
radix_f <- params[params$gender == "f", c("radix")]
lt_f <- probs2lifetable(e2_f$qx, radix = radix_f, type = "qx", name = name_lt_f) # nolint
