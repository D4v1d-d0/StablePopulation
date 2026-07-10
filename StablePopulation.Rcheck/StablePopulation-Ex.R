pkgname <- "StablePopulation"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "StablePopulation-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('StablePopulation')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("alpha_objective")
### * alpha_objective

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: alpha_objective
### Title: Objective Function for 'uniroot': Finds the Difference Between
###   Births and 1
### Aliases: alpha_objective

### ** Examples

# Basic usage
alpha_objective(0.5, 1.2, c(0.2, 0.3, 0.5, 0.4))

# Example with uniroot:
fertility_rates <- c(0.2, 0.3, 0.5, 0.4)
beta <- 1.2
res <- uniroot(
  alpha_objective,
  interval = c(0.000001, 100),
  beta = beta,
  fertility_rates = fertility_rates
)
res$root



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("alpha_objective", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("calculate_population")
### * calculate_population

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: calculate_population
### Title: Calculates the population for each age group
### Aliases: calculate_population

### ** Examples

calculate_population(0.5, 1.2, c(0.2, 0.3, 0.5, 0.4))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("calculate_population", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("derive_demographic_profile")
### * derive_demographic_profile

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: derive_demographic_profile
### Title: Derive stable-structure and mortality profiles from survivorship
### Aliases: derive_demographic_profile

### ** Examples

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
reconstruction <- reconstruct_population(mx, beta = 1.10)

demographic_profile <- derive_demographic_profile(
  lx = reconstruction$lx,
  fertility_rates = mx
)

demographic_profile$table




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("derive_demographic_profile", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("find_alphas")
### * find_alphas

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: find_alphas
### Title: Function to find the value of alpha
### Aliases: find_alphas

### ** Examples

find_alphas(1.2, c(0.2, 0.3, 0.5, 0.4))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("find_alphas", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit_weibull_free")
### * fit_weibull_free

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit_weibull_free
### Title: Fit a free two-parameter Weibull survivorship model
### Aliases: fit_weibull_free

### ** Examples

lx_observed <- c(1, 0.92, 0.78, 0.57, 0.33, 0.12)
free_fit <- fit_weibull_free(lx_observed)
free_fit$alpha
free_fit$beta




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit_weibull_free", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("normalize_fertility")
### * normalize_fertility

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: normalize_fertility
### Title: Normalize fertility using a reference survivorship profile
### Aliases: normalize_fertility

### ** Examples

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
lx <- c(1, 0.90, 0.75, 0.55, 0.30, 0.10)
normalized <- normalize_fertility(mx, lx)
normalized$check_R0




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("normalize_fertility", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("reconstruct_population")
### * reconstruct_population

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: reconstruct_population
### Title: Reconstruct one constrained Weibull survivorship profile
### Aliases: reconstruct_population

### ** Examples

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
reconstruction <- reconstruct_population(mx, beta = 1.10)
reconstruction$alpha
reconstruction$table




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("reconstruct_population", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("run_reconstruction_excel")
### * run_reconstruction_excel

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: run_reconstruction_excel
### Title: Reconstruct stable-population profiles from an Excel workbook
### Aliases: run_reconstruction_excel

### ** Examples

## Not run: 
##D # Opens a file chooser in an interactive R session and creates the
##D # standard output workbook.
##D if (interactive()) {
##D   run_reconstruction_excel()
##D }
##D 
##D # Creates demography_StablePopulation.xlsx beside demography.xlsx.
##D run_reconstruction_excel("demography.xlsx")
##D 
##D # Request technical candidate and profile diagnostics.
##D run_reconstruction_excel("demography.xlsx", output_detail = "full")
##D 
##D # Use explicit column names when an input workbook has custom headings.
##D run_reconstruction_excel(
##D   input_file = "demography.xlsx",
##D   output_file = "results.xlsx",
##D   sheets = "Ovis_dalli",
##D   age_column = "Age class",
##D   fertility_column = "Female fertility",
##D   survivorship_column = "Observed survivorship",
##D   beta_column = "Beta"
##D )
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("run_reconstruction_excel", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("scan_beta")
### * scan_beta

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: scan_beta
### Title: Scan Weibull beta values under the stable-population constraint
### Aliases: scan_beta

### ** Examples

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)

# All stable candidate profiles
scan <- scan_beta(mx, beta_values = seq(0.05, 1.50, by = 0.05))

# Scenario route without observed lx
terminal_scan <- scan_beta(
  mx,
  beta_values = seq(0.05, 1.50, by = 0.05),
  terminal_window = c(1e-4, 0.05)
)
terminal_scan$admissible_summary




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("scan_beta", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("select_beta")
### * select_beta

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: select_beta
### Title: Select a constrained Weibull beta using observed survivorship
### Aliases: select_beta

### ** Examples

mx <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
lx_observed <- c(1, 0.8302461, 0.6893086, 0.5722958, 0.4751464, 0.3944885)

selection <- select_beta(mx, lx_observed)
selection$best_beta
selection$best_profile




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("select_beta", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("weibull_survival")
### * weibull_survival

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: weibull_survival
### Title: Weibull function for the survival rate
### Aliases: weibull_survival

### ** Examples

weibull_survival(1.5, 0.8, 10)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("weibull_survival", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
