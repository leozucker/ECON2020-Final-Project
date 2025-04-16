library(dtp)
library(momentfit)
library(pgmm)

# Create five-year averages of all variables in WDI
WDI_5yr_avg <- WDI %>%
  # Create a five-year period variable
  mutate(period = 5 * floor(year / 5)) %>%  # Creates 1960, 1965, etc.
  # Group by country and period
  group_by(country, period) %>%
  # Calculate means for all numeric variables
  summarize(across(where(is.numeric), ~mean(., na.rm = FALSE)))

WDI_5yr_avg$ln_gdp_pc <- log(WDI_5yr_avg$gdp_pc) # Recode log GDP per capita since averages of logs aren't interesting

WDI_5yr_avg <- WDI_5yr_avg |> # Generate a lead of the dependent variable
    arrange(country, period) |> 
    group_by(country) |> 
    mutate(LEAD_ln_gdp_pc = lead(ln_gdp_pc)) |> 
    ungroup()
WDI_5yr_avg$ln_life_exp <- log(WDI_5yr_avg$life_exp)
WDI_5yr_avg$ln_energy_pc <- log(WDI_5yr_avg$energy_pc)



sgmm_model <- pgmm(
  formula = LEAD_ln_gdp_pc ~ ln_life_exp + 
            educ_prim + educ_sec + educ_bach + 
            pop_0_14_pct + pop_65_plus_pct + pop_growth + 
            ln_energy_pc + rural_pct + tourism_pct + manuf_export | 
            lag(ln_gdp_pc, 2:99),  # Instruments
  data = WDI_5yr_avg,
  effect = "twoways",      # Both individual and time effects
  model = "twosteps",      # Two-step estimation
  transformation = "ld",   # First-difference transformation
  index = c("country", "period")  # Panel identifiers
)




# System GMM estimation for your full model
sgmm_model <- gmmFit(
  model = sysModel(LEAD_ln_gdp_pc ~ ln_life_exp + 
                  educ_prim + educ_sec + educ_bach + 
                  pop_0_14_pct + pop_65_plus_pct + pop_growth + 
                  ln_energy_pc + rural_pct + tourism_pct + manuf_export, 
                  data = WDI_5yr_avg),
  type = "twostep",              # Two-step estimation (more efficient)
  itertol = 1e-4,                # Convergence tolerance
  initW = "ident",               # Initial weighting matrix
  weights = "optimal",           # Use optimal weighting matrix
  itermaxit = 50,                # Maximum iterations
  efficientWeights = TRUE,       # Compute efficient weights 
  vcov = "robust"                # Use robust standard errors
)



gmmFit(model = sysModel(LEAD_ln_gdp_pc ~ ln_life_exp + 
                        pop_0_14_pct + pop_65_plus_pct + pop_growth, 
                        data = WDI_5yr_avg), 
        type = "onestep", itertol = 1e-4, initW = "ident", weights = "optimal", 
        itermaxit = 20, efficientWeights = FALSE)

dtp(LEAD_ln_gdp_pc ~ ln_life_exp + 
                        pop_0_14_pct + pop_65_plus_pct + pop_growth
                        | ln_gdp_pc, 
    WDI_5yr_avg, index = c("country","period"), 3, 2, 0.95, 0.8, 1, graph = TRUE)



dtp(LEAD_ln_gdp_pc ~ ln_life_exp + 
                        educ_prim + educ_sec + educ_bach + 
                        pop_0_14_pct + pop_65_plus_pct + pop_growth + 
                        ln_energy_pc + rural_pct + tourism_pct + manuf_export 
                        | ln_gdp_pc, 
  data = WDI_5yr_avg,                # Your dataset
  index = c("country", "period"),    # Panel identifiers
  maxlags = 3,
  initnum = 2,
  conf = .95,
  conf1 = .95,
  conf2 = .95,
  graph = TRUE                    # Show threshold confidence interval plot
)
