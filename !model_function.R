## variable number of classes ------------------------
run_capacity_planning2 <- function(params)
{
  library(ompr)
  library(ompr.roi)
  library(ROI.plugin.glpk)
  library(dplyr)
  
  L <- params$load_data$load_kWh
  eta_PV <- params$pv_data$electricity
  
  # check for matching lengths of L and eta_PV
  nb.T <- length(L)
  if (length(eta_PV) < nb.T) {
    eta_PV <- rep(eta_PV, length.out = nb.T)
  } else if (length(eta_PV) > nb.T) {
    eta_PV <- eta_PV[1:nb.T]
  }
  
  FD <- ceiling(nb.T/24)
  
  # Parameter aus params extrahieren
  technologies      <- params$technologies  # z.B. c("PV", "Battery", "Diesel")
  S                 <- params$S             # z.B. c(3, 4, 2) - Anzahl Größenklassen pro Technologie
  PV_Cap_y          <- params$PV_Cap_y
  B_Cap_y           <- params$B_Cap_y
  Grid_Cap_y        <- params$Grid_Cap_y
  PV_s_z            <- params$PV_s_z        # Vektor der Länge S[1]
  D_s_z             <- params$D_s_z         # Vektor der Länge S[3]
  B_s_z             <- params$B_s_z         # Vektor der Länge S[2]
  Cap_BC            <- params$Cap_BC
  c_rent_PV_z       <- params$c_rent_PV_z   # Vektor der Länge S[1]
  c_rent_B_z        <- params$c_rent_B_z    # Vektor der Länge S[2]
  c_rent_D_z        <- params$c_rent_D_z    # Vektor der Länge S[3]
  c_rent_AC_z       <- params$c_rent_AC_z
  rent_amount_PV_z <- params$rent_amount_PV_z # Vektor der Länge S[1]
  rent_amount_B_z  <- params$rent_amount_B_z  # Vektor der Länge S[2]
  rent_amount_D_z  <- params$rent_amount_D_z  # Vektor der Länge S[3]
  c_PV_y            <- params$c_PV_y
  c_grid            <- params$c_grid
  diesel_cost       <- params$c_fuel_D
  c_setup_PV_z      <- params$c_setup_PV_z
  c_setup_B_z       <- params$c_setup_B_z
  c_setup_D_z       <- params$c_setup_D_z
  c_setup_AC_z      <- params$c_setup_AC_z
  c_BC_discharge    <- params$c_BC_discharge
  eta_B             <- params$eta_B
  eta_D             <- params$eta_D
  eta_BC            <- params$eta_BC
  c_rate_charge     <- params$c_rate_charge
  c_rate_discharge  <- params$c_rate_discharge
  c_rate_charge_BC  <- params$c_rate_charge_BC
  c_rate_discharge_BC <- params$c_rate_discharge_BC
  AC_charge         <- params$AC_charge
  AC_discharge      <- params$AC_discharge
  SOC_0             <- params$SOC_0
  SOC_BC_0          <- ifelse( is.null(params$SOC_BC_0), 0.5 * Cap_BC, params$SOC_BC_0)  
  fuel_con_fix_Diesel <- params$fuel_con_fix_Diesel # Vektor der Länge S[3]
  fuel_con_var_Diesel <- params$fuel_con_var_Diesel
  M <- 1e9  # big-M for setup constraints
  
  # Indizes für Technologien ermitteln
  PV_idx <- which(technologies == "PV")
  Battery_idx <- which(technologies == "Battery")
  Diesel_idx <- which(technologies == "Diesel")
  
  model <- MIPModel() %>%
    # Separate Variablen für jede Technologie mit jeweiliger Anzahl Größenklassen
    add_variable(n_PV[s], s = 1:S[PV_idx], type = "integer", lb = 0) %>%
    add_variable(n_Battery[s], s = 1:S[Battery_idx], type = "integer", lb = 0) %>%
    add_variable(n_Diesel[s], s = 1:S[Diesel_idx], type = "integer", lb = 0) %>%
    
    # Energiefluss-Variablen
    add_variable(f_PV_gen_inst[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_PV_gen_rent[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_PV_load[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_PV_B[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_D_load[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_D_B[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_grid_load[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_grid_B[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_B_load[t], t = 1:nb.T, lb = 0) %>%
    add_variable(SOC[t], t = 0:nb.T, lb = 0) %>%
    add_variable(lambda[t], t = 1:nb.T, lb = 0, ub = 100) %>%
    add_variable(D_fuel_consumption[t], t = 1:nb.T, lb = 0) %>%
    
    # Kapazitäts-Variablen
    add_variable(PV_Cap_z, type = "continuous", lb = 0) %>%
    add_variable(B_Cap_z, type = "continuous", lb = 0) %>%
    add_variable(D_Cap_z, type = "continuous", lb = 0) %>%
    
    # Setup-Binärvariablen
    add_variable(delta_PV, type = "binary", lb = 0) %>%
    add_variable(delta_B, type = "binary") %>%
    add_variable(delta_D, type = "binary") %>%
    add_variable(delta_AC, type = "binary") %>%
    
    # Car Battery Variablen
    add_variable(f_PV_BC[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_grid_BC[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_D_BC[t], t = 1:nb.T, lb = 0) %>%
    add_variable(f_BC_load[t], t = 1:nb.T, lb = 0) %>%
    add_variable(SOC_BC[t], t = 0:nb.T, lb = 0) %>%
    add_variable(n_AC, type = "integer", lb = 0, ub = 100)
  
  # Kapazität der gemieteten Anlagen über die technologiespezifischen Größenklassen
  model <- model %>%
    add_constraint(
      PV_Cap_z == sum_expr(PV_s_z[s] * n_PV[s], s = 1:S[PV_idx])
    ) %>%
    add_constraint(
      B_Cap_z == sum_expr(B_s_z[s] * n_Battery[s], s = 1:S[Battery_idx])
    ) %>%
    add_constraint(
      D_Cap_z == sum_expr(D_s_z[s] * n_Diesel[s], s = 1:S[Diesel_idx])
    )
  
  # 0) Begrenzeung der Mietmengen pro Größenklasse
  model <- model %>%
    add_constraint(
      n_PV[s] <= rent_amount_PV_z[s],
      s = 1:S[PV_idx]
    ) %>%
    add_constraint(
      n_Battery[s] <= rent_amount_B_z[s],
      s = 1:S[Battery_idx]
    ) %>%
    add_constraint(
      n_Diesel[s] <= rent_amount_D_z[s],
      s = 1:S[Diesel_idx]
    )
  
  
  # 1) PV Kapazität für Strom aus PV an Last und Batterie (t)
  model <- model %>%
    add_constraint(
      f_PV_gen_inst[t] <= eta_PV[t] * PV_Cap_y,
      t = 1:nb.T
    ) %>%
    add_constraint(
      f_PV_gen_rent[t] <= eta_PV[t] * sum_expr(PV_s_z[s] * n_PV[s], s = 1:S[PV_idx]),
      t = 1:nb.T
    ) %>%
    add_constraint(
      f_PV_load[t] + f_PV_B[t] + f_PV_BC[t] == f_PV_gen_inst[t] + f_PV_gen_rent[t],
      t = 1:nb.T
    )
  
  # 2) Dieselaggregat Kapazität (t)
  model <- model %>%
    add_constraint(
      f_D_load[t] + f_D_B[t] + f_D_BC[t] <=
        eta_D * sum_expr(D_s_z[s] * n_Diesel[s], s = 1:S[Diesel_idx]),
      t = 1:nb.T
    )
  
  # 3) Netzbezug Kapazität
  model <- model %>%
    add_constraint(
      f_grid_load[t] + f_grid_B[t] + f_grid_BC[t] <= Grid_Cap_y,
      t = 1:nb.T
    )
  
  # 4) Batterie Lade- und Entladeleistung
  model <- model %>%
    add_constraint(
      eta_B * (f_PV_B[t] + f_D_B[t] + f_grid_B[t]) <=
        c_rate_charge * (B_Cap_y + sum_expr(B_s_z[s] * n_Battery[s], s = 1:S[Battery_idx])),
      t = 1:nb.T
    ) %>%
    add_constraint(
      f_B_load[t] <=
        c_rate_discharge * (B_Cap_y + sum_expr(B_s_z[s] * n_Battery[s], s = 1:S[Battery_idx])),
      t = 1:nb.T
    )
  
  # 5) Batterieladezustand
  # Anfangs-SOC (kann als Parameter vorgegeben werden)
  model <- model %>%
    add_constraint(
      SOC[0] == SOC_0 * (B_Cap_y + sum_expr(B_s_z[s] * n_Battery[s], s = 1:S[Battery_idx])) 
    )
  
  model <- model %>%
    add_constraint(
      SOC[t] == SOC[t - 1] + eta_B * (f_PV_B[t] + f_D_B[t] + f_grid_B[t]) - f_B_load[t], 
      t = 1:nb.T)
  
  # 6) SOC Begrenzung
  model <- model %>%
    add_constraint(
      SOC[t] <= B_Cap_y + sum_expr(B_s_z[s] * n_Battery[s], s = 1:S[Battery_idx]), 
      t = 0:nb.T)
  
  
  # 7) Lade- und Entladebegrenzungen car batteries
  model <- model %>%
    add_constraint(
      f_PV_BC[t] + f_grid_BC[t] + f_D_BC[t] <= c_rate_charge_BC * Cap_BC[t],
      t = 1:nb.T
    ) %>%
    add_constraint(
      f_BC_load[t] <= c_rate_discharge_BC * Cap_BC[t],
      t = 1:nb.T
    )
  
  # Ensures BC charging from PV, grid, and diesel in period t does not exceed the number of AC chargers
  model <- model %>%
    add_constraint(
      f_PV_BC[t] + f_grid_BC[t] + f_D_BC[t] <= n_AC * AC_charge,
      t = 1:nb.T
    )
  
  # Ensures BC discharge to the load in period t does not exceed the number of AC chargers
  model <- model %>%
    add_constraint(
      f_BC_load[t] <= n_AC * AC_discharge,
      t = 1:nb.T
    )
  
  # 8) BC_SOC-Bilanz
  model <- model %>%
    add_constraint(
      SOC_BC[t] == SOC_BC[t - 1] +
        eta_BC * (f_PV_BC[t] + f_grid_BC[t] + f_D_BC[t]) -
        f_BC_load[t],
      t = 1:nb.T
    ) %>% 
    add_constraint(
      SOC_BC[0] == SOC_BC_0
    )
  
  # 9) Kapazitätsgrenzen car batteries
  model <- model %>%
    add_constraint(
      SOC_BC[t] <= Cap_BC[t],
      t = 1:nb.T
    )
  
  # 10) Mindest-SOC bei Park Start und Ende
  model <- model %>%
    add_constraint(SOC_BC[nb.T] >= 0.5 * Cap_BC[nb.T])
  
  
  # 11) Strombilanz (Lastdeckung)
  model <- model %>%
    add_constraint(
      (f_PV_load[t] + f_B_load[t] + f_grid_load[t] + f_D_load[t] + f_BC_load[t]) == L[t],
      t = 1:nb.T
    )
  
  # 12) Diesel Kraftstoffverbrauch (fix + variabel) mit technologiespezifischen Größenklassen
  model <- model %>%
    add_constraint(
      D_fuel_consumption[t] ==
        sum_expr(fuel_con_fix_Diesel[s] * n_Diesel[s], s = 1:S[Diesel_idx]) +
        (f_D_load[t] + f_D_B[t] + f_D_BC[t]) * fuel_con_var_Diesel,
      t = 1:nb.T
    )
  
  # 13) Set-up-Entscheidung
  model <- model %>%
    add_constraint(sum_expr(n_PV[s], s = 1:S[PV_idx]) <= delta_PV * sum_expr(rent_amount_PV_z[s], s = 1:S[PV_idx] ) ) %>%
    add_constraint(sum_expr(n_Battery[s], s = 1:S[Battery_idx]) <= delta_B * sum_expr(rent_amount_B_z[s], s = 1:S[Battery_idx] ) ) %>%
    add_constraint(sum_expr(n_Diesel[s], s = 1:S[Diesel_idx]) <= delta_D * sum_expr(rent_amount_D_z[s], s = 1:S[Diesel_idx] )) %>%
    add_constraint(n_AC <= delta_AC * M) 
  
  # Kosten und Zielfunktion mit technologiespezifischen Größenklassen
  model <- model %>%
    set_objective(
      # Mietkosten mit technologiespezifischen Größenklassen
      FD * (
        sum_expr(c_rent_PV_z[s] * n_PV[s], s = 1:S[PV_idx]) +
          sum_expr(c_rent_B_z[s] * n_Battery[s], s = 1:S[Battery_idx]) +
          sum_expr(c_rent_D_z[s] * n_Diesel[s], s = 1:S[Diesel_idx]) +
          c_rent_AC_z * n_AC
      ) +
        
        # Setupkosten
        delta_PV * c_setup_PV_z +
        delta_B * c_setup_B_z +
        delta_D * c_setup_D_z +
        delta_AC * c_setup_AC_z +
        
        # Verbrauchskosten
        sum_expr(
          c_PV_y * f_PV_gen_inst[t] +
            c_grid * (f_grid_load[t] + f_grid_B[t] + f_grid_BC[t]) +
            c_BC_discharge * f_BC_load[t] +
            diesel_cost * D_fuel_consumption[t],
          t = 1:nb.T
        ),
      
      sense = "min"
    )
  
  results_energy<- solve_model(model, with_ROI(solver = "glpk", verbose = TRUE, tm_limit = 600))
  # Gurobi
  #library(ROI.plugin.gurobi)
  #results_energy<- solve_model(model, with_ROI(solver = "gurobi"))
  
  # results list including results and inputs
  res.list <- list(
    results_opt = results_energy,
    inputs = list(
      parameters = params,
      energy.demand = L,
      ceof_PV = eta_PV)
  )
  
  return(res.list)
}
