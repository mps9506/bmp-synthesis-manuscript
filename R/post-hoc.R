source_sensitvity <- function(model) {
  ## run sensitivity on source type
  
  mods <- model$sel_model |> 
    formula()
  
  
  df <- model$data |> 
    group_by(Study) |> 
    mutate(study_num = row_number(Study)) |> 
    ungroup() |> 
    mutate(Study = paste0(Study, ".", study_num)) |> 
    select(-study_num) |> 
    mutate(Study = as_factor(Study)) |> 
    mutate(Source_Type = as_factor(Source_Type)) |> 
    mutate(Scale = as_factor(Scale))
  
  
  
  VCV <- vcalc(vi = df$vi,
               cluster = df$StudyID,
               obs = df$ValueID,
               rho = 0.5,
               nearpd = TRUE)
  
  
  ## leave one out
  loo <- list()
  study_levels <- levels(df$Source_Type)
  
  
  for(i in 1:length(study_levels)) {
    loo_df <- df |>
      filter(Source_Type != study_levels[i]) |>
      mutate(Source_Type = fct_drop(Source_Type))
    VCV_leave1out <- vcalc(vi = loo_df$vi,
                           cluster = loo_df$StudyID,
                           obs = loo_df$ValueID,
                           rho = 0.5,
                           nearpd = TRUE)
    loo[[i]] <- rma.mv(yi = yi,
                       V = VCV_leave1out,
                       random = ~1 | StudyID / ValueID,
                       mods = mods,
                       data = loo_df,
                       method = "REML",
                       test = "t",
                       dfs = "contain",
                       sparse = FALSE,
                       verbose = FALSE,
                       control=list(rel.tol=1e-8))
    
  }
  
  loo <- loo |>
    purrr::map(summary) |>
    purrr::map(coef) |>
    purrr::map(\(x) slice(x, 1)) |>
    list_rbind() |>
    tibble::rownames_to_column() |>
    as_tibble() |>
    mutate(studies = study_levels)
  
  
  
  m2_VCV <-  rma.mv(yi = yi,
                    V = VCV,
                    random = ~1 | StudyID / ValueID,
                    mods = mods,
                    data = df,
                    method = "REML",
                    test = "t",
                    dfs = "contain",
                    sparse = FALSE,
                    verbose = FALSE,
                    control=list(rel.tol=1e-8))
  
  
  mean <- m2_VCV$b[1]
  ub <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.ub)
  lb <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.lb)
  
  p1 <- ggplot(loo) +
    geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = studies)) +
    geom_vline(xintercept = mean) +
    geom_vline(xintercept = ub, linetype = "dotted") +
    geom_vline(xintercept = lb, linetype = "dotted") +
    labs(x = "*β<sub>0</sub>*", y = "Excluded Runoff Source") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", size = rel(1)),
          axis.title.x = element_markdown(family = "Manrope SemiBold", hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", size = rel(0.8)),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90",
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope ExtraBold",
                                    color = "black",
                                    size = rel(1), hjust = 0))
  
  return(list(loo = loo, model = m2_VCV, p1 = p1))
  
}


scale_sensitvity <- function(model) {
  ## run leave one out analysis
  
  mods <- model$sel_model |> 
    formula()
  
  
  df <- model$data |> 
    group_by(Study) |> 
    mutate(study_num = row_number(Study)) |> 
    ungroup() |> 
    mutate(Study = paste0(Study, ".", study_num)) |> 
    select(-study_num) |> 
    mutate(Study = as_factor(Study)) |> 
    mutate(Source_Type = as_factor(Source_Type)) |> 
    mutate(Scale = as_factor(Scale))
  
  
  
  
  VCV <- vcalc(vi = df$vi,
               cluster = df$StudyID,
               obs = df$ValueID,
               rho = 0.5,
               nearpd = TRUE)
  
  
  ## leave one out
  loo <- list()
  study_levels <- levels(df$Scale)
  
  
  for(i in 1:length(study_levels)) {
    loo_df <- df |>
      filter(Scale != study_levels[i]) |>
      mutate(Scale = fct_drop(Scale))
    VCV_leave1out <- vcalc(vi = loo_df$vi,
                           cluster = loo_df$StudyID,
                           obs = loo_df$ValueID,
                           rho = 0.5,
                           nearpd = TRUE)
    loo[[i]] <- rma.mv(yi = yi,
                       V = VCV_leave1out,
                       random = ~1 | StudyID / ValueID,
                       mods = mods,
                       data = loo_df,
                       method = "REML",
                       test = "t",
                       dfs = "contain",
                       sparse = FALSE,
                       verbose = FALSE,
                       control=list(rel.tol=1e-8))
    
  }
  
  loo <- loo |>
    purrr::map(summary) |>
    purrr::map(coef) |>
    purrr::map(\(x) slice(x, 1)) |>
    list_rbind() |>
    tibble::rownames_to_column() |>
    as_tibble() |>
    mutate(studies = study_levels)
  
  
  
  m2_VCV <-  rma.mv(yi = yi,
                    V = VCV,
                    random = ~1 | StudyID / ValueID,
                    mods = mods,
                    data = df,
                    method = "REML",
                    test = "t",
                    dfs = "contain",
                    sparse = FALSE,
                    verbose = FALSE,
                    control=list(rel.tol=1e-8))
  
  
  mean <- m2_VCV$b[1]
  ub <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.ub)
  lb <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.lb)
  
  p1 <- ggplot(loo) +
    geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = studies)) +
    geom_vline(xintercept = mean) +
    geom_vline(xintercept = ub, linetype = "dotted") +
    geom_vline(xintercept = lb, linetype = "dotted") +
    labs(x = "*β<sub>0</sub>*", y = "Excluded Catchment Scale") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", size = rel(1)),
          axis.title.x = element_markdown(family = "Manrope SemiBold", hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", size = rel(0.8)),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90",
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope ExtraBold",
                                    color = "black",
                                    size = rel(1), hjust = 0))
  
  return(list(loo = loo, model = m2_VCV, p1 = p1))
  
}