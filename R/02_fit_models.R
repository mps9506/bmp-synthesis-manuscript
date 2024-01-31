

#' fit random effect meta regression to bacteria data
#'
#' @param data dataframe with `StudyID`, `Study`, `ValueID`, `Source_Type`,
#'   `BMP_SubCat`, `n_Pre`, `n_Post`, `Pre`, `Post`, `sd_Pre`, `sd_Post`,
#'   `Unit`. Or the output from `load_df()`
#'
#' @return list with model output of class `rma.mv` and dataframe with formatted
#'   data used to fit the model.
fit_bac_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |> 
    filter(Unit %in% c("CFU/100ml", "MPN/100ml", "Geometric mean (CFU/100ml)", "Geomean (CFU/100 mL)")) |> 
    filter(!is.na(Scale)) |> 
    filter(!is.na(Pre), !is.na(Post)) |>
    ## removing categories that result in rank deficiencies
    filter(BMP_SubCat != "Drainage") |> 
    filter(BMP_SubCat != "Filtration") |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre),
           Scale = forcats::fct_relevel(Scale, "Lot", "Community", "Watershed"))
  
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")

  
  ## Prep data for rma
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  

  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))

  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]


 
  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
    ))
}



#' Fit Egger's regression to detect small-study effect (publication bias)
#'
#' @param model
#'
#' @return rma object

fit_sse_model <- function(model) {
  
  mods <- model$sel_model |> 
    formula() |> 
    # add adjusted based sampling error - tilde square root n as a moderator to test small study effect.
    update.formula(~. + ase)
  
  df <- model$data
  
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  df <- df |>
    #calculate adapted sampling variance based on effective size based  - tilde n
    mutate(asv = (1/n_Pre) + (1/n_Post)) |> 
    # calculate adapted sampling error based on effective size - tilde square root n
    mutate(ase = sqrt(asv))
  
  m2 <- rma.mv(yi = yi,
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
  
  ## plot results
  df_em <- orchaRd::mod_results(m2, mod = "ase", group = "StudyID")
  df_mean <- orchaRd::mod_results(m2, group = "StudyID")
  terms <- confint(m2, fixed = TRUE)
  
  beta1 <- terms$fixed["ase", "estimate"]
  upper <- terms$fixed["ase", "ci.ub"]
  lower <- terms$fixed["ase", "ci.lb"]
  
  beta_label <- paste0("*β<sub>ase</sub>* =", 
                       round(beta1,2),
                       ",<br>95% CI [",
                       round(lower,2),
                       ",",
                       round(upper,2),
                       "]")
  beta_label <- richtext_grob(beta_label, x = 0.05, y = 0.9, hjust = 0,
                              gp = gpar(fontfamily = "Manrope Regular", fontsize = 9))
  
  p1 <- ggplot() +
    geom_hline(data = df_mean$mod_table, aes(yintercept = estimate), linetype = 2, alpha = 0.4) +
    geom_line(data = df_em$mod_table, aes(moderator, estimate)) +
    geom_ribbon(data = df_em$mod_table, aes(moderator, ymin = lowerCL, ymax = upperCL), alpha = 0.25) +
    geom_point(data = df, aes(ase, yi)) +
    
    annotation_custom(beta_label, ) +
    labs(x = "Adjusted sampling error", y = "Effect size (ROM)") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
    
    
    
  return(list(model = m2,
              plot = p1))
}

run_sensitvity <- function(model) {
  ## run leave one out analysis
  
  mods <- model$sel_model |> 
    formula()
  
  df <- model$data |> 
    group_by(Study) |> 
    mutate(study_num = row_number(Study)) |> 
    ungroup() |> 
    mutate(Study = paste0(Study, ".", study_num)) |> 
    select(-study_num) |> 
    mutate(Study = as_factor(Study))
  
  VCV <- vcalc(vi = df$vi,
               cluster = df$StudyID,
               obs = df$ValueID,
               rho = 0.5,
               nearpd = TRUE)
  
  
  ## leave one out
  loo <- list()
  study_levels <- levels(df$Study)
  
  for(i in 1:length(study_levels)) {
    loo_df <- df |> 
      filter(Study != study_levels[i]) |> 
      mutate(Study = fct_drop(Study))
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
    labs(x = "*β<sub>0</sub>*", y = "Study left out") +
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


#' fit random effect meta regression to tn data
#'
#' @param data dataframe with `StudyID`, `Study`, `ValueID`, `Source_Type`,
#'   `BMP_SubCat`, `n_Pre`, `n_Post`, `Pre`, `Post`, `sd_Pre`, `sd_Post`,
#'   `Unit`. Or the output from `load_df()`
#'
#' @return list with model output of class `rma.mv` and dataframe with formatted
#'   data used to fit the model.
fit_tn_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter %in% c("TN")) |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))

  ## Prep data for rma
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  df <- df |>
    #calculate adapted sampling variance based on effective size based  - tilde n
    mutate(asv = (1/n_Pre) + (1/n_Post)) |> 
    # calculate adapted sampling error based on effective size - tilde square root n
    mutate(ase = sqrt(asv))
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]
  
  # ## fit model with adjust sampling size to account for pub bias
  # m2 <- rma.mv(yi = yi,
  #              V = VCV,
  #              # allows true effect sizes to vary among different primary
  #              # studies - account for the between-study effect and quantify
  #              # between-study heterogeneity; allows true effect sizes to vary
  #              # within primary studies - account for the with-study effect and
  #              # quantify with-study heterogeneity;
  #              random = list(~ 1 | StudyID, ~ 1 | ValueID),
  #              mods = ~ ase + Scale + ai * BMP_SubCat + lnPre * BMP_SubCat,
  #              data = df,
  #              method = "ML",
  #              test = "t",
  #              dfs = "contain",
  #              sparse = FALSE,
  #              verbose = FALSE,
  #              control=list(rel.tol=1e-8))
  # 
  # res_ase <- dredge(m2, beta = "none", evaluate = TRUE, rank = "AICc", fixed = "ase")
  # models_ase <- res_ase |> get.models(subset = delta <= 2, method = "REML")
  # sel_model_ase <- models_ase[[length(models_ase)]]
  

  
  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}

fit_tin_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter %in% c("NO3", "NOX", "NH3")) |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))
  
  ## Prep data for rma
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  df <- df |>
    #calculate adapted sampling variance based on effective size based  - tilde n
    mutate(asv = (1/n_Pre) + (1/n_Post)) |> 
    # calculate adapted sampling error based on effective size - tilde square root n
    mutate(ase = sqrt(asv))
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]


  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}


#' fit random effect meta regression to tp data
#'
#' @param data dataframe with `StudyID`, `Study`, `ValueID`, `Source_Type`,
#'   `BMP_SubCat`, `n_Pre`, `n_Post`, `Pre`, `Post`, `sd_Pre`, `sd_Post`,
#'   `Unit`. Or the output from `load_df()`
#'
#' @return list with model output of class `rma.mv` and dataframe with formatted
#'   data used to fit the model.
fit_tp_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter %in% c("TP")) |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))
  
  ## Prep data for rma
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]

  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}

fit_tp_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter %in% c("TP")) |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))
  
  ## Prep data for rma
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]
  
  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}


fit_po4_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter %in% c("PO4")) |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))
  
  ## Prep data for rma
  ##impute SD
  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]
  
  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}

#' fit random effect meta regression to tss data
#'
#' @param data dataframe with `StudyID`, `Study`, `ValueID`, `Source_Type`,
#'   `BMP_SubCat`, `n_Pre`, `n_Post`, `Pre`, `Post`, `sd_Pre`, `sd_Post`,
#'   `Unit`. Or the output from `load_df()`
#'
#' @return list with model output of class `rma.mv` and dataframe with formatted
#'   data used to fit the model.
fit_tss_model <- function(data) {
  df <- data |> 
    select(StudyID, Study, ValueID, Parameter, Source_Type, BMP_SubCat, Scale, Area, study_length, n_Pre, n_Post, Pre, Post, sd_Pre, sd_Post, Unit, ai) |>
    filter(Parameter == "TSS") |> 
    filter(Unit %in% c("mg/L")) |> 
    filter(!is.na(Pre), !is.na(Post)) |> 
    mutate(StudyID = fct_drop(StudyID),
           Study = fct_drop(Study),
           lnPre = log(Pre),
           lnArea = log(Area),
           BMP_SubCat = fct_drop(BMP_SubCat),
           missing_sd = is.na(sd_Pre))
  
  ## Prep data for rma
  ##impute SD

  df <- metagear::impute_SD(data.frame(df),
                            c("sd_Pre", "sd_Post"),
                            c("Pre", "Post"),
                            method = "Bracken1992")
  
  
  ## Prep data for rma
  
  df <- escalc(measure = "ROM",
               vtype = "AV",
               m1i = Pre,
               m2i = Post,
               n1i = n_Pre,
               n2i = n_Post,
               sd1i = sd_Pre,
               sd2i = sd_Post,
               slab = Study,
               data = df)
  
  # impute the variance covariance matrix
  VCV <- vcalc(vi = vi,
               cluster = StudyID,
               obs = ValueID,
               data = df,
               rho = 0.5,
               nearpd = TRUE)
  
  
  ## null model
  m_null <- rma.mv(yi = yi,
                   V = VCV,
                   # allows true effect sizes to vary among different primary
                   # studies - account for the between-study effect and quantify
                   # between-study heterogeneity; allows true effect sizes to vary
                   # within primary studies - account for the with-study effect and
                   # quantify with-study heterogeneity;
                   random = list(~ 1 | StudyID, ~ 1 | ValueID),
                   data = df,
                   method = "REML",
                   test = "t",
                   dfs = "contain",
                   sparse = FALSE,
                   verbose = FALSE,
                   control=list(rel.tol=1e-8))
  
  ## fit multilevel random effects
  m1 <- rma.mv(yi = yi,
               V = VCV,
               # allows true effect sizes to vary among different primary
               # studies - account for the between-study effect and quantify
               # between-study heterogeneity; allows true effect sizes to vary
               # within primary studies - account for the with-study effect and
               # quantify with-study heterogeneity;
               random = list(~ 1 | StudyID, ~ 1 | ValueID),
               mods = ~ ai * BMP_SubCat + lnPre * BMP_SubCat,
               data = df,
               method = "ML",
               test = "t",
               dfs = "contain",
               sparse = FALSE,
               verbose = FALSE,
               control=list(rel.tol=1e-8))
  
  
  res <- dredge(m1, beta = "none", evaluate = TRUE, rank = "AICc")
  sel_model <- get.models(res, subset = 1, method = "REML")[[1]]
  
  return(list(
    null = m_null,
    model = m1,
    sel_model = sel_model,
    data = df,
    res = res
  ))
}



#' Assessing number of records missing standard deviation
#'
#' @param model_output Accepts output from `fit_bac_model()` 
#'
#' @return named list with total number of records and records missing sd.
assess_missing <- function(model_output) {
  df <- model_output$data
  
  df <- df |> 
    group_by(missing_sd) |> 
    summarise(n = n())
  
  missing <- df |> 
    filter(missing_sd == TRUE) |> 
    pull(n)
  total <- sum(df$n)
  return(list(missing = missing,
              total = total))
}
