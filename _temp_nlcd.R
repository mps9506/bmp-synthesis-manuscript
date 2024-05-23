library(targets)
library(MuMIn)
library(RColorBrewer)
library(broom)
library(dplyr)
library(forcats)
library(ggforce)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(grid)
library(gridtext)
library(gt)
library(httr)
library(kableExtra)
library(latex2exp)
library(markdown)
library(metafor)
library(metagear)
library(modelr)
library(mpsTemplates)
library(orchaRd)
library(patchwork)
library(purrr)
library(readxl)
library(rstudioapi)
library(scales)
library(sf)
library(shiny)
library(terra)
library(tibble)
library(tidyr)
library(tigris)
library(units)
library(visNetwork)



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
    mutate(Study = as_factor(Study)) |> 
    mutate(Source_Type = as_factor(Source_Type))
  
  VCV <- vcalc(vi = df$vi,
               cluster = df$StudyID,
               obs = df$ValueID,
               rho = 0.5,
               nearpd = TRUE)


  ## leave one out
  loo <- list()
  study_levels <- levels(df$Source_Type)
  print(study_levels)

  for(i in 1:length(study_levels)) {
    loo_df <- df |>
      filter(Source_Type != Source_Type[i]) #|>
      #mutate(Source_Type = fct_drop(Source_Type))
    # VCV_leave1out <- vcalc(vi = loo_df$vi,
    #                        cluster = loo_df$StudyID,
    #                        obs = loo_df$ValueID,
    #                        rho = 0.5,
    #                        nearpd = TRUE)
    # loo[[i]] <- rma.mv(yi = yi,
    #                    V = VCV_leave1out,
    #                    random = ~1 | StudyID / ValueID,
    #                    mods = mods,
    #                    data = loo_df,
    #                    method = "REML",
    #                    test = "t",
    #                    dfs = "contain",
    #                    sparse = FALSE,
    #                    verbose = FALSE,
    #                    control=list(rel.tol=1e-8))
    print(loo_df)

  }
  # 
  # loo <- loo |>
  #   purrr::map(summary) |>
  #   purrr::map(coef) |>
  #   purrr::map(\(x) slice(x, 1)) |>
  #   list_rbind() |>
  #   tibble::rownames_to_column() |>
  #   as_tibble() #|>
  #   mutate(studies = study_levels)
  # loo
  # 
  # 
  # 
  # m2_VCV <-  rma.mv(yi = yi,
  #                   V = VCV,
  #                   random = ~1 | StudyID / ValueID,
  #                   mods = mods,
  #                   data = df,
  #                   method = "REML",
  #                   test = "t",
  #                   dfs = "contain",
  #                   sparse = FALSE,
  #                   verbose = FALSE,
  #                   control=list(rel.tol=1e-8))
  # 
  # 
  # mean <- m2_VCV$b[1]
  # ub <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.ub)
  # lb <- m2_VCV |> summary() |> coef() |> slice(1) |> pull(ci.lb)
  # 
  # p1 <- ggplot(loo) +
  #   geom_pointrange(aes(x = estimate, xmin = ci.lb, xmax = ci.ub, y = studies)) +
  #   geom_vline(xintercept = mean) +
  #   geom_vline(xintercept = ub, linetype = "dotted") +
  #   geom_vline(xintercept = lb, linetype = "dotted") +
  #   labs(x = "*β<sub>0</sub>*", y = "Study left out") +
  #   theme_mps_noto(base_family = "Manrope Regular") +
  #   theme(axis.title = element_text(family = "Manrope SemiBold", size = rel(1)),
  #         axis.title.x = element_markdown(family = "Manrope SemiBold", hjust = 0),
  #         axis.title.y = element_text(hjust = 1),
  #         axis.text = element_text(family = "Manrope Light", size = rel(0.8)),
  #         legend.position = "none",
  #         panel.grid.major.y = element_blank(),
  #         strip.background = element_rect(fill = "grey90", 
  #                                         color = "grey90"),
  #         strip.text = element_text(family = "Manrope ExtraBold",
  #                                   color = "black", 
  #                                   size = rel(1), hjust = 0))
  # 
  # return(list(loo = loo, model = m2_VCV, p1 = p1))
  
}


run_sensitvity(tar_read(bac_model))



aridity_landuse <- tar_read(aridity_landuse)

plot_ai_summary <- function(bac_df, nut_df, aridity_landuse) {
  
  ai <- rast("data/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
  ai <- ai * 0.0001 
  
  
  us_file <- tigris::states(cb = TRUE, progress_bar = FALSE)
  
  us_file <- us_file |> 
    filter(!(NAME %in% c("Alaska",
                         "American Samoa",
                         "Guam",
                         "Commonwealth of the Northern Mariana Islands",
                         "United States Virgin Islands")))
  
  us_file_trans <- us_file |> 
    vect() |> 
    project(ai)
  
  
  cropped_ai <- crop(ai, us_file_trans) |> 
    mask(us_file_trans)
  
  
  cropped_ai_tibble <- as_tibble(cropped_ai, na.rm = TRUE)
  
  bac_ai <- tibble(awi_pm_sr_yr = bac_df$model_df$ai_uncentered,
                   label = "BMP AI Values")
  
  nut_ai <- tibble(awi_pm_sr_yr = nut_df$model_df$ai_uncentered,
                   label = "BMP AI Values")
  
  landuse_ai <- tibble(awi_pm_sr_yr = aridity_landuse$awi_pm_sr_yr,
                   label = "Developed and Agricultural AI Values")
  
  
  
  cropped_ai_tibble |> 
    mutate(label = "U.S. AI Values") |> 
    bind_rows(bac_ai) |> 
    bind_rows(nut_ai) |> 
    bind_rows(landuse_ai) |> 
    ggplot() +
    geom_density(aes(awi_pm_sr_yr,
                     color = label,
                     fill = label),
                 alpha = 0.5,
                 trim = TRUE,
                 bw = "bcv") +
    #scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(expand = expansion(mult = c(0.01,0.05))) +
    scale_color_brewer("", palette = "Dark2") +
    scale_fill_brewer("", palette = "Dark2") +
    coord_cartesian(xlim = c(0,4)) +
    labs(x = "Aridity index", y = "Density") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey90",
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black",
                                    face = "plain",
                                    size = rel(1), hjust = 0))
}


plot_ai_summary(tar_read(bac_model_df),
                tar_read(nut_model_df),
                aridity_landuse)
