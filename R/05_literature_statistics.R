
study_count_summary <- function(bac_df, nut_df) {
  
  ## summarize study
  study_bac <- bac_df |> 
    group_by(StudyID) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    summarise(Study_n = sum(n)) |> 
    mutate(Parameter = "FIB")
  ##summarize effect
  effect_bac <- bac_df |> 
    group_by(ValueID) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    summarise(Effect_n = sum(n)) |> 
    mutate(Parameter = "FIB")
  
  
  study_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(StudyID, Parameter) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Parameter) |> 
    summarise(Study_n = sum(n)) |> 
    bind_rows(study_bac)
  
  effect_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(ValueID, Parameter) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Parameter) |> 
    summarise(Effect_n = sum(n)) |> 
    bind_rows(effect_bac)
  
  df <- left_join(study_df, effect_df) 
  df
}


plot_map <- function(bac_df,
                     nut_df) {
  bac_df <- bac_df |>  
    group_by(StudyID, State) |> 
    summarise(n = 1) |> 
    ungroup() |> 
    group_by(State) |> 
    summarise(FIB = sum(n))
  
  nut_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    group_by(StudyID, State, Parameter) |>
    summarise(n = 1) |> 
    filter(!is.na(Parameter)) |> 
    ungroup() |> 
    group_by(State, Parameter) |> 
    summarise(n = sum(n)) |> 
    pivot_wider(names_from = Parameter,
                values_from = n)
  
  us_file <- tigris::states(cb = TRUE, progress_bar = FALSE)
  
  
  ## shold make the following left join a bac field, then nut field, then pivot longer 
  
  us_file |>  
    left_join(bac_df, by = c("NAME" = "State")) |> 
    left_join(nut_df, by = c("NAME" = "State")) |> 
    pivot_longer(cols = FIB:TP,
                 names_to = "Parameter",
                 values_to = "n") |> 
    mutate(n_scale = case_when(
      is.na(n) ~ 0,
      .default = n
    )) |> 
    filter(!(NAME %in% c("American Samoa",
                         "Commonwealth of the Northern Mariana Islands",
                         "Guam",
                         "United States Virgin Islands"))) |>
    st_simplify(preserveTopology = TRUE, dTolerance = 1000) |> 
    shift_geometry() |> 
    ggplot() +
    geom_sf(aes(fill = n)) +
    geom_text_repel(aes(label = n, geometry = geometry), size = 2.5, 
                    stat = "sf_coordinates", bg.color = "white", bg.r = 0.15,
                    family = "Manrope SemiBold", fontface = "plain",
                    force = 0) +
    scale_fill_distiller(palette = "Greens", direction = 1, na.value = "grey80",
                         trans = "log10") +
    facet_wrap(~Parameter, ncol = 2) +
    labs(x = "", y = "") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          panel.grid = element_blank(),
          strip.background = element_rect(fill = "grey90",
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black",
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  
}


plot_bmp_summary <- function(bac_df, nut_df) {
  
  ## summarize scale
  scale_bac <- bac_df |> 
    group_by(StudyID, Scale) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Scale) |> 
    summarise(n = sum(n)) |> 
    mutate(Parameter = "FIB")
  
  
  scale_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(StudyID, Parameter, Scale) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Parameter, Scale) |> 
    summarise(n = sum(n)) |> 
    bind_rows(scale_bac) |> 
    mutate(Scale = case_when(
      Scale == "Lot" ~ "Lot/Field",
      Scale == "Community" ~ "Community/Farm",
      Scale == "Watershed" ~ "Catchment/Watershed",
      .default = Scale
    )) |> 
    mutate(Scale = forcats::fct_relevel(Scale, "Lot/Field", "Community/Farm", "Catchment/Watershed"))
  
  p1 <- ggplot(scale_df) +
    geom_col(aes(n, Parameter, fill = Scale)) +
    geom_text_repel(aes(n, Parameter, label = n, group = Scale),
                    position = position_stack(vjust = 0.5, reverse = FALSE),
                    bg.color = "#FFFFFF33", bg.r = 0.15,
                    family = "Manrope SemiBold", fontface = "plain",
                    force = 0, size = 3) +
    scale_x_continuous("Studies (n)", expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete("") +
    scale_fill_manual("Study Scale:", values = c("#1B9E77","#D95F02","#7570B3"),
                      guide = guide_legend(reverse = TRUE)) +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.y = element_blank(),
          plot.tag = element_text(family = "Manrope SemiBold",
                                  color = "black", 
                                  face = "bold",
                                  size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
  
  ## summarize source type
  source_bac <- bac_df |> 
    group_by(StudyID, Source_Type) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Source_Type) |> 
    summarise(n = sum(n)) |> 
    mutate(Parameter = "FIB")
  
  
  source_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(StudyID, Parameter, Source_Type) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Parameter, Source_Type) |> 
    summarise(n = sum(n)) |> 
    bind_rows(source_bac)
  
  p2 <- ggplot(source_df) +
    geom_col(aes(n, Parameter, fill = Source_Type)) +
    geom_text_repel(aes(n, Parameter, label = n, group = Source_Type),
                    position = position_stack(vjust = 0.5, reverse = FALSE),
                    bg.color = "#FFFFFF33", bg.r = 0.15,
                    family = "Manrope SemiBold", fontface = "plain",
                    force = 0, size = 3) +
    scale_x_continuous("Studies (n)", expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete("") +
    scale_fill_manual("Runoff Source:", values = c("#1B9E77","#D95F02"),
                      guide = guide_legend(reverse = TRUE)) +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.y = element_blank(),
          plot.tag = element_text(family = "Manrope SemiBold",
                                  color = "black", 
                                  face = "bold",
                                  size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
  
  
  ## BMPs
  bmp_bac <- bac_df |> 
    group_by(StudyID, BMP_Class) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(BMP_Class) |> 
    summarise(n = sum(n)) |> 
    mutate(Parameter = "FIB")
  
  bmp_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(StudyID, Parameter, BMP_Class) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(Parameter, BMP_Class) |> 
    summarise(n = sum(n)) |> 
    bind_rows(bmp_bac) |> 
    mutate(BMP_Class = stringr::str_to_sentence(BMP_Class)) |> 
    mutate(BMP_Class = case_when(
      BMP_Class == "Swale" ~ "Swales",
      BMP_Class == "Proprietary" ~ "Proprietary devices",
      BMP_Class == "Drainage systems" ~ "Drainage water management",
      .default = BMP_Class
    )) |> 
    ungroup() |> 
    mutate(BMP_Class = reorder(BMP_Class,
                               n,
                               FUN = sum))
  
  p3 <- ggplot(bmp_df) +
    geom_col(aes(n,  BMP_Class, fill = Parameter)) +
    scale_fill_brewer("Parameter:", palette = "Dark2",
                      guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous("Studies (n)", expand = expansion(mult = c(0,0.1))) +
    labs(y = "") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          legend.position = "right",
          legend.direction = "vertical",
          panel.grid.major.y = element_blank(),
          plot.tag = element_text(family = "Manrope SemiBold",
                                  color = "black", 
                                  face = "bold",
                                  size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
  
  
  layout <- "
  AAAA
  AAAA
  AAAA
  BBBB
  BBBB
  BBBB
  CCCC
  CCCC
  CCCC
  CCCC
  CCCC
  CCCC"
  
  p1 + p2 + p3 + patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(design = layout)
  
  
  
}


plot_bmp_temporal <- function(bac_df, nut_df) {
  bac_pub_year <- bac_df |> 
    group_by(StudyID, Pub_Year) |> 
    summarise(n = n()) |> 
    group_by(Pub_Year) |> 
    summarise(n = n()) |> 
    mutate(Parameter = "FIB")
  
  nut_pub_year <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter)) |> 
    group_by(StudyID, Parameter, Pub_Year) |> 
    summarise(n = n()) |> 
    group_by(Parameter, Pub_Year) |> 
    summarise(n = n())
  
  ## year of publication
  pub_year <- bind_rows(bac_pub_year, nut_pub_year) |> 
    ggplot() +
    geom_col(aes(Pub_Year, n, fill = Parameter)) +
    facet_wrap(vars(Parameter)) +
    scale_fill_brewer("Parameter:", palette = "Dark2",
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous("Studies (n)", expand = expansion(mult = c(0,0.1))) +
    labs(x = "Publication Year") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          legend.position = "none",
          legend.direction = "vertical",
          panel.grid.major.y = element_blank(),
          plot.tag = element_text(family = "Manrope SemiBold",
                                  color = "black", 
                                  face = "bold",
                                  size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
  
  ## length of study
  bac_df <- bac_df |> 
    group_by(StudyID, Data_Year) |> 
    summarise(n = n())
  
  nut_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN") ~ "TN",
      Parameter %in% c("NO3", "NOX", "NH3") ~ "DIN",
      Parameter %in% c("TP") ~ "TP",
      Parameter %in% c("PO4") ~ "PO4",
      Parameter == "TSS" ~ "TSS",
      .default = NA
    )) |> 
    filter(!is.na(Parameter))
  
  sum_param <- function(df, param) {
    df |> 
      filter(Parameter == {{param}}) |> 
      group_by(StudyID, Data_Year) |> 
      summarise(n = n())
  }
  
  nut_df |> 
    sum_param(param = "TN")
  
  tn_df <- nut_df |> 
    sum_param(param = "TN")
  
  din_df <- nut_df |> 
    sum_param(param = "DIN")
  
  tp_df <- nut_df |> 
    sum_param(param = "TP")
  
  po4_df <- nut_df |> 
    sum_param(param = "PO4")
  
  tss_df <- nut_df |> 
    sum_param(param = "TSS")
  
  split_years <- function(Data_Year) {
    lapply(strsplit(Data_Year,
                    '\\s*;\\s*'),
           function(x) unlist(lapply(strsplit(x,'-'), function(y){ z <- as.integer(y); if (length(z)==1L) z else z[1L]:z[2L]; })))
  }
  
  sum_years <- function(Data_Year, parameter) {
    tibble(years = split_years(Data_Year)) |> 
      filter(!is.na(years)) |> 
      mutate(study_length = map(years, ~length(.x))) |> 
      unnest(study_length) |>
      group_by(study_length) |> 
      summarize(n = n()) |> 
      mutate(parameter = {{parameter}})
  }
  
  fib <- sum_years(bac_df$Data_Year, parameter = "FIB")
  tn <- sum_years(tn_df$Data_Year, parameter = "TN")
  din <- sum_years(din_df$Data_Year, parameter = "DIN")
  tp <- sum_years(tp_df$Data_Year, parameter = "TP")
  po4 <- sum_years(po4_df$Data_Year, parameter = "PO4")
  tss <- sum_years(tss_df$Data_Year, parameter = "TSS")
  
  stdy_length <- bind_rows(fib, tn, din, tp, po4, tss) |> 
    ggplot() +
    geom_col(aes(study_length, n, fill = parameter)) +
    facet_wrap(vars(parameter)) +
    scale_fill_brewer("Parameter:", palette = "Dark2",
                      guide = guide_legend(reverse = TRUE)) +
    scale_y_continuous("Studies (n)", expand = expansion(mult = c(0,0.1))) +
    labs(x = "Study length (years)") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(family = "Manrope Light", face = "plain", size = rel(0.8), hjust = 1),
          legend.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          legend.position = "none",
          legend.direction = "vertical",
          panel.grid.major.y = element_blank(),
          plot.tag = element_text(family = "Manrope SemiBold",
                                  color = "black", 
                                  face = "bold",
                                  size = rel(0.8), hjust = 0),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "bold",
                                    size = rel(0.8), hjust = 0))
  
  p1 <- (pub_year / stdy_length) + patchwork::plot_annotation(tag_levels = "A")
  
  ## sanity check on means, commented out.
  # stdy_lngth_mean <- bind_rows(fib, tn, din, tp, po4, tss) |> 
  #   group_by(parameter) |> 
  #   summarise(mean = sum(study_length*n)/sum(n))
  
  stdy_len_stats <- bind_rows(fib, tn, din, tp, po4, tss) |> 
    mutate(vals = map2(study_length, n, \(study_length, n) rep(study_length, n))) |> 
    unnest(vals) |> 
    group_by(parameter) |> 
    summarise(median = median(vals),
              mean = mean(vals),
              n = n())
  
  return(list(p1 = p1,
              stdy_len_stats = stdy_len_stats))
}


plot_ai_summary <- function(bac_df, nut_df) {

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
  
  plot(cropped_ai)
  
  cropped_ai_tibble <- as_tibble(cropped_ai, na.rm = TRUE)
  
  bac_ai <- tibble(awi_pm_sr_yr = bac_df$model_df$ai[,1] + bac_df$mean,
                   label = "BMP AI Values")
  
  nut_ai <- tibble(awi_pm_sr_yr = nut_df$model_df$ai[,1] + bac_df$mean,
                   label = "BMP AI Values")
  
  
  
  cropped_ai_tibble |> 
    mutate(label = "U.S. AI Values") |> 
    bind_rows(bac_ai) |> 
    bind_rows(nut_ai) |> 
    ggplot() +
    geom_density(aes(awi_pm_sr_yr,
                     color = label,
                     fill = label),
                 alpha = 0.5,
                 trim = TRUE,
                 bw = "bcv") +
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    scale_x_continuous(expand = expansion(mult = c(0,0.05))) +
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