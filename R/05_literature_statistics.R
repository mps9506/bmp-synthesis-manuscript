plot_map <- function(bac_df,
                     nut_df) {
  bac_df <- bac_df |>  
    group_by(StudyID, State) |> 
    summarise(n = 1) |> 
    ungroup() |> 
    group_by(State) |> 
    summarise(Bacteria = sum(n))
  
  nut_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN", "DON", "NOX", "NO3", "NO2", "TDN") ~ "Nitrogen",
      Parameter %in% c("TP", "PO4", "DOP", "DP", "TDP") ~ "Phosphorus",
      Parameter == "TSS" ~ "Suspended Solids",
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
    pivot_longer(cols = Bacteria:Phosphorus,
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
    shift_geometry() |> 
    ggplot() +
    geom_sf(aes(fill = n)) +
    geom_text_repel(aes(label = n, geometry = geometry), size = 3, 
                    stat = "sf_coordinates", bg.color = "white", bg.r = 0.15,
                    family = "Manrope SemiBold", fontface = "plain",
                    force = 0) +
    scale_fill_distiller(palette = "Greens", direction = 1, na.value = "grey80",
                         trans = "log10") +
    facet_wrap(~Parameter) +
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
    mutate(Parameter = "Fecal indicator bacteria")
  
  
  scale_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN", "DON", "NOX", "NO3", "NO2", "TDN") ~ "Nitrogen",
      Parameter %in% c("TP", "PO4", "DOP", "DP", "TDP") ~ "Phosphorus",
      Parameter == "TSS" ~ "Suspended Solids",
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
    mutate(Parameter = "Fecal indicator bacteria")
  
  
  source_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN", "DON", "NOX", "NO3", "NO2", "TDN") ~ "Nitrogen",
      Parameter %in% c("TP", "PO4", "DOP", "DP", "TDP") ~ "Phosphorus",
      Parameter == "TSS" ~ "Suspended Solids",
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
  
  p1 + p2
  
  
  ## BMPs
  bmp_bac <- bac_df |> 
    group_by(StudyID, BMP_Class) |> 
    summarize(n = 1) |> 
    ungroup() |> 
    group_by(BMP_Class) |> 
    summarise(n = sum(n)) |> 
    mutate(Parameter = "Fecal indicator bacteria")
  
  bmp_df <- nut_df |> 
    mutate(Parameter = case_when(
      Parameter %in% c("TN", "DON", "NOX", "NO3", "NO2", "TDN") ~ "Nitrogen",
      Parameter %in% c("TP", "PO4", "DOP", "DP", "TDP") ~ "Phosphorus",
      Parameter == "TSS" ~ "Suspended Solids",
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
    scale_fill_manual("", values = c("#1B9E77","#D95F02","#7570B3", "#E7298A"),
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
  AABB
  AABB
  AABB
  CCCC
  CCCC
  CCCC
  CCCC
  CCCC"
  
  p1 + p2 + p3 + patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(design = layout)
  
  
}