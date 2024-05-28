# _targets.R
library(targets)

tar_option_set(packages = c("broom",
                            "dplyr",
                            "forcats",
                            "ggplot2",
                            "ggforce",
                            "ggtext",
                            "ggrepel",
                            "ggridges",
                            "grid",
                            "gridtext",
                            "httr",
                            "kableExtra",
                            "latex2exp",
                            "metafor",
                            "metagear",
                            "modelr",
                            "mpsTemplates",
                            "MuMIn",
                            "orchaRd",
                            "patchwork",
                            "purrr",
                            "RColorBrewer",
                            "readxl",
                            "scales",
                            "sf",
                            "terra",
                            "tibble",
                            "tidyr",
                            "tigris",
                            "units"
))

options(knitr.kable.NA = '',
        na.action = "na.fail")


## source functions
source("R/01_read_data.R")
source("R/02_fit_models.R")
source("R/03_plot_models.R")
source("R/04_tables.R")
source("R/05_literature_statistics.R")
source("R/post-hoc.R")

## prep fancy figure font
font_hoist("Manrope")

## for runnning dredge() in MuMIn on rma models
eval(metafor:::.MuMIn)




list(
  # Data -----------------------------------------------------------------------
  
  tar_target(bacteria_df, load_df("bacteria")),
  tar_target(nutrient_df, load_df("nutrients")),
  #https://stackoverflow.com/questions/70281450/dealing-with-zip-files-in-a-targets-workflow
  #https://figshare.com/ndownloader/files/34377245

  tar_target(global_ai, download_aridity("https://api.figshare.com/v2/file/download/34377245"),
             format = "file"),
  tar_target(ai_file_names, global_ai), # not a format = "file" target
  tar_target(
    files, {
      global_ai # Re-hash all the files separately if any file changes.
      ai_file_names
    },
    pattern = map(ai_file_names),
    format = "file"
  ),
  
  tar_target(nlcd, download_nlcd("https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip"),
             format = "file"),
  tar_target(nlcd_file_names, nlcd), # not a format = "file" target
  tar_target(
    nlcd_files, {
      nlcd # Re-hash all the files separately if any file changes.
      nlcd_file_names
    },
    pattern = map(nlcd_file_names),
    format = "file"
  ),
  
  ## add aridity index to data -------------------------------------------------
  tar_target(bac_model_df, add_aridity(bacteria_df)),
  tar_target(nut_model_df, add_aridity(nutrient_df)),
  
  # Summary stats
  tar_target(study_count, study_count_summary(bacteria_df,
                                              nutrient_df)),
  
  
  # Bacteria data analysis -----------------------------------------------------
  ## fit multilevel random effects model ---------------------------------------
  tar_target(bac_model, fit_bac_model(bac_model_df$model_df)),
  
  ## Diagnostics and assessment ------------------------------------------------
  ## small study effect/publication bias using extension of egger's regression
  tar_target(bac_sse, fit_sse_model(bac_model)),

  ## leave one out sensitivity analysis identifies outliers or influential studies having disproportionate effects on the model estimates
  tar_target(bac_sens, run_sensitvity(bac_model)),

  ## missing SDs
  tar_target(bac_missing_sd, assess_missing(bac_model)),

  # i2
  # i2_total represents the relative variance due to difference between and within studies
  # i2_studyid represent the relative variance variation attributed to difference between studies
  # i2_valueid represents the relative variation due to difference within studies.
  tar_target(bac_i2_null, i2_ml(bac_model$null, method = "ratio")),
  tar_target(bac_i2, i2_ml(bac_model$sel_mod, method = "ratio")),

  ## marginal r2 quantifies the percentage of variation explained by the moderators.
  ## condition r2 quanitifes varience explained by both fixed and random components of the model
  # nakagawa 2013)
  tar_target(bac_r2, r2_ml(bac_model$sel_mod)),

  ## generate forest plot
  tar_target(bac_forest_plot, plot_forest(model = bac_model$null)),
  
  ## plot conditional means from the model
  tar_target(bac_marginal_effect_conc,
             plot_marginal_effect_lnRR(bac_model$sel_mod,
                                       mod = "lnPre",
                                       by = NULL,
                                       xlab = "Influent FIB (counts/100mL)",
                                       ylab = "ROM",
                                       sec_y_breaks = c(-1000,-100,0,40,80,99),
                                       sec_y_labels = c("-1000","-100","0","40","80",">99"),
                                       ylim = c(-5,5))),
  
  tar_target(bac_marginal_effect_ai,
             plot_marginal_effect_lnRR(bac_model$sel_mod,
                                       mod = "ai",
                                       by = "BMP_SubCat",
                                       xlab = "Aridity Index",
                                       ylab = "Predicted marginal effect (ROM)",
                                       sec_y_breaks = c(-1000,-100,0,40,80,99),
                                       sec_y_labels = c("-1000","-100","0","40","80",">99"),
                                       ylim = c(-5,5),
                                       x_adj = bac_model_df$mean)),

  ## generate model summary table
  tar_target(bac_table, tidy_metafor(bac_model$sel_mod,
                                     terms = c("Intercept",
                                               "Aridity Index",
                                               "Infiltration",
                                               "Livestock Management",
                                               "Treatment",
                                               "log(Influent)",
                                               "Aridity:Infiltration",
                                               "Aridity:Livestock",
                                               "Aridity:Treatment"),
                                     i2 = bac_i2,
                                     r2 = bac_r2,
                                     caption = "Summary table of multilevel random effects model for effect of BMPs on fecal indicator bacteria concentrations.")),



  # TN data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tn_model, fit_tn_model(nut_model_df$model_df)),
  # ## Diagnostics and assessment ------------------------------------------------
  tar_target(tn_sse, fit_sse_model(tn_model)),
  tar_target(tn_sens, run_sensitvity(tn_model)),
  tar_target(tn_missing_sd, assess_missing(tn_model)),
  tar_target(tn_i2_null, i2_ml(tn_model$null, method = "ratio")),
  tar_target(tn_i2, i2_ml(tn_model$sel_model, method = "ratio")),
  tar_target(tn_r2, r2_ml(tn_model$sel_model)),
  tar_target(tn_forest_plot, plot_forest(model = tn_model$null)),

  tar_target(tn_table, tidy_metafor(tn_model$sel_model,
                                    terms = c("Intercept"),
                                    i2 = tn_i2,
                                    r2 = tn_r2,
                                    caption = "Summary table of multilevel random effects model for effect of BMPs on total nitrogen removal.")),

  # TIN data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tin_model, fit_tin_model(nut_model_df$model_df)),
  # ## Diagnostics and assessment ------------------------------------------------
  tar_target(tin_sse, fit_sse_model(tin_model)),
  tar_target(tin_sens, run_sensitvity(tin_model)),
  tar_target(tin_missing_sd, assess_missing(tin_model)),
  tar_target(tin_i2_null, i2_ml(tin_model$null, method = "ratio")),
  tar_target(tin_i2, i2_ml(tin_model$sel_model, method = "ratio")),
  tar_target(tin_r2, r2_ml(tn_model$sel_model)),
  tar_target(tin_forest_plot, plot_forest(model = tin_model$null)),
  
  tar_target(tin_table, tidy_metafor(tin_model$sel_model,
                                    terms = c("Intercept"),
                                    i2 = tn_i2,
                                    r2 = tn_r2,
                                    caption = "Summary table of multilevel random effect model for effect of BMPs on inorganic nitrogen removal.")),
  
  # TP data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tp_model, fit_tp_model(nut_model_df$model_df)),
  ## Diagnostics and assessment ------------------------------------------------
  tar_target(tp_sse, fit_sse_model(tp_model)),
  tar_target(tp_sens, run_sensitvity(tp_model)),
  tar_target(tp_missing_sd, assess_missing(tp_model)),
  tar_target(tp_i2_null, i2_ml(tp_model$null, method = "ratio")),
  tar_target(tp_i2, i2_ml(tp_model$sel_mod, method = "ratio")),
  tar_target(tp_r2, r2_ml(tp_model$sel_mod)),
  tar_target(tp_forest_plot, plot_forest(model = tp_model$null)),
  tar_target(tp_conc_marginal_means, plot_marginal_effect_lnRR(tp_model$sel_model,
                                                               mod = "lnPre",
                                                               by = NULL,
                                                               xlab = "Influent TP (mg/L)",
                                                               ylab = "ROM",
                                                               sec_y_breaks = c(-1000,-100,0,40,80,99),
                                                               sec_y_labels = c("-1000","-100","0","40","80",">99"),
                                                               ylim = c(-3,3))),
  tar_target(tp_table, tidy_metafor(tp_model$sel_model,
                                    terms = c("Intercept",
                                              "log(Influent)"),
                                    i2 = tp_i2,
                                    r2 = tp_r2,
                                    caption = "Summary table of multilevel random effect model for effect of BMPs on total phosphorus removal.")),
  
  
  # PO4 data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(po4_model, fit_po4_model(nut_model_df$model_df)),
  ## Diagnostics and assessment ------------------------------------------------
  tar_target(po4_sse, fit_sse_model(po4_model)),
  tar_target(po4_sens, run_sensitvity(po4_model)),
  tar_target(po4_missing_sd, assess_missing(po4_model)),
  tar_target(po4_i2_null, i2_ml(po4_model$null, method = "ratio")),
  tar_target(po4_i2, i2_ml(po4_model$sel_mod, method = "ratio")),
  tar_target(po4_r2, r2_ml(po4_model$sel_mod)),
  tar_target(po4_forest_plot, plot_forest(model = po4_model$null)),
  tar_target(po4_conc_marginal_means, plot_marginal_effect_lnRR(po4_model$sel_model,
                                                               mod = "lnPre",
                                                               by = NULL,
                                                               xlab = "Influent PO4 (mg/L)",
                                                               ylab = "ROM",
                                                               sec_y_breaks = c(-1000,-100,0,40,80,99),
                                                               sec_y_labels = c("-1000","-100","0","40","80",">99"),
                                                               ylim = c(-3,3))),
  tar_target(po4_table, tidy_metafor(po4_model$sel_model,
                                    terms = c("Intercept",
                                              "log(Influent)"),
                                    i2 = tp_i2,
                                    r2 = tp_r2,
                                    caption = "Summary table of multilevel random effect model for effect of BMPs on PO4 removal.")),

  # TSS data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tss_model, fit_tss_model(nut_model_df$model_df)),
  ## Diagnostics and assessment ------------------------------------------------
  tar_target(tss_sse, fit_sse_model(tss_model)),
  tar_target(tss_sens, run_sensitvity(tss_model)),
  tar_target(tss_missing_sd, assess_missing(tss_model)),
  tar_target(tss_i2_null, i2_ml(tss_model$null, method = "ratio")),
  tar_target(tss_i2, i2_ml(tss_model$sel_model, method = "ratio")),
  tar_target(tss_r2, r2_ml(tss_model$sel_model)),
  tar_target(tss_forest_plot, plot_forest(model = tss_model$null)),

  tar_target(tss_table, tidy_metafor(tss_model$sel_model,
                                    terms = c("Intercept"),
                                    i2 = tss_i2,
                                    r2 = tss_r2,
                                    caption = "Summary table of multilevel random effect model for effect of BMPs on TSS removal.")),
  
  
  # Some post-hoc analysis for reviewers ---------------------------------------
  ## Sensitivity analysis to the runoff type (ag or urban)
  tar_target(bac_source, source_sensitvity(bac_model)),
  tar_target(tn_source, source_sensitvity(tn_model)),
  tar_target(tin_source, source_sensitvity(tin_model)),
  tar_target(tp_source, source_sensitvity(tp_model)),
  tar_target(po4_source, source_sensitvity(po4_model)),
  tar_target(tss_source, source_sensitvity(tss_model)),
  
  ## Sensitivity to scale
  tar_target(bac_scale, scale_sensitvity(bac_model)),
  tar_target(tn_scale, scale_sensitvity(tn_model)),
  tar_target(tin_scale, scale_sensitvity(tin_model)),
  tar_target(tp_scale, scale_sensitvity(tp_model)),
  tar_target(po4_scale, scale_sensitvity(po4_model)),
  tar_target(tss_scale, scale_sensitvity(tss_model)),
  
  

  # Figures for Manuscript -----------------------------------------------------
  tar_target(study_map, plot_map(bacteria_df, nutrient_df)),
  tar_target(study_map_file, ggsave("figures/study_map.pdf",
                                    plot = study_map,
                                    device = cairo_pdf,
                                    width = 6,
                                    height = 6.5,
                                    units = "in"),
             format = "file"),
  
  tar_target(bmp_summary, plot_bmp_summary(bacteria_df, nutrient_df)),
  tar_target(bmp_summary_file, ggsave("figures/bmp_summary.pdf",
                                    plot = bmp_summary,
                                    device = cairo_pdf,
                                    width = 6,
                                    height = 3,
                                    units = "in"),
             format = "file"),
  
  tar_target(bmp_summary_param, plot_bmp_param_summary(bacteria_df, nutrient_df)),
  tar_target(bmp_summary_param_file, ggsave("figures/bmp_summary_param.pdf",
                                      plot = bmp_summary_param,
                                      device = cairo_pdf,
                                      width = 6,
                                      height = 6,
                                      units = "in"),
             format = "file"),

  tar_target(bmp_temporal, plot_bmp_temporal(bacteria_df, nutrient_df)),
  tar_target(bmp_temporal_file, ggsave("figures/bmp_temporal_summary.pdf",
                                      plot = bmp_temporal$p1,
                                      device = cairo_pdf,
                                      width = 6,
                                      height = 6.5,
                                      units = "in"),
             format = "file"),

  tar_target(overall_effect, mult_orch_plot(bac_model$null,
                                            tn_model$null,
                                            tin_model$null,
                                            tp_model$null,
                                            po4_model$null,
                                            tss_model$null)),

  tar_target(overall_effect_file, ggsave("figures/overall_effect.pdf",
                                         plot = overall_effect,
                                         device = cairo_pdf,
                                         width = 6.5,
                                         height = 6.5,
                                         units = "in"),
             format = "file"),
  
  tar_target(bac_marginal_means, patch_bac_marginal_means(bac_marginal_effect_conc,
                                                          bac_marginal_effect_ai)),

  
  tar_target(bac_marginal_means_file, ggsave("figures/bac_predicted.pdf",
                                         plot = bac_marginal_means,
                                         device = cairo_pdf,
                                         width = 8.5,
                                         height = 5,
                                         units = "in"),
             format = "file"),

  tar_target(phos_marginal_means, patch_phos_marginal_means(tp_conc_marginal_means,
                                                            po4_conc_marginal_means)),
  
  tar_target(phos_marginal_means_file, ggsave("figures/phos_predicted.pdf",
                                             plot = phos_marginal_means,
                                             device = cairo_pdf,
                                             width = 8.5,
                                             height = 3.5,
                                             units = "in"),
             format = "file"),
  
  tar_target(aridity_landuse, crop_aridity_to_land_use()),
  
  tar_target(aridity_density, plot_ai_summary(bac_model_df,
                                              nut_model_df,
                                              aridity_landuse)),
  
  tar_target(aridity_file, ggsave("figures/aridity_density.pdf",
                                  plot = aridity_density,
                                  device = cairo_pdf,
                                  width = 4.5,
                                  height = 3.5,
                                  units = "in"),
             format = "file"),

  NULL
  )