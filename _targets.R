# _targets.R
library(targets)

tar_option_set(packages = c("broom",
                            "dplyr",
                            "flextable",
                            "forcats",
                            "ggplot2",
                            "ggtext",
                            "ggrepel",
                            "kableExtra",
                            "latex2exp",
                            "metafor",
                            "metagear",
                            "mice",
                            "modelr",
                            "mpsTemplates",
                            "MuMIn",
                            "officer",
                            "orchaRd",
                            "patchwork",
                            "purrr",
                            "RColorBrewer",
                            "readxl",
                            "scales",
                            "sf",
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

## prep fancy figure font
font_hoist("Manrope")

## for runnning dredge() in MuMIn on rma models
eval(metafor:::.MuMIn)




list(
  tar_target(bacteria_df, load_df("bacteria")),
  tar_target(nutrient_df, load_df("nutrients")),
  # Bacteria data analysis -----------------------------------------------------
  ## fit multilevel random effects model ---------------------------------------
  tar_target(bac_model, fit_bac_model(bacteria_df)),
  
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
  tar_target(bac_i2, i2_ml(bac_model$sel_mod, method = "ratio")),

  ## marginal r2 quantifies the percentage of variation explained by the moderators.
  ## condition r2 quanitifes varience explained by both fixed and random components of the model
  # nakagawa 2013)
  tar_target(bac_r2, r2_ml(bac_model$sel_mod)),

  ## generate forest plot
  tar_target(bac_forest_plot, plot_forest(model = bac_model$null)),
  
  ## plot conditional means from the model
  tar_target(bac_conc_marginal_effect_lnRR,
             plot_marginal_effect_lnRR(bac_model$sel_mod,
                                       mod = "lnPre",
                                       by = "BMP_SubCat",
                                       xlab = "Fecal indicator bacteria concentration (counts/100mL)",
                                       ylab = "Predicted marginal effect (ROM)",
                                       sec_y_breaks = c(-1000,-100,0,40,80,99),
                                       sec_y_labels = c("-1000","-100","0","40","80",">99"))),


  ## generate model summary table
  tar_target(bac_table, tidy_metafor(bac_model$sel_mod,
                                     terms = c("Intercept",
                                               "Filtration",
                                               "Infiltration",
                                               "Livestock",
                                               "Treatment",
                                               "log(Concentration)"),
                                     i2 = bac_i2,
                                     r2 = bac_r2,
                                     caption = "Summary table of moderator effects for performance of BMPs on fecal indicator bacteria concentrations.")),



  # TN data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tn_model, fit_tn_model(nutrient_df)),
  # ## Diagnostics and assessment ------------------------------------------------
  tar_target(tn_sse, fit_sse_model(tn_model)),
  tar_target(tn_sens, run_sensitvity(tn_model)),
  tar_target(tn_missing_sd, assess_missing(tn_model)),
  tar_target(tn_i2, i2_ml(tn_model$sel_adj_model, method = "ratio")),
  tar_target(tn_r2, r2_ml(tn_model$sel_adj_model)),
  tar_target(tn_forest_plot, plot_forest(model = tn_model$null)),
  tar_target(tn_conc_marginal_means, plot_marginal_effect_lnRR(tn_model$sel_adj_model,
                                                               mod = "lnPre",
                                                               by = NULL,
                                                               xlab = "Nitrogen concentration (mg/L)",
                                                               ylab = "Predicted marginal effect (ROM)",
                                                               sec_y_breaks = c(-400,-200,-100,0,40,80,99),
                                                               sec_y_labels = c("-400","-200","-100","0","40","80",">99"))),
  tar_target(tn_table, tidy_metafor(tn_model$sel_adj_model,
                                    terms = c("Intercept",
                                              "log(Concentration)",
                                              "Adjusted sampling error"),
                                    i2 = tn_i2,
                                    r2 = tn_r2,
                                    caption = "Summary table of moderator effects for performance of BMPs on nitrogen removal.")),


  # TP data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tp_model, fit_tp_model(nutrient_df)),
  # ## Diagnostics and assessment ------------------------------------------------
  tar_target(tp_sse, fit_sse_model(tp_model)),
  tar_target(tp_sens, run_sensitvity(tp_model)),
  tar_target(tp_missing_sd, assess_missing(tp_model)),
  tar_target(tp_i2, i2_ml(tp_model$sel_mod, method = "ratio")),
  tar_target(tp_r2, r2_ml(tp_model$sel_mod)),
  tar_target(tp_forest_plot, plot_forest(model = tp_model$null)),
  tar_target(tp_conc_marginal_means, plot_marginal_effect_lnRR(tp_model$sel_model,
                                                               mod = "lnPre",
                                                               by = NULL,
                                                               xlab = "Phosphorus concentration (mg/L)",
                                                               ylab = "Predicted effect (ROM)",
                                                               sec_y_breaks = c(-1000,-100,-10,0,40,80,99),
                                                               sec_y_labels = c("-1000","-100","-10","0","40","80",">99"))),
  tar_target(tp_table, tidy_metafor(tp_model$sel_model,
                                    terms = c("Intercept",
                                              "log(Concentration)"),
                                    i2 = tp_i2,
                                    r2 = tp_r2,
                                    caption = "Summary table of moderator effects for performance of BMPs on phosphorus removal.")),

  # TSS data analysis
  ## Fit model -----------------------------------------------------------------
  tar_target(tss_model, fit_tss_model(nutrient_df)),
  # ## Diagnostics and assessment ------------------------------------------------
  tar_target(tss_sse, fit_sse_model(tss_model)),
  tar_target(tss_sens, run_sensitvity(tss_model)),
  tar_target(tss_missing_sd, assess_missing(tss_model)),
  tar_target(tss_i2, i2_ml(tss_model$sel_model, method = "ratio")),
  tar_target(tss_r2, r2_ml(tss_model$sel_model)),
  tar_target(tss_forest_plot, plot_forest(model = tss_model$null)),
  tar_target(tss_marginal_means, plot_marginal_effect_lnRR(tss_model$sel_model,
                                                           mod = "study_length",
                                                           by = NULL,
                                                           xlab = "Study length (years)",
                                                           ylab = "Predicted effect (ROM)",
                                                           sec_y_breaks = c(-1000,-100,0,40,80,99),
                                                           sec_y_labels = c("-1000","-100","0","40","80",">99"))),
  tar_target(tss_table, tidy_metafor(tss_model$sel_model,
                                    terms = c("Intercept",
                                              "Study length"),
                                    i2 = tss_i2,
                                    r2 = tss_r2,
                                    caption = "Summary table of moderator effects for performance of BMPs on TSS removal.")),

  # Figures for Manuscript
  tar_target(study_map, plot_map(bacteria_df, nutrient_df)),
  tar_target(study_map_file, ggsave("figures/study_map.pdf",
                                    plot = study_map,
                                    device = cairo_pdf,
                                    width = 8.5,
                                    height = 6,
                                    units = "in"),
             format = "file"),
  
  tar_target(bmp_summary, plot_bmp_summary(bacteria_df, nutrient_df)),
  tar_target(bmp_summary_file, ggsave("figures/bmp_summary.pdf",
                                    plot = bmp_summary,
                                    device = cairo_pdf,
                                    width = 8.5,
                                    height = 6,
                                    units = "in"),
             format = "file"),

  tar_target(overall_effect, mult_orch_plot(bac_model$null,
                                            tn_model$null,
                                            tp_model$null,
                                            tss_model$null)),

  tar_target(overall_effect_file, ggsave("figures/overall_effect.pdf",
                                         plot = overall_effect,
                                         device = cairo_pdf,
                                         width = 8.5,
                                         height = 6,
                                         units = "in"),
             format = "file"),
  
  

  # tar_target(marginal_means, patch_marginal_means(plots = list(bac_marginal_means,
  #                                                              tn_marginal_means,
  #                                                              tp_marginal_means,
  #                                                              tss_marginal_means))),
  # tar_target(marginal_means_file, ggsave("figures/marginal_means.pdf",
  #                                        plot = marginal_means,
  #                                        device = cairo_pdf,
  #                                        width = 8.5,
  #                                        height = 8.5,
  #                                        units = "in"),
  #            format = "file"),

  NULL
  )