## create a flextable output
tidy_metafor <- function(model, 
                         conf.level = 0.95, 
                         terms = NULL,
                         i2 = NULL,
                         r2 = NULL,
                         caption = NULL) {
  
  s_data <- summary(model)
  
  term_tab <- broom::tidy(model,
                          conf.int = TRUE,
                          conf.level = conf.level) |> 
    mutate(estimate = prettyNum(estimate, digits = 2, nsmall = 2),
           std.error = prettyNum(std.error, digits = 2, nsmall = 2),
           statistic = prettyNum(statistic, digits = 2, nsmall = 2),
           df = s_data$ddf,
           `95\\% CI` = paste0("[",
                             prettyNum(conf.low, digits = 2, nsmall = 2), 
                             ",", 
                             prettyNum(conf.high, digits = 2, nsmall = 2),
                             "]"),
           `p-value`= format.pval(p.value, eps = .01, digits = 2, nsmall = 1)) |>
    mutate(`p-value` = gsub('<', '\\\\textless ', `p-value`)) |> 
    select(Term = term,
           `Estimate\\nLog response ratio` = estimate,
           `95\\% CI`,
           SE = std.error,
           statistic,
           df,
           `\\textit{p}-value` = `p-value`)
  
  if(!is.null(terms)) {
    term_tab <- term_tab |> 
      mutate(Term = terms)

  }
  
  kableExtra::kbl(term_tab,
                  format = "latex",
                  escape = FALSE,
                  booktabs = TRUE,
                  caption = caption,
                  col.names = linebreak(c("Moderator", 
                                          "Estimate\n(\\textit{lnRR})",
                                          "95\\% CI",
                                          "SE",
                                          "\\textit{T}-statistic",
                                          "df",
                                          "\\textit{p}-value"),
                                        align = c("l", "r", "l", "r", "r", "r")),
                  align = c("l", "r", "l", "r", "r", "r")
                  )|>
    # add_indent(c(5:7, 9:10, 12:15)) |> 
    footnote(general = paste0("$I^{2}_{\\\\text{total}}$=",round(i2[[1]], 2), 
                              ", $I^{2}_{\\\\text{study}}$=",round(i2[[2]], 2),
                              ", $I^{2}_{\\\\text{effect}}$=",round(i2[[3]], 2),
                              "; $R^{2}_{\\\\text{marginal}}$=",round(r2[[1]], 2)),
             general_title = "",
             escape = FALSE)
  

}

# https://stackoverflow.com/questions/49015578/space-after-every-five-rows-in-kable-output-with-booktabs-option-in-r-markdown
linesep <- function(x,y=character()){
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'\\addlinespace',y))  
}