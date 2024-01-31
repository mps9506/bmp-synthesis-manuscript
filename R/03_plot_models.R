## load custom fonts
## https://yjunechoe.github.io/posts/2021-06-24-setting-up-and-debugging-custom-fonts/
font_hoist <- function(family, silent = FALSE) {
  font_specs <- systemfonts::system_fonts() |> 
    dplyr::filter(family == {{family}}) |> 
    dplyr::mutate(family = paste(family, style)) |> 
    dplyr::select(plain = path, name = family)
  
  purrr::pwalk(as.list(font_specs), systemfonts::register_font)
  
  if (!silent)  message(paste0("Hoisted ", nrow(font_specs), " variants:\n",
                               paste(font_specs$name, collapse = "\n")))
}





#' Plots the estimated marginal means from the model with bubble_plot
#'
#' @param model rma model object
#' @param xlab character object with the x-axis label
#' @param ... additional arguments for bubble plot
#'
#' @return ggobject

plot_marginal_effect_lnRR <- function(model,
                                      xlab,
                                      ylab,
                                      mod = "lnPre",
                                      by = "BMP_SubCat",
                                      at = NULL,
                                      subset = NULL,
                                      sec_y_breaks = waiver(),
                                      sec_y_labels = waiver(),
                                      ylim = NULL,
                                      x_adj = NULL,
                                      ...) {
  
  ## use mod_results from orchaRd to generate model grid 
  mod_table <- mod_results(model, 
                           mod = mod, 
                           group = "ValueID", 
                           weights = "prop", 
                           by = by,
                           at = at,
                           subset = subset)
  
  ## back transform lnPre for plotting purposes
  ##would like to back transform the moderator
  if(mod == "lnPre"){
    mod_table <- as.list(mod_table) |> 
      purrr::modify_at("mod_table",
                       ~mutate(.,
                               moderator = exp(moderator))) |> 
      purrr::modify_at("data",
                       ~mutate(.,
                               moderator = exp(moderator)))    
  }

  ## back transform centered ai data
  if(mod == "ai") {
    mod_table <- as.list(mod_table) |> 
      purrr::modify_at("mod_table",
                       ~mutate(.,
                               moderator = moderator + x_adj)) |> 
      purrr::modify_at("data",
                       ~mutate(.,
                               moderator = moderator + x_adj))    
  }
  
  ## remove NA values from "data"
  if(!is.null(at)){
    mod_table <- mod_table |> 
      purrr::modify_at("data",
                       ~filter(.,
                               condition %in% at$BMP_SubCat))
  }
  
  if(!inherits(mod_table, "orchard")) {
    class(mod_table) <- c("orchard", "data.frame") 
  }
  
  ## create manual palette
  if(!is.null(by)) {
    if(by == "BMP_SubCat") {
      pal_vals <- RColorBrewer::brewer.pal(7, "Dark2")
      names(pal_vals) <- c("Cropland", "Detention", "Drainage", "Filtration", "Infiltration", "Livestock", "Treatment")
    }
  } else {
    pal_vals <- RColorBrewer::brewer.pal(3, "Dark2")[[2]]
  }
  
  ## plot
  if(is.null(by)){
    p1 <- ggplot(mod_table$mod_table) +
      geom_hline(yintercept = 0, alpha = 0.25, linetype = "solid", color = "gray75") +
      geom_ribbon(aes(x = moderator, ymin = lowerPR, ymax = upperPR),
                  alpha = 0.2,
                  fill = "#386CB0",
                  colour = "#386CB0",
                  linetype = "dotted") +
      geom_ribbon(aes(x = moderator, ymin = lowerCL, ymax = upperCL),
                  alpha = 0.2,
                  fill = "#386CB0",
                  colour = "#386CB0",
                  linetype = "dashed") +
      geom_line(aes(moderator, estimate),
                color = "#33507D",
                linewidth = 1,) +
      geom_point(data = mod_table$data, aes(moderator, yi, size = 1/vi),
                 shape = 21, fill = "gray70", alpha = 0.5) 
  }
  if(!is.null(by)) {
    if(by == "BMP_SubCat") {
      p1 <- ggplot(mod_table$mod_table) +
        geom_hline(yintercept = 0, alpha = 0.25, linetype = "solid", color = "gray75") +
        geom_ribbon(aes(x = moderator, ymin = lowerPR, ymax = upperPR,
                        color = condition, fill = condition),
                    alpha = 0.2,
                    linetype = "dotted") +
        geom_ribbon(aes(x = moderator, ymin = lowerCL, ymax = upperCL,
                        color = condition, fill = condition),
                    alpha = 0.2,
                    linetype = "dashed") +
        geom_line(aes(moderator, estimate,
                      color = condition),
                  linewidth = 1,) +
        geom_point(data = mod_table$data, aes(moderator, yi, size = 1/vi),
                   shape = 21, fill = "gray70", alpha = 0.5) +
        facet_wrap(~condition)
    }
  }
  
  p1 <- p1 +
    scale_y_continuous(sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_radius(trans = "sqrt") +
    labs(x = xlab,
         y = ylab) +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.title.y.right = element_text(hjust = 0, angle = 270),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.8)),
          axis.text.y.left = element_text(hjust = 1),
          axis.text.y.right = element_text(hjust = 0),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90",
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black",
                                    face = "plain",
                                    size = rel(1), hjust = 0))

  if(mod == "lnPre") {
    p1 <- p1 +
      scale_x_log10()
  }
  
  if(!is.null(by)) {
    if(by == "BMP_SubCat") {
      p1 <- p1 +
        scale_fill_manual(values = pal_vals) +
        scale_color_manual(values = pal_vals)
    }    
  }
  if(!is.null(ylim)) {
    p1 <- p1 +
      coord_cartesian(ylim = ylim)
  }


  return(p1)
}



patch_marginal_means <- function(plots) {
  
  ## trying to fine tune alignment of the facets
  layout <- "
  AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA########
  BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB
  CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
  DDDDDDDDDDDDDDDDDDDDDDDDD###############
  "
  patchwork::wrap_plots(plots, ncol = 1, design = layout)
}


patch_bac_marginal_means <- function(p1, p2) {
  
  p1 <- p1 +
    theme(axis.title.y.right = element_blank())
  p2 <- p2 +
    theme(axis.title.y.left = element_blank())
  
  p1 + p2 + patchwork::plot_annotation(tag_levels = "A")
}

patch_phos_marginal_means <- function(p1, p2) {
  
  p1 <- p1 +
    theme(axis.title.y.right = element_blank())
  p2 <- p2 +
    theme(axis.title.y.left = element_blank())
  
  p1 + p2 + patchwork::plot_annotation(tag_levels = "A")
  
}


plot_forest <- function(model) {
  tibble(Effect = model$yi,
         SE = 1/model$vi,
         weight = weights(model),
         ylab = attr(model$yi, "slab"),
         BMP = model$data$BMP_SubCat) |> 
    mutate(ylab = forcats::fct_reorder(ylab, Effect)) |> 
    ggplot(aes(x = Effect, y = ylab)) +
    geom_pointrange(aes(xmin = Effect-(1.96 * SE),
                        xmax = Effect+(1.96 * SE),
                        size = weight)) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    facet_col(vars(BMP),
              scales = "free",
              space = "free") +
    scale_size(range = c(0.05,0.75)) +
    labs(x = "ROM", y = "") +
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
  
}



#' Lightly customized bubble plot function lifted from orchaRd
#'
#' @param object 
#' @param mod 
#' @param group 
#' @param xlab 
#' @param ylab 
#' @param N 
#' @param alpha 
#' @param cb 
#' @param k 
#' @param g 
#' @param est.lwd 
#' @param ci.lwd 
#' @param pi.lwd 
#' @param est.col 
#' @param ci.col 
#' @param pi.col 
#' @param legend.pos 
#' @param k.pos 
#' @param condition.nrow 
#' @param weights 
#' @param by 
#' @param at 
#'
#' @return ggobject

bubble_plot <- function(object, mod, group = NULL, xlab = "Moderator", 
                        ylab = "Effect size", N = "none",
                        alpha = 0.2, cb = TRUE, k = TRUE, g = FALSE,
                        est.lwd = 1, ci.lwd = 0.5, pi.lwd = 0.5,
                        est.col = "#386CB0", ci.col = "#386CB0", pi.col = "#386CB0",
                        legend.pos = c("top.left", "top.right",
                                       "bottom.right", "bottom.left",
                                       "top.out", "bottom.out",
                                       "none"),
                        k.pos = c("top.right", "top.left",
                                  "bottom.right", "bottom.left",
                                  "none"),
                        condition.nrow = 2,
                        #condition.lab = "Condition",
                        weights = "prop", by = NULL, at = NULL,
                        fill = "gray70")
{
  legend.pos <- match.arg(NULL, choices = legend.pos)
  k.pos <- match.arg(NULL, choices = k.pos)
  #facet <- match.arg(NULL, choices = facet)
  
  if(missing(group)){
    stop("Please specify the 'group' argument by providing the name of the grouping variable. See ?bubble_plot")
  }
  
  if(is.numeric(by)){
    k = FALSE
    g = FALSE
  }
  
  
  if(any(class(object) %in% c("robust.rma", "rma.mv", "rma", "rma.uni"))){
    
    if(mod != "1"){
      results <-  orchaRd::mod_results(object, mod, group,
                                       by = by, at = at, weights = weights)
    } else {
      results <-  orchaRd::mod_results(object, mod = "1", group,
                                       by = by, at = at, weights = weights)
    }
  }
  
  if(any(class(object) %in% c("orchard"))) {
    results <- object
  }
  
  mod_table <- results$mod_table
  
  data_trim <- results$data
  
  data_trim$scale <- (1/sqrt(data_trim[,"vi"]))
  legend <- "Precision (1/SE)"
  
  if(any(N != "none")){
    data_trim$scale <- data_trim$N
    legend <- paste0("Sample Size ($\\textit{N}$)") # we want to use italic
  }
  
  label <- xlab
  # if(transfm == "tanh"){
  #   cols <- sapply(mod_table, is.numeric)
  #   mod_table[,cols] <- Zr_to_r(mod_table[,cols])
  #   data_trim$yi <- Zr_to_r(data_trim$yi)
  #   label <- xlab
  # }else{
  #   label <- xlab
  # }
  
  if(is.null(data_trim$condition) == TRUE){
    
    # the number of effect sizes
    effect_num <- nrow(data_trim)
    
    # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
    group_num <- length(unique(data_trim$stdy))
    
    dat_text <- data.frame(K = effect_num, G = group_num)
    
  }else{
    
    # making sure factor names match
    data_trim$condition <- factor(data_trim$condition, levels = mod_table$condition, labels = mod_table$condition)
    
    effect_num <- as.vector(by(data_trim, data_trim[,"condition"], function(x) base::length(x[,"yi"])))
    
    # Add in total levels of a grouping variable (e.g., study ID) within each moderator level.
    #group_num <- c(2,4)
    group_num <- as.vector(by(data_trim, data_trim[,"condition"], function(x) base::length(base::unique(x[,"stdy"]))))
    
    dat_text <- data.frame(K = effect_num, G = group_num, condition = as.vector(base::levels(data_trim$condition)))
  }
  # the number of groups in a moderator & data points
  #group_no <- length(unique(mod_table[, "name"]))
  
  #data_no <- nrow(data)
  
  # # colour blind friendly colours with grey
  # cbpl <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
  
  if(is.null(data_trim$condition) == TRUE){
    plot <-ggplot2::ggplot() +
      # putting bubbles
      ggplot2::geom_point(data = data_trim, ggplot2::aes(x = moderator, y = yi, size = scale), shape = 21, alpha = alpha, fill = fill ) +
      # prediction interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd, colour = pi.col) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR), method =  "loess", formula = y~x, se = FALSE, lty = "dotted", lwd = pi.lwd, colour = pi.col) +
      # confidence interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL), method =  "loess", formula = y~x, se = FALSE,lty = "dashed", lwd = ci.lwd, colour = ci.col) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL), method =  "loess", formula = y~x, se = FALSE, lty ="dashed", lwd = ci.lwd, colour = ci.col) +
      # main line
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd, colour = est.col) +
      #facet_grid(rows = vars(condition)) +
      ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
      ggplot2::guides(fill = "none", colour = "none") +
      # themes
      ggplot2::theme_bw() +
      #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
      ggplot2::theme(legend.direction="horizontal") +
      #theme(legend.background = element_rect(fill = "white", colour = "black")) +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
  } else if(is.character(data_trim$condition) == TRUE || is.factor(data_trim$condition) == TRUE){
    
    plot <-ggplot2::ggplot() +
      # putting bubbles
      ggplot2::geom_point(data = data_trim, ggplot2::aes(x = moderator, y = yi, size = scale, fill = condition), shape = 21, alpha = alpha) +
      # prediction interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR, color = condition), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR, color = condition), method =  "loess", formula = y~x,se = FALSE, lty = "dotted", lwd = pi.lwd) +
      # confidence interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL, color = condition), method =  "loess", formula = y~x,se = FALSE,lty = "dashed", lwd = ci.lwd) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL, color = condition), method =  "loess", formula = y~x,se = FALSE, lty ="dashed", lwd = ci.lwd) +
      # main line
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate, color = condition), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd) +
      ggplot2::facet_wrap(ggplot2::vars(condition), nrow = condition.nrow) +
      ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
      ggplot2::guides(fill = "none", colour = "none") +
      # themses
      ggplot2::theme_bw() +
      #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
      ggplot2::theme(legend.direction="horizontal") +
      #theme(legend.background = element_rect(fill = "white", colour = "black")) +
      ggplot2::theme(legend.background = ggplot2::element_blank()) +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
    
    # if(facet == "rows"){
    #   plot <- plot + facet_grid(rows = vars(condition))
    # } else{
    #   plot <- plot + facet_grid(cols = vars(condition))
    # }
    
    
  } else{
    plot <-ggplot2::ggplot() +
      # putting bubbles
      #geom_point(data = data, aes(x = moderator, y = yi, size = scale), shape = 21, alpha = alpha, fill = "grey90" ) +
      # prediction interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerPR), method =  "loess", formula = y~x, se = FALSE, lty =  "dotted", lwd = pi.lwd, colour = pi.col) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperPR), method =  "loess", formula = y~x,se = FALSE, lty = "dotted", lwd = pi.lwd, colour = pi.col) +
      # confidence interval
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = lowerCL), method =  "loess", formula = y~x,se = FALSE,lty = "dashed", lwd = ci.lwd, colour = ci.col) +
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = upperCL), method =  "loess", formula = y~x,se = FALSE, lty ="dashed", lwd = ci.lwd, colour = ci.col) +
      # main line
      ggplot2::geom_smooth(data = mod_table, ggplot2::aes(x = moderator, y = estimate), method =  "loess", formula = y~x, se = FALSE, lwd = est.lwd, colour = est.col) +
      ggplot2::facet_wrap(ggplot2::vars(condition), nrow = condition.nrow) +
      ggplot2::labs(x = xlab, y = ylab, size = legend, parse = TRUE) +
      ggplot2::guides(fill = "none", colour = "none") +
      # themses
      ggplot2::theme_bw() # +
    #theme(legend.position= c(1, 1), legend.justification = c(1, 1)) +
    # theme(legend.direction="horizontal") +
    # #theme(legend.background = element_rect(fill = "white", colour = "black")) +
    # theme(legend.background = element_blank()) +
    # theme(axis.text.y = element_text(size = 10, colour ="black", hjust = 0.5, angle = 90))
  }
  
  # adding legend
  if(legend.pos == "bottom.right"){
    plot <- plot + ggplot2::theme(legend.position= c(1, 0), legend.justification = c(1, 0))
  } else if ( legend.pos == "bottom.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 0), legend.justification = c(0, 0))
  } else if ( legend.pos == "top.right") {
    plot <- plot + ggplot2::theme(legend.position= c(1, 1), legend.justification = c(1, 1))
  } else if (legend.pos == "top.left") {
    plot <- plot + ggplot2::theme(legend.position= c(0, 1), legend.justification = c(0, 1))
  } else if (legend.pos == "top.out") {
    plot <- plot + ggplot2::theme(legend.position="top")
  } else if (legend.pos == "bottom.out") {
    plot <- plot + ggplot2::theme(legend.position="bottom")
  } else if (legend.pos == "none") {
    plot <- plot + ggplot2::theme(legend.position="none")
  }
  
  # putting k and g in
  # c("top.right", "top.left", "bottom.right", "bottom.left","none")
  if(k == TRUE && g == FALSE && k.pos == "top.right"){
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = 2,
                         vjust   = 2.5
      )
    
  } else if(k == TRUE && g == FALSE && k.pos == "top.left") {
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = 2.5
      )
  } else if(k == TRUE && g == FALSE && k.pos == "bottom.right") {
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = -Inf),
                         label =  paste("italic(k)==", rev(dat_text$K)),
                         parse = TRUE,
                         hjust   = 2,
                         vjust   = -1.5
      )
  } else if (k == TRUE && g == FALSE && k.pos == "bottom.left"){
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = -1.5
      )
    # below get g ----
    
  } else if (k == TRUE && g == TRUE && k.pos == "top.right"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = 1.5,
                         vjust   = 2)
    
  } else if (k == TRUE && g == TRUE && k.pos == "top.left"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = 2)
  } else if (k == TRUE && g == TRUE && k.pos == "bottom.right"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = 1.5,
                         vjust   = -0.5)
  } else if (k == TRUE && g == TRUE && k.pos == "bottom.left"){
    # get group numbers for moderator
    plot <- plot +
      ggplot2::geom_text(data = dat_text,
                         mapping = ggplot2::aes(x = -Inf, y = -Inf),
                         label =  paste("italic(k)==", dat_text$K,
                                        "~","(", dat_text$G, ")"),
                         parse = TRUE,
                         hjust   = -0.5,
                         vjust   = -0.5)
  }
  
  # # putting colors in
  # if(cb == TRUE){
  #   plot <- plot +
  #     ggplot2::scale_fill_manual(values=cbpl) +
  #     ggplot2::scale_colour_manual(values=cbpl)
  # }
  
  return(plot)
}



mult_orch_plot <- function(bac, tn, tin, tp, po4, tss) {
  sec_y_breaks = c(-1000,-100,0,40,80,99)
  sec_y_labels = c("-1000","-100","0","40","80",">99")

  p1 <- orchaRd::orchard_plot(bac, group = "StudyID", xlab = "",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("FIB")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Overall effect (Efficiency, % reduction)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#66C2A5") +
    scale_fill_manual(values = "#66C2A5") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          axis.text.x.bottom = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  p2 <- orchaRd::orchard_plot(tn, group = "StudyID", xlab = "",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("TN")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#FC8D62") +
    scale_fill_manual(values = "#FC8D62") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.bottom = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  
  p3 <- orchaRd::orchard_plot(tin, group = "StudyID", xlab = "",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("DIN")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#8DA0CB") +
    scale_fill_manual(values = "#8DA0CB") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.y = element_text(hjust = 1),
          axis.title.x.top = element_blank(),
          axis.text.x.bottom = element_blank(),
          axis.text.x.top = element_blank(),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  
  p4 <- orchaRd::orchard_plot(tp, group = "StudyID", xlab = "",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("TP")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#E78AC3") +
    scale_fill_manual(values = "#E78AC3") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.x.top = element_blank(),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          axis.text.x.bottom = element_blank(),
          axis.text.x.top = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  
  p5 <- orchaRd::orchard_plot(po4, group = "StudyID", xlab = "",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("PO4")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#A6D854") +
    scale_fill_manual(values = "#A6D854") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.x.top = element_blank(),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          axis.text.x.bottom = element_blank(),
          axis.text.x.top = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  
  p6 <- orchaRd::orchard_plot(tss, group = "StudyID", xlab = "Overall effect (ROM)",
                              k.pos = -2.5) +
    scale_x_discrete(labels = c("TSS")) +
    scale_y_continuous(limits = c(-3,7.5),
                       sec.axis = sec_axis(~(1-(1/exp(.))) * 100,
                                           name = "Efficiency (%)",
                                           breaks = sec_y_breaks,
                                           labels = sec_y_labels)) +
    scale_color_manual(values = "#FFD92F") +
    scale_fill_manual(values = "#FFD92F") +
    theme_mps_noto(base_family = "Manrope Regular") +
    theme(axis.title = element_text(family = "Manrope SemiBold", face = "plain", size = rel(1)),
          axis.title.x = element_text(hjust = 0),
          axis.title.x.top = element_blank(),
          axis.title.y = element_text(hjust = 1),
          axis.text = element_text(family = "Manrope Light", face = "plain", size = rel(0.7)),
          axis.text.x.top = element_blank(),
          legend.position = "none",
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "grey90", 
                                          color = "grey90"),
          strip.text = element_text(family = "Manrope SemiBold",
                                    color = "black", 
                                    face = "plain",
                                    size = rel(1), hjust = 0))
  # bacteria notes
  bac_preds <- TeX(sum_preds(bac), output = "character")
  bac_eff <- TeX(sum_eff(bac), output = "character")
  p7 <- ggplot() +
    geom_text(aes(1,1, label = bac_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = bac_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  # TN
  nit_preds <- TeX(sum_preds(tn), output = "character")
  nit_eff <- TeX(sum_eff(tn), output = "character")
  p8 <- ggplot() +
    geom_text(aes(1,1, label = nit_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = nit_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  
  # TIN
  tin_preds <- TeX(sum_preds(tin), output = "character")
  tin_eff <- TeX(sum_eff(tin), output = "character")
  p9 <- ggplot() +
    geom_text(aes(1,1, label = tin_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = tin_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  
  # phos
  phos_preds <- TeX(sum_preds(tp), output = "character")
  phos_eff <- TeX(sum_eff(tp), output = "character")
  p10 <- ggplot() +
    geom_text(aes(1,1, label = phos_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = phos_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  
  # po4
  po4_preds <- TeX(sum_preds(po4), output = "character")
  po4_eff <- TeX(sum_eff(po4), output = "character")
  p11 <- ggplot() +
    geom_text(aes(1,1, label = po4_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = po4_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  
  ## tss
  
  tss_preds <- TeX(sum_preds(tss), output = "character")
  tss_eff <- TeX(sum_eff(tss), output = "character")
  p12 <- ggplot() +
    geom_text(aes(1,1, label = tss_preds),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    geom_text(aes(1,0.75, label = tss_eff),
              parse = TRUE,
              family = "Manrope Regular",
              size = 2.5,
              hjust = 0) +
    coord_cartesian(xlim = c(1,2),
                    ylim = c(0.5,1.5)) +
    theme_void()
  
  layout <- "
  AAAAAAGGG
  BBBBBBHHH
  CCCCCCIII
  DDDDDDJJJ
  EEEEEEKKK
  FFFFFFLLL"
  
  p1 + p2 + p3 + p4 + p5 + p6 + p7 + p8 + p9 + p10 + p11 + p12 +  patchwork::plot_layout(design = layout) &
    theme(plot.margin = margin(t = 0.1, b = 0.1, unit = "pt") )
}


sum_preds  <- function(model) {
  preds <- predict(model)
  y_hat <- preds$pred
  y_hat_lb <- preds$ci.lb
  y_hat_ub <- preds$ci.ub

  paste0("$\\widehat{ROM}=$",
         prettyNum(y_hat, digits = 2, nsmall = 2),
         ", 95% CI: [",
         prettyNum(y_hat_lb, digits = 2, nsmall = 2),
         ", ",
         prettyNum(y_hat_ub, digits = 2, nsmall = 2),
         "]")
}

sum_eff <- function(model) {
  preds <- predict(model)
  y_hat <- preds$pred
  y_hat_lb <- preds$ci.lb
  y_hat_ub <- preds$ci.ub
  eff_hat <- (1-(1/exp(y_hat)))*100
  eff_lb <- (1-(1/exp(y_hat_lb)))*100
  eff_ub <- (1-(1/exp(y_hat_ub)))*100
  
  
  paste0("$\\widehat{BMP_{eff}}=$",
         prettyNum(eff_hat, digits = 2, nsmall = 1),
         "%, 95% CI: [",
         prettyNum(eff_lb, digits = 2, nsmall = 1),
         "%, ",
         prettyNum(eff_ub, digits = 2, nsmall = 1),
         "%]")
  
}