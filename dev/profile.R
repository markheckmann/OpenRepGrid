devtools::load_all()
library(ggplot2)

elementProfile <- function(x, elements) {
  color_border <- "black"
  text_col <- "black"

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is needed", call. = FALSE)
  }
  xr <- x[, elements]
  df_ratings <- ratings(xr, trim = NA) %>%
    as.data.frame() %>%
    `rownames<-`(NULL)
  df_ratings$c_idx <- seq_len(nrow(x)) %>% rev()
  df_ratings <- dplyr::bind_cols(constructs(xr), df_ratings)
  df_long <- tidyr::pivot_longer(df_ratings,
    cols = -c(c_idx, leftpole, rightpole),
    names_to = "element", values_to = "rating"
  )

  mp <- midpoint(x)
  sr <- getScale(x)
  r_min <- min(sr)
  r_max <- max(sr)
  r_all <- seq(r_min, r_max)

  th <-
    theme_bw() +
    theme(
      axis.text.y.left = element_text(size = 13.5, color = text_col, hjust = 1, margin = margin(r = 5)),
      axis.text.y.right = element_text(size = 13.5, color = text_col, hjust = .0, margin = margin(l = 5)),
      axis.text.x = element_text(size = 14, color = text_col, face = "bold"), # , family = "Space Grotesk Bold"),
      axis.line = element_line(colour = color_border),
      axis.ticks = element_line(colour = color_border)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      legend.text = element_text(size = 13.5),
      legend.title = element_blank()
    )

  ggplot(df_long, aes(x = c_idx, y = rating, group = element, color = element)) +
    geom_hline(yintercept = mp, linewidth = .5, color = grey(.05)) +
    geom_hline(yintercept = r_all, linewidth = .5, color = grey(.91)) +
    scale_y_continuous("",
      labels = r_all, breaks = r_all,
      limits = sr, expand = c(0, 0)
    ) +
    coord_flip(clip = "off") +
    geom_hline(yintercept = sr, color = "grey") +
    th +
    geom_point(aes(y = rating, color = element), shape = 19, size = 3) +
    geom_line(aes(y = rating, color = element, linetype = element), linewidth = 1) +
    scale_x_continuous(
      breaks = df_ratings$c_idx, labels = df_ratings$leftpole,
      sec.axis = sec_axis(~., breaks = df_ratings$c_idx, labels = df_ratings$rightpole)
    ) +
    xlab("")
}


b <- alignByIdeal(boeker, "ideal self")
b <- align(boeker)
elementProfile(b, c("ideal self")) + scale_color_manual(values = c("ideal self" = "blue"))
elementProfile(b, c("ideal self", "self")) +
  scale_color_manual(values = c("ideal self" = "green", "self" = "grey"))



elementDifferencePlot <- function(x, elements) {
  color_border <- "black"
  text_col <- "black"

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("package ggplot2 is needed", call. = FALSE)
  }
  xr <- x[, elements]
  df_ratings <- ratings(xr, trim = NA) %>%
    as.data.frame() %>%
    `rownames<-`(NULL)
  df_ratings$c_idx <- seq_len(nrow(x)) %>% rev()
  df_ratings <- dplyr::bind_cols(constructs(xr), df_ratings)
  df_long <- tidyr::pivot_longer(df_ratings,
                                 cols = -c(c_idx, leftpole, rightpole),
                                 names_to = "element", values_to = "rating"
  )
  names(df_ratings)[3:4] <- c("e1", "e2")

  mp <- midpoint(x)
  sr <- getScale(x)
  r_min <- min(sr)
  r_max <- max(sr)
  r_all <- seq(r_min, r_max)

  th <-
    theme_bw() +
    theme(
      axis.text.y.left = element_text(size = 13.5, color = text_col, hjust = 1, margin = margin(r = 5)),
      axis.text.y.right = element_text(size = 13.5, color = text_col, hjust = .0, margin = margin(l = 5)),
      axis.text.x = element_text(size = 14, color = text_col, face = "bold"), # , family = "Space Grotesk Bold"),
      axis.line = element_line(colour = color_border),
      axis.ticks = element_line(colour = color_border)
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      legend.text = element_text(size = 13.5),
      legend.title = element_blank()
    )

  ggplot(df_long, aes(x = c_idx, y = rating, group = element, color = element)) +
    geom_hline(yintercept = mp, linewidth = .5, color = grey(.05)) +
    geom_hline(yintercept = r_all, linewidth = .5, color = grey(.91)) +
    scale_y_continuous("",
                       labels = r_all, breaks = r_all,
                       limits = sr, expand = c(0, 0)
    ) +
    coord_flip(clip = "off") +
    geom_hline(yintercept = sr, color = "grey") +
    th +
    geom_segment(aes(y = e1, yend = e2, x = c_idx, xend = c_idx, group = NULL), colour = "grey",
                 linewidth = 2,
                 data = df_ratings) +
    geom_point(aes(y = rating, color = element), shape = 19, size = 3) +
    scale_x_continuous(
      breaks = df_ratings$c_idx, labels = df_ratings$leftpole,
      sec.axis = sec_axis(~., breaks = df_ratings$c_idx, labels = df_ratings$rightpole)
    ) +
    xlab("")
}


b <- alignByIdeal(boeker, "ideal self")
b <- align(boeker)
elementDifferencePlot(b, c("ideal self", "self")) +
  scale_color_manual(values = c("ideal self" = "green", "self" = "black"))
