# create hex sticker

library(OpenRepGrid)
library(hexSticker)
library(showtext)
library(magick)

path <- "man/figures/logo.png"
img_grid <- "inst/figures/grid.png"
color <- "white"
fill <- "#B02418"
border <- "#702418"
font <- "Cabin Condensed"
family <- "grid"

font_add_google(font, "grid") #
showtext_auto()

# background grid image
png(img_grid, bg = "transparent", units = "cm", width = 10, height = 7, res = 300)
bertin(feixas2004[c(1, 6, 8, 11), 1:4], c("white", "darkred"),
  id = c(FALSE, FALSE), col.e.and.c = "white", col.lines = "white",
  ylim = c(0, .7), xlim = c(.25, .75),
  cex.elements = .9, cex.constructs = .9, cex.text = .95
)
dev.off()

sticker(img_grid,
  package = "OpenRepGrid",
  h_fill = fill, p_color = color, p_fontface = "plain",
  h_color = border, p_y = 1.41, h_size = 2,
  p_size = 20, s_x = 1, s_y = .58, s_width = .88,
  p_family = family,
  filename = path,
  white_around_sticker = TRUE
)

# crop hexagon
# source:  https://stackoverflow.com/questions/60426922/trim-around-hexagon-shape-with-hexsticker
fuzz <- 50
p <- image_read(path)
pp <- p %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width - 1, "+1")) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+1", "+", image_info(p)$height - 1)) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width - 1, "+", image_info(p)$height - 1))
image_write(image = pp, path = path)

mh::file_open(path)
