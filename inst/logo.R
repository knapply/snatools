library(hexSticker)
library(magick)

img_url <- "http://res.cloudinary.com/syknapptic/image/upload/v1533688420/spatial-net_zhmhez.png"

logo_path <- "man/figures/logo.png"
# site_logo_path <- "docs/logo.png"
# favicon_path <- "docs/favicon.ico"

if(!dir.exists("man/figures")) {
  dir.create("man/figures")
} 
# if(!dir.exists("docs")) {
  # dir.create("docs")
# } 

# logo ====
logo <- image_read(img_url)

res_x <- image_info(logo)[["width"]] / 1000
res_y <- image_info(logo)[["height"]] / 1000

sticker(subplot = logo,
  s_x = 1,
  s_y = 1,
  s_width = 0.00001,
  s_height = 0.00001,
  package = "snatools",
  p_y = 1.6, p_size = 18,
  p_color = "black",
  # h_color = "#3b3b3b",
  h_fill = "white",
  u_size = 5,
  u_color = "black",
  url = "knapply.github.io/snatools",
  filename = logo_path)
