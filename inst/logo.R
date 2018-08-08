# library(hexSticker)
# library(magick)
# 
# img_url <- "http://res.cloudinary.com/syknapptic/image/upload/v1516468904/logo_rd5ifq.png"
# 
# img_raw <- img_url %>% 
#   image_read() %>% 
#   image_crop("8000x8000+2000+0") %>% 
#   image_resize("5%x5%")
# 
# # 
# # logo <- image_read("man/figures/logo-plot.png") %>% 
# #   image_background(color = "transparent", flatten = FALSE)
# # hex sticker ====
# sticker(subplot = img_raw,
#   # s_x = 1,
#   # s_y = 1,
#   # s_width = 1,
#   # s_height = 1,
#   package = "snatools",
#   p_y = 1.625,
#   p_size = 30,
#   p_color = "#00471a",
#   h_color = "#056d21",
#   h_fill = "white",
#   u_size = 8,
#   u_color = "black",
#   # spotlight = TRUE,  #l_width = 5,
#   # l_height = 1,
#   # l_alpha = 0.9,l_y = 0.25,
#   url = "knapply.github.io/snatools",
#   filename = "man/figures/logo.png")
# #   
# # ggsave(width = 43.9 * 1.5, height = 50.8 * 1.5, dpi = 600,
# #        filename = "man/figures/logo.png", 
# #        bg = "transparent", units = "mm")
# # 
# # 
# # 
