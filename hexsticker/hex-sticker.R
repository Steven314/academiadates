# This is based on:
# https://github.com/robjhyndman/calcal/blob/main/hexsticker/hex-sticker.R

library(cropcircles)
library(ggpath)
library(showtext)
library(rsvg)
library(magick)

# choose a font from Google Fonts
font_add_google("Fira Sans", "firasans")
showtext_auto()

path <- "hexsticker/calendar2-week.svg"

download.file(
    "https://icons.getbootstrap.com/assets/icons/calendar2-week.svg",
    path
)

bitmap <- rsvg(path, height = 480, css = charToRaw("svg {fill: #2294B7}"))
png::writePNG(bitmap, "hexsticker/calendar.png", dpi = 144)

image_read(bitmap) |>
    image_extent('1200x900') |>
    image_extent('1200x1100', gravity = "North") |>
    image_write("hexsticker/temp_calendar.png")

image_path <- cropcircles::crop_hex(
    image = "hexsticker/temp_calendar.png",
    bg_fill = "#EFCB68",
    border_colour = "#160c28",
    border_size = 60
)

ggplot() +
    geom_from_path(aes(0.5, 0.5, path = image_path)) +
    annotate(
        "text",
        x = 0.5,
        y = -0.2,
        label = "academiadates",
        family = "firasans",
        size = 18,
        colour = "#160c28",
        hjust = 0.5,
        fontface = "bold"
    ) +
    xlim(-1, 2) +
    ylim(-1, 2) +
    theme_void() +
    coord_fixed()

file.remove("hexsticker/temp_calendar.png")

ggsave("./man/figures/hex.png", height = 2.5, width = 2.5)
