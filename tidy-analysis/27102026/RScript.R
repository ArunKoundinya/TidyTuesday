library(dplyr)
library(ggplot2)
library(sf)                 
library(rnaturalearth)      
library(ggrepel)            
library(scales)

ikea_seating <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_seating.csv"
)

min_countries <- 10

ikea_index <- ikea_seating |>
  group_by(item_no) |>
  filter(n_distinct(country) >= min_countries) |>
  mutate(rel_price = price_usd / median(price_usd)) |>
  group_by(country) |>
  summarise(index   = median(rel_price),
            n_items = n_distinct(item_no),
            .groups = "drop") |>
  mutate(iso_a2 = toupper(country)) |>
  arrange(index)


world <- ne_countries(scale = 50, returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  select(name, iso_a2 = iso_a2_eh) |>
  left_join(ikea_index, by = "iso_a2") |>
  st_transform("+proj=eqearth")                     # Equal Earth projection


ikea_points <- world |>
  filter(!is.na(index)) |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  group_by(iso_a2) |>
  slice_max(area, n = 1, with_ties = FALSE) |>
  ungroup() |>
  st_point_on_surface() |>
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

# Label the 5 cheapest and 5 most expensive countries
extremes <- bind_rows(slice_min(ikea_points, index, n = 8),
                      slice_max(ikea_points, index, n = 8)) |>
  mutate(label = paste0(name, "  ", number(index, accuracy = 0.01),
                        "\n", n_items, " items"))



st <- list(
  bg        = "#f6f2ea",
  ink       = "#1b2a3a",
  ink_muted = "#5d6878",
  title     = "#0058a3",
  no_data   = "#ebe5da",
  border    = "#ffffff",
  neutral   = "#a9a59d",
  cheap     = "#0058a3",
  pricey    = "#e8a200",
  circle    = "#1b2a3a"
)

font <- "sans"

#plot
p_map <- ggplot() +
  geom_sf(data = world, aes(fill = index), colour = st$border, linewidth = 0.2) +
  # circle size = number of items compared in that country
  geom_point(data = ikea_points, aes(x, y, size = n_items),
             shape = 21, fill = NA, colour = alpha(st$circle, 0.7), stroke = 0.45) +
  geom_label_repel(
    data = extremes, aes(x, y, label = label),
    family = font, size = 2.9, lineheight = 0.95,
    colour = st$ink, fill = alpha(st$bg, 0.9), label.size = 0,
    label.padding = unit(0.18, "lines"),
    segment.colour = st$ink_muted, segment.size = 0.3,
    box.padding = 0.6, min.segment.length = 0, max.overlaps = Inf, seed = 7
  ) +
  scale_fill_gradient2(
    low = st$cheap, mid = st$neutral, high = st$pricey, midpoint = 1,
    limits = c(0.7, 1.3), oob = squish, na.value = st$no_data,
    breaks = c(0.7, 0.85, 1, 1.15, 1.3),
    labels = c("30% cheaper", "15%", "global\nmiddle", "15%", "30%+ pricier"),
    name = NULL,
    guide = guide_colourbar(barwidth = unit(9, "cm"), barheight = unit(0.28, "cm"),
                            title.position = "top", order = 1)
  ) +
  scale_size_continuous(
    range = c(0.8, 3.6), name = "Items compared",
    guide = guide_legend(title.position = "top", order = 2,
                         override.aes = list(colour = st$circle))
  ) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(
    title = "The IKEA Price Comparision",
    subtitle = paste0(
      "The same items in USD across 50 countries. ",
      "Each country's index is its median price relative to the item's global median.\n",
      "Labelled: the 8 cheapest and 8 priciest markets. Circles: how many items were compared."
    )
  ) +
  theme_void(base_family = font) +
  theme(
    plot.background   = element_rect(fill = st$bg, colour = NA),
    text              = element_text(colour = st$ink),
    plot.title        = element_text(face = "bold", size = 24, colour = st$title, hjust = 0.5,
                                     margin = margin(b = 6)),
    plot.subtitle     = element_text(colour = st$ink_muted, size = 10, lineheight = 1.2, hjust = 0.5,
                                     margin = margin(b = 8)),
    plot.caption      = element_text(colour = st$ink_muted, size = 7.5, hjust = 0,
                                     lineheight = 1.2, margin = margin(t = 10)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.title      = element_text(colour = st$ink_muted, size = 8.5),
    legend.text       = element_text(colour = st$ink_muted, size = 8),
    legend.spacing.x  = unit(1, "cm"),
    plot.margin       = margin(18, 18, 12, 18)
  )

p_map
bin_width <- 0.05

hist_data <- ikea_points |>
  mutate(bin = floor(index / bin_width) * bin_width + bin_width / 2) |>
  count(bin, name = "n_countries")

p_bar <- ggplot(hist_data, aes(bin, n_countries, fill = bin)) +
  geom_col(width = bin_width * 0.88) +
  geom_vline(xintercept = 1, colour = st$ink_muted, linewidth = 0.3, linetype = "22") +
  geom_text(aes(label = n_countries), vjust = -0.4,
            family = font, size = 2.3, colour = st$ink) +
  annotate("text", x = 1, y = max(hist_data$n_countries) * 1.32, label = "global middle",
           family = font, size = 2, colour = st$ink_muted, hjust = -0.08) +
  scale_fill_gradient2(
    low = st$cheap, mid = st$neutral, high = st$pricey, midpoint = 1,
    limits = c(0.7, 1.3), oob = squish, guide = "none"
  ) +
  scale_x_continuous(breaks = seq(0.7, 1.7, 0.1),
                     labels = \(x) number(x, accuracy = 0.1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.4))) +
  labs(title = "How many countries at each price level",
       subtitle = paste0("IKEA Index in bands of ", bin_width)) +
  theme_void(base_family = font) +
  theme(
    plot.title      = element_text(colour = st$ink, size = 7.5, face = "bold",
                                   margin = margin(b = 1)),
    plot.subtitle   = element_text(colour = st$ink_muted, size = 6,
                                   margin = margin(b = 2)),
    axis.text.x     = element_text(colour = st$ink_muted, size = 5.5,
                                   margin = margin(t = 2)),
    axis.line.x     = element_line(colour = alpha(st$ink_muted, 0.5), linewidth = 0.25),
    plot.background = element_rect(fill = NA, colour = NA),
    plot.margin     = margin(2, 4, 2, 4)
  )

inset_box <- st_sfc(st_point(c(34, -10)), st_point(c(140, 29)), crs = 4326) |>
  st_transform(st_crs(world)) |>
  st_coordinates()
bb <- st_bbox(world)

p_final <- p_map +
  annotation_custom(ggplotGrob(p_bar),
                    xmin = inset_box[1, "X"], xmax = inset_box[2, "X"],
                    ymin = bb[["ymin"]],      ymax = inset_box[1, "Y"])

p_final