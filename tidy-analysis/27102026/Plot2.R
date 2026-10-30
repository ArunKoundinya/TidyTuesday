# =============================================================================
# #TidyTuesday 2026-10-27 · How affordable is IKEA for IKEA's own customers?
#
# ASSUMPTIONS (each one is a parameter below, so you can change it)
#
# A1  BASKET     A living room: one 3-seat sofa + one armchair, bought from
#                each country's own IKEA range, priced in LOCAL currency (so no
#                exchange rate is needed for affordability).
#                "typical": the median-priced sofa and armchair in that country
#                "entry"  : the cheapest sofa and armchair in that country
#                Item types are taken in English via item_no, because 12
#                countries list product_type in their own language.
#
# A2  INCOME     World Inequality Database (WID): average pre-tax national
#                income of each 1% slice of adults (aptinc, age 992, equal-split
#                adults "j"), in local currency, latest year available per
#                country. Income is per ADULT, not per household: we assume
#                one adult's income pays for the basket.
#
# A3  TARGET     IKEA's customer = an adult with income at or above an absolute
#     MARKET     "IKEA-ready" floor, set in PPP dollars so it means the same
#                standard of living everywhere. It does NOT depend on IKEA's
#                prices, so the target market and its affordability are not
#                circular. Default floor: 20,000 PPP$ a year (~55 PPP$ a day).
#                Converted to local currency with WID's PPP rate (xlcusp).
#
# A4  AFFORD-    Days of income = basket cost (local) / (average annual
#     ABILITY    income of the target group / 365). Shown next to the same
#                measure for the MEDIAN adult, for contrast.
#
# A5  TIMING     WID income is a year or more older than IKEA's 2026 prices.
#                No inflation adjustment is made. A check compares WID's
#                exchange rate with the rate implied by IKEA's own USD prices
#                and flags countries where they differ a lot (high inflation
#                or a currency change), so you know where to be careful.
#
# NOT INCLUDED: taxes (income is pre-tax), credit/instalments, household size,
#               urban vs rural, share of spending on furniture.
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(wid)        # install.packages("wid")

ikea_seating <- readr::read_csv(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-27/ikea_seating.csv"
)

# ---- Parameters (the assumptions above) -------------------------------------
basket_level         <- "typical"
basket_types         <- c("3-seat sofa", "Armchair")
ikea_ready_floor_ppp <- 20000
floor_sensitivity    <- c(10000, 20000, 40000)
wid_years            <- 2015:2026
fx_flag_tolerance    <- 0.25

# =============================================================================
# 1. Basket cost per country (local currency)
# =============================================================================
english_type <- ikea_seating |>
  filter(language == "en") |>
  count(item_no, product_type) |>
  group_by(item_no) |>
  slice_max(n, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(item_no, type_en = product_type)

pick_price <- median

basket_items <- ikea_seating |>
  inner_join(english_type, by = "item_no") |>
  filter(type_en %in% basket_types) |>
  group_by(country, currency, type_en, item_no, product_name) |>
  summarise(price = median(price), price_usd = median(price_usd), .groups = "drop") |>
  group_by(country, currency, type_en) |>
  summarise(n_choices = n(),
            price     = pick_price(price),
            price_usd = pick_price(price_usd),
            .groups = "drop")

basket_items |> count(type_en, name = "countries")

basket <- basket_items |>
  group_by(country, currency) |>
  summarise(basket_lcu = sum(price),
            basket_usd = sum(price_usd),
            n_basket   = n(),
            .groups = "drop") |>
  filter(n_basket == length(basket_types)) |>
  mutate(iso2 = toupper(country),
         ikea_fx = basket_lcu / basket_usd) 

basket_label <- paste0( "a median-priced",
                       " 3-seat sofa + armchair")

# =============================================================================
# 2. Income distribution from WID (100 slices of adults)
# =============================================================================
slices <- paste0("p", 0:99, "p", 1:100)

wid_income <- download_wid(
  indicators = "aptinc", areas = basket$iso2, years = wid_years,
  perc = slices, ages = "992", pop = "j"
)

wid_fx <- download_wid(
  indicators = c("xlcusp", "xlcusx"), areas = basket$iso2, years = wid_years,
  perc = "p0p100", ages = "999", pop = "i"
)

# latest year with a complete distribution, per country
income <- wid_income |>
  distinct(country, year, percentile, .keep_all = TRUE) |>    
  group_by(country, year) |>
  filter(n() >= 95, any(percentile == "p50p51")) |>
  group_by(country) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(slice = as.integer(sub("p(\\d+)p.*", "\\1", percentile))) |>
  select(iso2 = country, income_year = year, slice, avg_income = value)

fx <- wid_fx |>
  mutate(rate = substr(variable, 1, 6)) |>
  select(iso2 = country, year, rate, value) |>
  pivot_wider(names_from = rate, values_from = value)

# =============================================================================
# 3. Target market and affordability, for any income floor
# =============================================================================
affordability_for <- function(floor_ppp) {
  income |>
    left_join(fx, by = c("iso2", "income_year" = "year")) |>
    left_join(select(basket, iso2, basket_lcu), by = "iso2") |>
    group_by(iso2, income_year) |>
    summarise(
      floor_lcu        = first(floor_ppp * xlcusp),
      median_income    = avg_income[slice == 50],
      target_share     = mean(avg_income >= floor_lcu),
      target_income    = mean(avg_income[avg_income >= floor_lcu]),
      basket_lcu       = first(basket_lcu),
      wid_fx           = first(xlcusx),
      .groups = "drop"
    ) |>
    mutate(
      floor_ppp          = floor_ppp,
      days_median_adult  = basket_lcu / (median_income / 365),
      days_target_adult  = basket_lcu / (target_income / 365)
    )
}

results <- affordability_for(ikea_ready_floor_ppp) |>
  left_join(select(basket, iso2, basket_usd, ikea_fx), by = "iso2") |>
  mutate(fx_gap  = ikea_fx / wid_fx - 1,
         fx_flag = abs(fx_gap) > fx_flag_tolerance) |>
  arrange(days_target_adult)

# Countries WID could not cover
setdiff(basket$iso2, results$iso2)

# Countries where income year/currency may not match IKEA's 2026 prices (A5)
results |> filter(fx_flag) |> select(iso2, income_year, ikea_fx, wid_fx, fx_gap)

# Sensitivity: does the story survive a lower or higher floor?
sensitivity <- bind_rows(lapply(floor_sensitivity, affordability_for)) |>
  select(iso2, floor_ppp, target_share, days_target_adult) |>
  pivot_wider(names_from = floor_ppp,
              values_from = c(target_share, days_target_adult))
sensitivity

# =============================================================================
# 4. Chart: median adult vs IKEA's target customer
# =============================================================================
st <- list(bg = "#f6f2ea", ink = "#1b2a3a", ink_muted = "#5d6878",
           title = "#0058a3", median = "#a9a59d", target = "#0058a3",
           flag = "#e8a200")
font <- "sans"

plot_data <- results |>
  mutate(label = paste0(iso2, if_else(fx_flag, " *", "")),
         label = reorder(label, -days_target_adult))

p_afford <- ggplot(plot_data, aes(y = label)) +
  geom_segment(aes(x = days_target_adult, xend = days_median_adult, yend = label),
               colour = alpha(st$ink_muted, 0.4), linewidth = 0.8) +
  geom_point(aes(x = days_median_adult), colour = st$median, size = 2.4) +
  geom_point(aes(x = days_target_adult), colour = st$target, size = 2.6) +
  geom_text(aes(x = days_median_adult, label = percent(target_share, accuracy = 1)),
            hjust = -0.4, size = 2.5, colour = st$ink_muted, family = font) +
  scale_x_log10(labels = label_number(suffix = " d")) +
  labs(
    title = "What an IKEA living room costs, in days of income",
    subtitle = paste0(
      "Days of pre-tax income needed to buy ", basket_label,
      " (local prices, each country's own range).\n",
      "Blue dot = IKEA's target customer (adults earning at least ",
      dollar(ikea_ready_floor_ppp), " PPP a year)  |  Grey dot = median adult  |  ",
      "% = share of adults in the target market"
    ),
    x = "Days of pre-tax income needed (log scale)", y = NULL
  ) +
  theme_minimal(base_family = font, base_size = 10) +
  theme(
    plot.background    = element_rect(fill = st$bg, colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(colour = alpha(st$ink_muted, 0.2)),
    text               = element_text(colour = st$ink),
    axis.text          = element_text(colour = st$ink_muted),
    plot.title         = element_text(face = "bold", size = 16, colour = st$title,hjust = 0.5),
    plot.subtitle      = element_text(colour = st$ink_muted, size = 9, lineheight = 1.2,hjust = 0.5,),
    plot.caption       = element_text(colour = st$ink_muted, size = 7, hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin        = margin(16, 20, 12, 16)
  )

p_afford

# =============================================================================
# 5. Map: days of income for IKEA's target customer
# =============================================================================
library(sf)
library(rnaturalearth)
library(ggrepel)

st_map <- list(bg = "#f6f2ea", ink = "#1b2a3a", ink_muted = "#5d6878",
               title = "#0058a3", no_data = "#ebe5da", border = "#ffffff",
               low = "#f3e2b0", mid = "#e8a200", high = "#7a4e00", circle = "#1b2a3a")

world_aff <- ne_countries(scale = 50, returnclass = "sf") |>
  filter(admin != "Antarctica") |>
  select(name, iso2 = iso_a2_eh) |>
  left_join(results, by = "iso2") |>
  st_transform("+proj=eqearth")

aff_points <- world_aff |>
  filter(!is.na(days_target_adult)) |>
  st_cast("MULTIPOLYGON") |>
  st_cast("POLYGON") |>
  mutate(area = st_area(geometry)) |>
  group_by(iso2) |>
  slice_max(area, n = 1, with_ties = FALSE) |>
  ungroup() |>
  st_point_on_surface() |>
  mutate(x = st_coordinates(geometry)[, 1],
         y = st_coordinates(geometry)[, 2]) |>
  st_drop_geometry()

aff_labels <- bind_rows(slice_min(aff_points, days_target_adult, n = 5),
                        slice_max(aff_points, days_target_adult, n = 5)) |>
  mutate(label = paste0(name, if_else(fx_flag, " *", ""), "  ",
                        number(days_target_adult, accuracy = 0.1), " days",
                        "\n", percent(target_share, accuracy = 1), " of adults in market"))

day_range <- range(results$days_target_adult, na.rm = TRUE)

p_aff_map <- ggplot() +
  geom_sf(data = world_aff, aes(fill = days_target_adult),
          colour = st_map$border, linewidth = 0.2) +
  geom_point(data = aff_points, aes(x, y, size = target_share),
             shape = 21, fill = NA, colour = alpha(st_map$circle, 0.7), stroke = 0.45) +
  geom_label_repel(
    data = aff_labels, aes(x, y, label = label),
    family = font, size = 2.8, lineheight = 0.95,
    colour = st_map$ink, fill = alpha(st_map$bg, 0.9), label.size = 0,
    label.padding = unit(0.18, "lines"),
    segment.colour = st_map$ink_muted, segment.size = 0.3,
    box.padding = 0.6, min.segment.length = 0, max.overlaps = Inf, seed = 7
  ) +
  scale_fill_gradientn(
    colours = c(st_map$low, st_map$mid, st_map$high),
    trans = "log10", na.value = st_map$no_data,
    labels = label_number(accuracy = 1, suffix = " days"),
    name = "Days of income for the target customer (darker = harder to afford)",
    guide = guide_colourbar(barwidth = unit(9, "cm"), barheight = unit(0.28, "cm"),
                            title.position = "top", order = 1)
  ) +
  scale_size_continuous(
    range = c(0.8, 3.6), labels = percent, name = "Share of adults in target market",
    guide = guide_legend(title.position = "top", order = 2,
                         override.aes = list(colour = st_map$circle))
  ) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(
    title = "How affordable is IKEA for IKEA's own customers?",
    subtitle = paste0(
      "Days of pre-tax income a target customer needs to buy ", basket_label,
      " from each country's own range, at local prices.\n",
      "Target customer: an adult earning at least ", dollar(ikea_ready_floor_ppp),
      " PPP a year. Circles: how big that target market is."
    )
  ) +
  theme_void(base_family = font) +
  theme(
    plot.background   = element_rect(fill = st_map$bg, colour = NA),
    text              = element_text(colour = st_map$ink),
    plot.title        = element_text(face = "bold", size = 22, colour = st_map$title,hjust = 0.5,
                                     margin = margin(b = 6)),
    plot.subtitle     = element_text(colour = st_map$ink_muted, size = 10, lineheight = 1.2,hjust = 0.5,
                                     margin = margin(b = 8)),
    plot.caption      = element_text(colour = st_map$ink_muted, size = 7.5, hjust = 0,
                                     lineheight = 1.2, margin = margin(t = 10)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position   = "bottom",
    legend.box        = "horizontal",
    legend.title      = element_text(colour = st_map$ink_muted, size = 8.5),
    legend.text       = element_text(colour = st_map$ink_muted, size = 8),
    legend.spacing.x  = unit(1, "cm"),
    plot.margin       = margin(18, 18, 12, 18)
  )

aff_hist <- results |>
  filter(!is.na(days_target_adult)) |>
  mutate(band = cut(days_target_adult,
                    breaks = c(0, 2, 4, 6, 8, 10, 15, 20, 30, Inf),
                    labels = c("<2", "2-4", "4-6", "6-8", "8-10", "10-15", "15-20", "20-30", "30+"),
                    right = FALSE)) |>
  count(band, .drop = FALSE) |>
  mutate(mid = c(1, 3, 5, 7, 9, 12.5, 17.5, 25, 40)[as.integer(band)])

p_aff_hist <- ggplot(aff_hist, aes(band, n, fill = mid)) +
  geom_col(width = 0.85) +
  geom_text(aes(label = if_else(n > 0, as.character(n), "")), vjust = -0.4,
            family = font, size = 2.3, colour = st_map$ink) +
  scale_fill_gradientn(colours = c(st_map$low, st_map$mid, st_map$high),
                       trans = "log10", limits = day_range, oob = squish, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "How many countries at each level",
       subtitle = "Days of income for the target customer") +
  theme_void(base_family = font) +
  theme(
    plot.title      = element_text(colour = st_map$ink, size = 7.5, face = "bold",hjust = 0.5,
                                   margin = margin(b = 1)),
    plot.subtitle   = element_text(colour = st_map$ink_muted, size = 6, hjust = 0.5,  margin = margin(b = 2)),
    axis.text.x     = element_text(colour = st_map$ink_muted, size = 5.5, margin = margin(t = 2)),
    axis.line.x     = element_line(colour = alpha(st_map$ink_muted, 0.5), linewidth = 0.25),
    plot.background = element_rect(fill = NA, colour = NA),
    plot.margin     = margin(2, 4, 2, 4)
  )

inset_box_aff <- st_sfc(st_point(c(34, -29)), st_point(c(110, -29)), crs = 4326) |>
  st_transform(st_crs(world_aff)) |>
  st_coordinates()
bb_aff <- st_bbox(world_aff)

p_aff_final <- p_aff_map +
  annotation_custom(ggplotGrob(p_aff_hist),
                    xmin = inset_box_aff[1, "X"], xmax = inset_box_aff[2, "X"],
                    ymin = bb_aff[["ymin"]],      ymax = inset_box_aff[1, "Y"])

p_aff_final