ai_salary_majors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-20/ai_salary_majors.csv')
major_occupations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-10-20/major_occupations.csv')

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggforce)
library(gt)


### Personal choice for themes
bg        <- "black"
panel     <- "midnightblue"
grid_col  <- "#473C8B"   # slateblue4; hex so it also works in the HTML table
ink       <- "white"
ink_muted <- "lightsteelblue"
font      <- "sans"

tier_levels <- c("Low", "Mid", "High")

tier_cols <- c(
  Low  = "#00E5EE",  # turquoise2
  Mid  = "gold",
  High = "hotpink"
)


jobs <- major_occupations |>
  inner_join(
    ai_salary_majors |>
      select(cip4, median_starting_salary, median_salary_4yr, n_graduates),
    by = "cip4"
  ) |>
  group_by(soc, occupation) |>
  summarise(
    ai_exposure          = first(ai_exposure),
    ai_exposure_language = first(ai_exposure_language),
    ai_exposure_image    = first(ai_exposure_image),
    salary_proxy    = weighted.mean(median_salary_4yr, n_graduates, na.rm = TRUE),
    n_feeder_majors = n_distinct(cip4),
    .groups = "drop"
  ) |>
  filter(!is.na(salary_proxy))

# K-means clustering
km_features <- jobs |>
  select(ai_exposure, ai_exposure_language, ai_exposure_image) |>
  scale()

km <- kmeans(km_features, centers = 3, nstart = 25)

cluster_order <- tibble(cluster = 1:3,
                        mean_exp = tapply(jobs$ai_exposure, km$cluster, mean)) |>
  arrange(mean_exp) |>
  mutate(tier = factor(tier_levels, levels = tier_levels))

jobs <- jobs |>
  mutate(cluster = km$cluster,
         dist_to_centre = sqrt(rowSums((km_features - km$centers[km$cluster, ])^2))) |>
  left_join(select(cluster_order, cluster, tier), by = "cluster")




major_jobs_all <- major_occupations |>
  distinct(cip4, soc) |>
  inner_join(jobs, by = "soc") |>
  group_by(cip4) |>
  mutate(n_total   = n(),
         main_tier = tier_levels[which.max(tabulate(as.integer(tier), nbins = 3))]) |>
  ungroup() |>
  inner_join(select(ai_salary_majors, cip4, field_of_study, n_graduates), by = "cip4")

major_fit <- major_jobs_all |>
  filter(tier == main_tier) |>
  group_by(cip4, main_tier, field_of_study, n_graduates) |>
  summarise(n_total = first(n_total),
            n_main  = n(),
            purity  = n_main / n_total,
            spread  = sqrt(sd(ai_exposure)^2 + (sd(salary_proxy) / 10000)^2),
            .groups = "drop") 
  #filter(n_main >= min_jobs, purity >= min_purity)

major_fit_sub <- major_fit |>
  filter(purity > 0.75, !is.na(spread))

picked <- major_fit_sub |>
  group_by(main_tier) |>
  #filter(spread <= median(spread)) |>
  slice_max(n_graduates, n = 3, with_ties = FALSE) |>
  ungroup() |>
  mutate(major = str_wrap(str_trunc(field_of_study, 34), 22))

major_jobs <- major_jobs_all |>
  inner_join(select(picked, cip4, major, n_main), by = "cip4") |>
  filter(tier == main_tier)


y_top <- max(jobs$salary_proxy)

p_scatter <- ggplot(jobs, aes(ai_exposure, salary_proxy)) +
  # "average job" reference line
  geom_vline(xintercept = 0, colour = ink_muted, linetype = "22", linewidth = 0.4) +
  annotate("text", x = 0.04, y = y_top, label = "average job",
           hjust = 0, vjust = 1, colour = ink_muted, size = 3, family = font) +
  # direction hints along the x axis
  annotate("text", x = min(jobs$ai_exposure), y = -Inf, vjust = -0.8, hjust = 0,
           label = "<  less exposed: hands-on work", colour = ink_muted,
           size = 3, family = font) +
  annotate("text", x = max(jobs$ai_exposure), y = -Inf, vjust = -0.8, hjust = 1,
           label = "more exposed: AI overlaps the skill mix  >", colour = ink_muted,
           size = 3, family = font) +
  # all jobs
  geom_point(aes(colour = tier), alpha = 0.7, size = 1.9, stroke = 0) +
  # spotlight majors: their jobs ringed in white, circled with a dashed ellipse
  geom_point(data = major_jobs, aes(fill = tier), shape = 21,
             colour = "white", stroke = 0.6, size = 2.6, show.legend = FALSE) +
  geom_mark_ellipse(
    data = major_jobs,
    aes(
      group = major,
      label = major,
      description = paste0(
        n_main, " of its ", n_total, " jobs  |  ",
        comma(n_graduates), " grads"
      )
    ),
    colour = "lightskyblue",
    linetype = "dashed",
    linewidth = 0.5,
    expand = unit(3, "mm"),
    label.family = font,
    label.fontsize = c(9, 7.5),
    label.colour = c("white", "lightsteelblue"),
    label.fill = alpha("navy", 0.96),
    label.buffer = unit(6, "mm"),
    con.colour = "lightsteelblue",
    con.size = 0.4,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = tier_cols, name = "AI exposure cluster",
                      guide = guide_legend(override.aes = list(size = 3.5, alpha = 1))) +
  scale_fill_manual(values = tier_cols, guide = "none") +
  scale_x_continuous(breaks = -3:2, labels = \(x) ifelse(x > 0, paste0("+", x), x)) +
  scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0.06, 0.04))) +
  labs(
    x = "AI exposure score  (standard deviations from the average job)",
    y = "Salary 4 years after graduation (proxy)",
    title = "Where AI meets the job market",
    subtitle = paste0(
      "453 occupations grouped by k-means on their overall, language and image AI exposure.\n",
      "Circled: in each cluster, the largest major whose graduates land in a tight group of jobs."
    )
  ) +
  theme_minimal(base_size = 11, base_family = font) +
  theme(
    plot.background  = element_rect(fill = bg, colour = NA),
    panel.background = element_rect(fill = panel, colour = NA),
    
    panel.grid.major = element_line(
      colour = grid_col,
      linewidth = 0.3
    ),
    panel.grid.minor = element_blank(),
    
    text = element_text(colour = ink),
    axis.text = element_text(colour = ink_muted),
    axis.title = element_text(colour = ink_muted, size = 9.5),
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 20,
      colour = "white",
      margin = margin(b = 6)
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      colour = ink_muted,
      size = 10.5,
      lineheight = 1.2,
      margin = margin(b = 10)
    ),
    
    legend.position = "top",
    legend.title = element_text(colour = ink_muted, size = 9),
    legend.text = element_text(colour = ink, size = 9.5),
    legend.key = element_rect(fill = NA, colour = NA)
  )

p_scatter

grad_flow <- major_jobs_all |>
  mutate(grads_to_job = n_graduates / n_total) |>
  group_by(tier) |>
  summarise(graduates = sum(grads_to_job), .groups = "drop") |>
  mutate(grad_share = graduates / sum(graduates))

typical_jobs <- jobs |>
  group_by(tier) |>
  slice_min(dist_to_centre, n = 3, with_ties = FALSE) |>
  summarise(typical = paste(occupation, collapse = " | "), .groups = "drop")

cluster_tbl <- jobs |>
  group_by(tier) |>
  summarise(
    n_jobs     = n(),
    exposure   = mean(ai_exposure),
    language   = mean(ai_exposure_language),
    image      = mean(ai_exposure_image),
    salary_med = median(salary_proxy),
    salary_lo  = quantile(salary_proxy, 0.25),
    salary_hi  = quantile(salary_proxy, 0.75),
    .groups = "drop"
  ) |>
  left_join(grad_flow, by = "tier") |>
  left_join(typical_jobs, by = "tier") |>
  left_join(picked |>
              group_by(tier = factor(main_tier, levels = c("High", "Mid", "Low"))) |>
              summarise( spotlight = paste(field_of_study, collapse = " | "), .groups = "drop"), by = "tier") |>
  arrange(desc(tier)) |>
  mutate(
    salary_range = paste0(dollar(salary_lo, scale = 1e-3, suffix = "k", accuracy = 1), " – ",
                          dollar(salary_hi, scale = 1e-3, suffix = "k", accuracy = 1)),
    tier_chr = as.character(tier)
  ) |>
  select(tier_chr, n_jobs, exposure, language, image, salary_med, salary_range,
         grad_share, graduates, typical, spotlight)

dot_label <- function(tier) {
  paste0(
    "<span style='color:", unname(tier_cols[tier]),
    ";font-size:18px;'>&#9679;</span> ",
    "<span>", tier, "</span>"
  )
}

share_bar <- function(share, tier) {
  paste0(
    "<div style='display:flex;align-items:center;gap:8px;'>",
    "<div style='width:110px;height:8px;background:", grid_col,
    ";border-radius:4px;overflow:hidden;'>",
    "<div style='width:", round(100 * share),
    "%;height:8px;background:", unname(tier_cols[tier]),
    ";border-radius:4px;'></div></div>",
    "<span>", percent(share, accuracy = 0.1), "</span></div>"
  )
}

gt_tbl <- cluster_tbl |>
  mutate(cluster_html = dot_label(tier_chr),
         share_html   = mapply(share_bar, grad_share, tier_chr)) |>
  select(cluster_html, n_jobs, exposure, language, image,
         salary_med, salary_range, share_html, graduates, typical, spotlight) |>
  gt() |>
  fmt_markdown(columns = c(cluster_html, share_html)) |>
  tab_header(
    title    = md("**Three ways AI meets work**"),
    subtitle = "Occupation clusters from k-means on AI exposure, with pay and where graduates go"
  ) |>
  cols_label(
    cluster_html = "Cluster",
    n_jobs       = "Jobs",
    exposure     = "Overall",
    language     = "Language",
    image        = "Image",
    salary_med   = "Median",
    salary_range = "Middle 50%",
    share_html   = "Share of graduates",
    graduates    = "Graduates",
    typical      = "Most typical jobs",
    spotlight    = "Spotlight majors"
  ) |>
  tab_spanner(label = "AI exposure (avg score)", columns = c(exposure, language, image)) |>
  tab_spanner(label = "Salary, 4 yrs out (proxy)", columns = c(salary_med, salary_range)) |>
  tab_spanner(label = "Where graduates go", columns = c(share_html, graduates)) |>
  fmt_number(columns = c(exposure, language, image), decimals = 2, force_sign = TRUE) |>
  fmt_currency(columns = salary_med, decimals = 0) |>
  fmt_number(columns = graduates, decimals = 0, suffixing = TRUE) |>
  data_color(columns = c(exposure, language, image),
             palette = c(tier_cols[["Low"]], panel, tier_cols[["High"]]),
             domain = c(-1.6, 1.6)) |>
  cols_align(align = "center", columns = c(n_jobs, exposure, language, image)) |>
  cols_width(typical ~ px(260), spotlight ~ px(260)) |>
  tab_style(
    style = cell_text(size = px(12)),
    locations = cells_body()
  ) |>
  tab_options(
    table.font.names               = c(font, "Helvetica", "Arial"),
    table.background.color         = bg,
    table.font.color               = ink,
    table.border.top.color         = bg,
    table.border.bottom.color      = bg,
    heading.align                  = "left",
    heading.title.font.size        = px(22),
    heading.subtitle.font.size     = px(13),
    heading.border.bottom.color    = grid_col,
    column_labels.font.size        = px(12),
    column_labels.font.weight      = "bold",
    column_labels.border.top.color = grid_col,
    column_labels.border.bottom.color = grid_col,
    table_body.hlines.color        = grid_col,
    table_body.border.bottom.color = grid_col,
    data_row.padding               = px(10),
    source_notes.font.size         = px(10),
    source_notes.border.bottom.color = bg
  ) |>
  tab_style(style = cell_text(color = ink_muted),
            locations = list(cells_title(groups = "subtitle"),
                             cells_source_notes(),
                             cells_column_spanners(),
                             cells_column_labels()))

gt_tbl
gtsave(gt_tbl, "ai_clusters_table.png")   # needs install.packages("webshot2")
# gtsave(gt_tbl, "ai_clusters_table.html")