# Load packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  refugees,    
  wbstats,     
  tidyverse,   
  unhcrthemes, 
  sf,          
  showtext,   
  patchwork, 
  httr,    
  jsonlite,   
  ggrepel,  
  ggsflabel,  
  cowplot 
)

# Load UNHCR font if available
tryCatch({
  font_add("Lato", regular = "C:/Windows/Fonts/Lato/Lato-Medium.ttf")
  showtext_auto()
}, error = function(e) {
  warning("Could not load Lato font. Falling back to system font.")
})

# Set up parameters - can be adapted as needed
config <- list(
  year = 2024,
  min_refugee_threshold = 20000,
  api_base_url = "https://api.unhcr.org/population/v1/population/",
  geo_data = list(
    poly_url = "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_polygons_simplified.json",
    line_url = "https://raw.githubusercontent.com/GDS-ODSSS/unhcr-dataviz-platform/master/data/geospatial/world_lines_simplified.json"
  )
)

# Function to fetch and clean refugee data
fetch_refugee_data <- function(year, base_url) {
  response <- GET(paste0(base_url, "?limit=500&coa_all=TRUE&year=", year))
  
  if (status_code(response) != 200) {
    stop("Failed to fetch data from UNHCR API")
  }
  
  data <- content(response, as = "parsed", type = "application/json")
  
  df <- bind_rows(lapply(data$items, function(x) {
    as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
  }))
  
  suppressWarnings({
    df %>%
      mutate(
        across(
          c(refugees:hst),
          ~ if_else(is.na(as.numeric(.x)), 0, as.numeric(.x))
        )
      )
  })
}

# Function to process refugee data
process_refugee_data <- function(ref_data, wb_info, min_threshold) {
  ref_data %>%
    mutate(total = refugees + asylum_seekers) %>%
    group_by(coa_name, coa_iso) %>%
    summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
    left_join(wb_info, by = c("coa_iso" = "iso3c")) %>%
    filter(
      income_level %in% c("Low income", "Lower middle income"),
      total >= min_threshold
    )
}

# Function to create map labels
create_map_labels <- function(data) {
  data %>%
    mutate(
      legend = case_when(
        total < 1e4 ~ "<10k",
        total < 1e5 ~ "10k-100k",
        total < 1e6 ~ "100k-1M",
        is.na(total) ~ NA_character_,
        TRUE ~ ">1M"
      ) %>%
        factor(levels = c("<10k", "10k-100k", "100k-1M", ">1M")),
      income_level = case_when(
        income_level == "Low income" ~ "Low-income countries",
        income_level == "Lower middle income" ~ "Lower-middle-income countries"
      )
    )
}

# Function to create base map
create_base_map <- function(poly, line) {
  ggplot() +
    geom_sf(data = poly, aes(fill = income_level), color = "transparent") +
    geom_sf(data = line, aes(linetype = type), color = "white", linewidth = .25, show.legend = FALSE) +
    labs(caption = "Data from mid-2024. Source: UNHCR Refugee Data Finder - © UNHCR, The UN Refugee Agency.<br>The boundaries and names shown and the designations used on this map do not imply official endorsement or acceptance by the United Nations.") +
    scale_linetype_manual(values = c(1, 2, 3, 4)) +
    scale_fill_unhcr_d(palette = "pal_blue") +
    coord_sf(crs = st_crs("ESRI:54030"), expand = FALSE) +
    theme_unhcr(void = TRUE, legend_text_size = 9) +
    guides(fill = "none")
}

# Function to create info boxes
create_info_boxes <- function(data) {
  n_countries <- data %>%
    group_by(income_level) %>%
    summarise(n = n(), .groups = "drop")
  
  total_refugees <- sum(data$total, na.rm = TRUE)
  
  list(
    box1 = data.frame(
      x = 4, y = 3.5, h = 3, w = 6,
      value = paste0(sum(n_countries$n), " countries"),
      info = sprintf("%.1fM refugees &\nasylum-seekers\n", total_refugees/10e5),
      color = factor(1)
    ),
    box2 = data.frame(
      x = c(1.4, 4.6),
      y = 1,
      h = 2.2,
      w = 2.9,
      value = n_countries$n,
      info = c("low-income\ncountries", "lower-middle-\nincome countries"),
      color = factor(1:2)
    )
  )
}

# Function to create infographic elements
create_infographic <- function(data) {
  # Calculate key stats
  stats <- list(
    total_refugees = sum(data$total, na.rm = TRUE),
    n_countries = nrow(data),
    low_income = sum(data$income_level == "Low income"),
    lower_middle_income = sum(data$income_level == "Lower middle income")
  )
  
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 2),
              fill = "#18375F", alpha = 0.1) +
    geom_point(aes(x = 2, y = 1), size = 30, color = "#0072BC", alpha = 0.8) +
    geom_text(aes(x = 2, y = 1.2),
              label = sprintf("%.1fM", stats$total_refugees/1e6),
              color = "white", size = 6, fontface = "bold") +
    geom_text(aes(x = 2, y = 0.8),
              label = "refugees &\nasylum-seekers",
              color = "white", size = 3, lineheight = 0.8) +
    # Countries breakdown
    geom_rect(aes(xmin = 4, xmax = 6, ymin = 0.4, ymax = 1.6),
              fill = "#0072BC", alpha = 0.8) +
    geom_text(aes(x = 5, y = 1.2),
              label = stats$n_countries,
              color = "white", size = 6, fontface = "bold") +
    geom_text(aes(x = 5, y = 0.8),
              label = "countries",
              color = "white", size = 3) +
    # Income level breakdown
    geom_text(aes(x = 7.5, y = 1.4),
              label = sprintf("%d low-income\ncountries", stats$low_income),
              color = "#18375F", size = 3, lineheight = 0.8) +
    geom_text(aes(x = 7.5, y = 0.6),
              label = sprintf("%d lower-middle-\nincome countries", stats$lower_middle_income),
              color = "#18375F", size = 3, lineheight = 0.8) +
    theme_void() +
    coord_fixed(ratio = 0.2)
}

# Function to create labeled map
create_labeled_map <- function(poly, line, boxes) {
  base_map <- create_base_map(poly, line) +
    geom_sf_text_repel(
      data = poly %>% distinct(coa_name, .keep_all = TRUE),
      aes(label = coa_name),
      size = 3,
      max.overlaps = 100
    )
  
  # Create info boxes
  plot_box1 <- ggplot(boxes$box1, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = color), alpha = 0.8) +
    geom_text(
      color = "white", fontface = "bold", size = 5,
      aes(label = value, x = x - 2.5, y = y + 1), hjust = 0
    ) +
    geom_text(
      color = "white", fontface = "bold", size = 3,
      aes(label = info, x = x - 2.5, y = y - 0.5), hjust = 0
    ) +
    coord_fixed() +
    scale_fill_unhcr_d(palette = "pal_navy") +
    theme_void() +
    guides(fill = FALSE)
  
  plot_box2 <- ggplot(boxes$box2, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = color), alpha = 0.8) +
    geom_text(
      color = "white", fontface = "bold", size = 5,
      aes(label = value, x = x - 1.25, y = y + 0.5), hjust = 0
    ) +
    geom_text(
      color = "white", fontface = "bold", size = 2.5,
      aes(label = info, x = x - 1.25, y = y - 0.3), hjust = 0
    ) +
    coord_fixed() +
    scale_fill_unhcr_d(palette = "pal_blue") +
    theme_void() +
    guides(fill = FALSE)
  
  boxes_combined <- plot_box1 / plot_box2
  
  base_map +
    inset_element(
      boxes_combined,
      right = .35,
      bottom = .3,
      left = .1,
      top = 1.1
    )
}

# Function to create map with title and boxes
create_enhanced_map <- function(poly, line, boxes) {
  # Create base map
  main_map <- create_base_map(poly, line)
  
  # Create info boxes
  plot_box1 <- ggplot(boxes$box1, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = color), alpha = 0.8) +
    geom_text(
      color = "white", fontface = "bold", size = 5,
      aes(label = value, x = x - 2.5, y = y + 1), hjust = 0
    ) +
    geom_text(
      color = "white", fontface = "bold", size = 3,
      aes(label = info, x = x - 2.5, y = y - 0.5), hjust = 0
    ) +
    coord_fixed() +
    scale_fill_unhcr_d(palette = "pal_navy") +
    theme_void() +
    guides(fill = FALSE)
  
  plot_box2 <- ggplot(boxes$box2, aes(x, y, height = h, width = w, label = info)) +
    geom_tile(aes(fill = color), alpha = 0.8) +
    geom_text(
      color = "white", fontface = "bold", size = 5,
      aes(label = value, x = x - 1.25, y = y + 0.5), hjust = 0
    ) +
    geom_text(
      color = "white", fontface = "bold", size = 2.5,
      aes(label = info, x = x - 1.25, y = y - 0.3), hjust = 0
    ) +
    coord_fixed() +
    scale_fill_unhcr_d(palette = "pal_blue") +
    theme_void() +
    guides(fill = FALSE)
  
  boxes_combined <- plot_box1 / plot_box2
  
  # Create map with info boxes
  map_with_boxes <- main_map +
    inset_element(
      boxes_combined,
      right = .35,
      bottom = .3,
      left = .1,
      top = 1.1
    )
  
  # Format threshold number with thousand separator
  formatted_threshold <- format(config$min_refugee_threshold, big.mark = ",", scientific = FALSE)
  
  title_text <- "Forced Displacement Survey universe"
  subtitle_text <- sprintf("Low and lower middle-income countries hosting more than %s refugees and asylum-seekers,
                          formatted_threshold,
                          config$year)
  
  # Final composition
  ggdraw() +
    draw_plot(map_with_boxes,
             x = 0, y = 0,
             width = 1, height = .9)  +
    draw_label(title_text,
              x = .02, y = .95,
              hjust = 0, vjust = 1,
              fontface = "bold",
              size = 16,
              color = "#18375F",
              fontfamily = "Lato") +
    draw_label(subtitle_text,
              x = 0.02, y = .90,
              hjust = 0, vjust = 1,
              size = 12,
              color = "#18375F",
              fontfamily = "Lato")
}


# Function to process all
main <- function(config) {
  # Fetch and process data
  ref_data <- fetch_refugee_data(config$year, config$api_base_url)
  wb_info <- wb_countries() %>%
    select(iso3c, income_level) %>%
    filter(income_level != "Aggregates")
  
  # Process refugee data
  processed_data <- process_refugee_data(ref_data, wb_info, config$min_refugee_threshold)
  
  # Load and process geographic data
  sf_use_s2(FALSE)
  poly <- read_sf(config$geo_data$poly_url) %>%
    st_set_crs(4326) %>%
    left_join(processed_data, by = c("color_code" = "coa_iso")) %>%
    create_map_labels()
  
  line <- read_sf(config$geo_data$line_url) %>%
    mutate(type = factor(type) %>%
             fct_relevel("solid", "dashed", "dotted", "dashed-dot")) %>%
    st_set_crs(4326)
  
  # Create info boxes
  boxes <- create_info_boxes(processed_data)
  
  # Return all versions
  list(
    base_map = create_base_map(poly, line),
    labeled_map = create_labeled_map(poly, line, boxes),
    enhanced_map = create_enhanced_map(poly, line, boxes),
    poly = poly,
    line = line,
    data = processed_data
  )
}

# Save outputs
save_outputs <- function(plots, prefix = "priority_countries") {
  ggsave(
    paste0(prefix, ".pdf"),
    plots$base_map,
    width = 9, height = 5
  )
  
  ggsave(
    paste0(prefix, "_labeled.pdf"),
    plots$labeled_map,
    width = 9, height = 5
  )
  
  ggsave(
    paste0(prefix, "_enhanced.pdf"),
    plots$enhanced_map,
    width = 9, height = 5,
    device = cairo_pdf
  )
}

# Run the script
plots <- main(config)
save_outputs(plots)