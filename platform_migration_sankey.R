# ============================================================
# Platform Migration Sankey — GraphPad Prism Enterprise
# Workflow: Phase 1c
# Description: User-level license migration mapping
# Library: networkD3
# ============================================================

# ── 0. Install & Load Libraries ─────────────────────────────
if (!requireNamespace("networkD3",   quietly = TRUE)) install.packages("networkD3")
if (!requireNamespace("dplyr",       quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

library(networkD3)
library(dplyr)
library(htmlwidgets)

# ============================================================
# 1. LOAD DATA
#    In Phase 2 this will be replaced by a live data source
#    (SQL query, API pull, or automated CSV drop)
# ============================================================

migrations <- read.csv("mock_migration_data.csv", stringsAsFactors = FALSE)

cat("=== Data Loaded ===\n")
cat("Total users:", nrow(migrations), "\n")
cat("\nColumn names:", paste(names(migrations), collapse = ", "), "\n")
cat("\nPreview:\n")
print(head(migrations, 5))

# ============================================================
# 2. BUILD SANKEY NODES & LINKS
#    Flow: source_license → dest_platform
# ============================================================

flow_data <- migrations %>%
  group_by(source_license, dest_platform) %>%
  summarise(value = n(), .groups = "drop")

# Build unique node list (order matters for 0-based indexing)
nodes_source <- unique(flow_data$source_license)
nodes_dest   <- unique(flow_data$dest_platform)
all_nodes    <- c(nodes_source, nodes_dest)

node_df <- data.frame(name = all_nodes, stringsAsFactors = FALSE)

# Map to 0-based indices required by networkD3
links_df <- flow_data %>%
  mutate(
    source = match(source_license, all_nodes) - 1,
    target = match(dest_platform,  all_nodes) - 1
  ) %>%
  select(source, target, value)

cat("\n=== Sankey Links ===\n")
print(links_df)

# ============================================================
# 3. COLOR PALETTE
# ============================================================

node_colors <- dplyr::case_when(
  node_df$name == "GraphPad Prism Enterprise"          ~ "#2C5F8A",
  node_df$name == "GraphPad Prism (Retained)"          ~ "#27AE60",
  node_df$name == "JMP Pro"                            ~ "#8E44AD",
  node_df$name == "SPSS"                               ~ "#D35400",
  node_df$name == "R / RStudio"                        ~ "#16A085",
  node_df$name == "Python (matplotlib/seaborn)"        ~ "#F39C12",
  node_df$name == "Churned - No Replacement"           ~ "#E74C3C",
  TRUE                                                  ~ "#95A5A6"
)

color_js <- paste0(
  'd3.scaleOrdinal().domain([',
  paste0('"', node_df$name, '"', collapse = ", "),
  ']).range([',
  paste0('"', node_colors, '"', collapse = ", "),
  '])'
)

# ============================================================
# 4. RENDER SANKEY
# ============================================================

sankey <- sankeyNetwork(
  Links        = as.data.frame(links_df),
  Nodes        = node_df,
  Source       = "source",
  Target       = "target",
  Value        = "value",
  NodeID       = "name",
  units        = "users",
  fontSize     = 13,
  nodeWidth    = 28,
  nodePadding  = 18,
  colourScale  = color_js,
  sinksRight   = TRUE
)

# ============================================================
# 5. EXPORT
# ============================================================

output_path <- "sankey_output.html"
saveWidget(sankey, file = output_path, selfcontained = TRUE)

cat("\n✅ Sankey saved to:", output_path, "\n")
cat("Source nodes:", length(nodes_source), "\n")
cat("Destination nodes:", length(nodes_dest), "\n")
cat("Total flow paths:", nrow(links_df), "\n")
