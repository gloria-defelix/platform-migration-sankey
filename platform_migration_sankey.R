# ============================================================
# Platform Migration Dashboard — GraphPad Prism Enterprise
# Version: 5.0
# Description: HTML Dashboard with 2 Sankeys
# ============================================================

# ── 0. Libraries ─────────────────────────────────────────────
if (!requireNamespace("networkD3",   quietly = TRUE)) install.packages("networkD3")
if (!requireNamespace("dplyr",       quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("htmlwidgets", quietly = TRUE)) install.packages("htmlwidgets")

library(networkD3)
library(dplyr)
library(htmlwidgets)

# ============================================================
# 1. LOAD DATA
# ============================================================

migrations <- read.csv("mock_migration_data.csv", stringsAsFactors = FALSE)
cat("Total users loaded:", nrow(migrations), "\n")

# ============================================================
# 2. SANKEY 1 — Pool → Destination
# ============================================================

flow_1   <- migrations %>%
  group_by(source_pool, dest_platform) %>%
  summarise(value = n(), .groups = "drop")

nodes_1  <- c(unique(flow_1$source_pool), unique(flow_1$dest_platform))
node_df1 <- data.frame(name = nodes_1, stringsAsFactors = FALSE)

links_1  <- flow_1 %>%
  mutate(source = match(source_pool,   nodes_1) - 1,
         target = match(dest_platform, nodes_1) - 1) %>%
  select(source, target, value)

colors_1 <- dplyr::case_when(
  node_df1$name == "Pool_1"         ~ "#1A3A5C",
  node_df1$name == "Pool_2"         ~ "#2C5F8A",
  node_df1$name == "Pool_3"         ~ "#4A90D9",
  node_df1$name == "Pool_4"         ~ "#7FB3E8",
  node_df1$name == "Pool_5"         ~ "#B3D4F5",
  node_df1$name == "R / RStudio"    ~ "#16A085",
  node_df1$name == "Jupyter"        ~ "#F39C12",
  node_df1$name == "Other Software" ~ "#8E44AD",
  node_df1$name == "Unknown"        ~ "#95A5A6",
  TRUE                               ~ "#BDC3C7"
)

sankey_1 <- sankeyNetwork(
  Links = as.data.frame(links_1), Nodes = node_df1,
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", units = "users", fontSize = 13,
  nodeWidth = 28, nodePadding = 18, sinksRight = TRUE,
  colourScale = paste0(
    'd3.scaleOrdinal().domain([',
    paste0('"', node_df1$name, '"', collapse = ","),
    ']).range([', paste0('"', colors_1, '"', collapse = ","), '])'
  )
)

# ============================================================
# 3. SANKEY 2 — Destination → Training Requirement
# ============================================================

flow_2   <- migrations %>%
  filter(dest_platform != "Unknown") %>%
  group_by(dest_platform, training_requirement) %>%
  summarise(value = n(), .groups = "drop")

nodes_2  <- c(unique(flow_2$dest_platform), unique(flow_2$training_requirement))
node_df2 <- data.frame(name = nodes_2, stringsAsFactors = FALSE)

links_2  <- flow_2 %>%
  mutate(source = match(dest_platform,        nodes_2) - 1,
         target = match(training_requirement, nodes_2) - 1) %>%
  select(source, target, value)

colors_2 <- dplyr::case_when(
  node_df2$name == "R / RStudio"    ~ "#16A085",
  node_df2$name == "Jupyter"        ~ "#F39C12",
  node_df2$name == "Other Software" ~ "#8E44AD",
  node_df2$name == "Self-Study"     ~ "#27AE60",
  node_df2$name == "Instructor-Led" ~ "#E74C3C",
  node_df2$name == "Self-Directed"  ~ "#F1C40F",
  TRUE                               ~ "#BDC3C7"
)

sankey_2 <- sankeyNetwork(
  Links = as.data.frame(links_2), Nodes = node_df2,
  Source = "source", Target = "target", Value = "value",
  NodeID = "name", units = "users", fontSize = 13,
  nodeWidth = 28, nodePadding = 18, sinksRight = TRUE,
  colourScale = paste0(
    'd3.scaleOrdinal().domain([',
    paste0('"', node_df2$name, '"', collapse = ","),
    ']).range([', paste0('"', colors_2, '"', collapse = ","), '])'
  )
)

# ============================================================
# 4. SAVE INDIVIDUAL SANKEYS TO TEMP FILES
# ============================================================

saveWidget(sankey_1, "temp_sankey1.html", selfcontained = TRUE)
saveWidget(sankey_2, "temp_sankey2.html", selfcontained = TRUE)

# Read them back as raw HTML strings
s1_html <- paste(readLines("temp_sankey1.html"), collapse = "\n")
s2_html <- paste(readLines("temp_sankey2.html"), collapse = "\n")

# ============================================================
# 5. SUMMARY STATS
# ============================================================

total    <- nrow(migrations)
migrated <- sum(migrations$status == "Migrated")
pending  <- sum(migrations$status == "Pending")
top_dest <- names(sort(table(migrations$dest_platform), decreasing = TRUE))[1]
top_train <- names(sort(table(migrations$training_requirement), decreasing = TRUE))[1]

# ============================================================
# 6. BUILD COMBINED HTML DASHBOARD
# ============================================================

html_out <- paste0('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<title>License Migration Dashboard</title>
<style>
  body { font-family: Segoe UI, Arial, sans-serif; background: #0f1117; color: #e0e0e0; padding: 24px; margin: 0; }
  .header { border-left: 4px solid #4A90D9; padding-left: 16px; margin-bottom: 32px; }
  .header h1 { font-size: 22px; font-weight: 600; color: #fff; }
  .header p { font-size: 12px; color: #888; margin-top: 4px; }
  .stats { display: flex; gap: 16px; margin-bottom: 32px; flex-wrap: wrap; }
  .stat { background: #1a1d27; border: 1px solid #2a2d3a; border-radius: 8px; padding: 16px 24px; flex: 1; min-width: 130px; }
  .stat .lbl { font-size: 11px; color: #888; text-transform: uppercase; letter-spacing: 0.5px; }
  .stat .val { font-size: 26px; font-weight: 700; color: #4A90D9; margin-top: 4px; }
  .stat .sub { font-size: 11px; color: #666; margin-top: 2px; }
  .stat .val-sm { font-size: 15px; font-weight: 700; color: #4A90D9; margin-top: 8px; }
  .card { background: #1a1d27; border: 1px solid #2a2d3a; border-radius: 8px; padding: 24px; margin-bottom: 24px; }
  .card h2 { font-size: 15px; font-weight: 600; color: #fff; margin-bottom: 4px; }
  .card p { font-size: 12px; color: #888; margin-bottom: 16px; }
  .sankey-wrap { width: 100%; overflow-x: auto; }
  .sankey-wrap iframe { width: 100%; height: 500px; border: none; background: transparent; }
  .footer { text-align: center; font-size: 11px; color: #444; margin-top: 24px; }
</style>
</head>
<body>

<div class="header">
  <h1>GraphPad Prism Enterprise &mdash; License Migration Dashboard</h1>
  <p>Generated: ', format(Sys.Date(), "%B %d, %Y"), ' &nbsp;|&nbsp; Source: GraphPad Prism Enterprise &nbsp;|&nbsp; 5 License Pools</p>
</div>

<div class="stats">
  <div class="stat">
    <div class="lbl">Total Users</div>
    <div class="val">', format(total, big.mark=","), '</div>
    <div class="sub">across 5 pools</div>
  </div>
  <div class="stat">
    <div class="lbl">Migrated</div>
    <div class="val">', format(migrated, big.mark=","), '</div>
    <div class="sub">', round(migrated/total*100,1), '% of total</div>
  </div>
  <div class="stat">
    <div class="lbl">Pending</div>
    <div class="val">', format(pending, big.mark=","), '</div>
    <div class="sub">', round(pending/total*100,1), '% unknown dest.</div>
  </div>
  <div class="stat">
    <div class="lbl">Top Destination</div>
    <div class="val-sm">', top_dest, '</div>
    <div class="sub">most common target</div>
  </div>
  <div class="stat">
    <div class="lbl">Top Training Need</div>
    <div class="val-sm">', top_train, '</div>
    <div class="sub">most common requirement</div>
  </div>
</div>

<div class="card">
  <h2>License Pool Migration Flow</h2>
  <p>How users from each Enterprise license pool are migrating to destination platforms</p>
  <div class="sankey-wrap">
    <iframe srcdoc="', gsub('"', '&quot;', s1_html), '"></iframe>
  </div>
</div>

<div class="card">
  <h2>Destination Platform &mdash; Training Requirements</h2>
  <p>Training needs of migrated users by destination platform (Unknown destinations excluded)</p>
  <div class="sankey-wrap">
    <iframe srcdoc="', gsub('"', '&quot;', s2_html), '"></iframe>
  </div>
</div>

<div class="footer">GraphPad Prism Enterprise License Migration &nbsp;|&nbsp; Confidential &nbsp;|&nbsp; Mock Data</div>

</body>
</html>')

writeLines(html_out, "migration_dashboard.html")

# Clean up temp files
file.remove("temp_sankey1.html")
file.remove("temp_sankey2.html")

cat("\n✅ Dashboard saved: migration_dashboard.html\n")
cat("   2 interactive Sankey charts\n")
cat("   5 summary stat cards\n")
cat("   Total users:", total, "\n")
