# app.R ───────────────────────────────────────────────────────────────
# NHS Wait Times Explorer – Shiny app script writing assisted using Gen AI

# ---- Required file checks (before anything else) --------------------
# Checking RDS/GeoJSON created by the data prep script
req_data_files <- c(
  "output_data/appointment_data.rds",
  "output_data/nhs_wait_icb.rds",
  "output_data/nhs_wait_trusts.rds",
  "output_data/icb-boundaries.geojson"
)

missing_data <- req_data_files[!file.exists(req_data_files)]
if (length(missing_data)) {
  stop(
    paste0(
      "\nMissing processed data files:\n  - ",
      paste(missing_data, collapse = "\n  - "),
      "\n\nRun scripts/load_and_transform.R first to build these files.\n"
    )
  )
}

# Trust shapefile pieces that must be downloaded manually
req_trust_files <- file.path(
  "output_data/2022Elective_FPTP_Full",
  c("FPTP_Elective22_Full.shp","FPTP_Elective22_Full.shx",
    "FPTP_Elective22_Full.dbf","FPTP_Elective22_Full.prj")
)

if (!all(file.exists(req_trust_files))) {
  stop(
    paste0(
      "\nTrust shapefile missing.\n",
      "Please download & extract manually:\n",
      "  1. Open: https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/folder/170910088405\n",
      "  2. Download the 2022Elective_FPTP_Full ZIP\n",
      "  3. Extract into: ",
      normalizePath("output_data/2022Elective_FPTP_Full", mustWork = FALSE), "\n",
      "Then re-run the app."
    )
  )
}

# ── Packages ─────────────────────────────────────────────────────────

# Core
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(viridisLite)
library(bslib)

# Necessary for current features
library(leaflet.extras)  
library(rmapshaper)      
library(scales)          
library(stringr)         
library(glue)            
library(data.table)   

# ── 1. DATA ───────────────────────────────────────────────────────────
# Load in GP appointment wait data
gp_df <- readRDS("output_data/appointment_data.rds")[!is.na(long) & !is.na(lat)] %>%
  mutate(icbnm_simp = trimws(gsub("INTEGRATED CARE BOARD|NHS", "", icb23nm)))
gp_sf <- st_as_sf(gp_df, coords = c("long","lat"), crs = 4326, remove = FALSE)

# Load in ICB / Acute Trust data and modify slightly
rtt_icb  <- setDT(readRDS("output_data/nhs_wait_icb.rds"))
rtt_prov <- setDT(readRDS("output_data/nhs_wait_trusts.rds"))
setnames(rtt_icb,  "Provider Parent Name", "icb23nm")
setnames(rtt_prov, c("Provider Parent Name","Provider Org Name","Provider Org Code"),
         c("icb23nm","TrustName","TrustCd"))
setkey(rtt_icb, `Treatment Function Name`, icb23nm)

# Load in ICB shape map file, update for mapping and simplify 
icb_sf <- st_read("output_data/icb-boundaries.geojson", quiet = TRUE) %>%
  mutate(icb23nm    = str_to_upper(icb23nm),
         icbnm_simp = trimws(gsub("INTEGRATED CARE BOARD|NHS", "", icb23nm))) %>%
  select(icb23nm, icbnm_simp, geometry) %>%
  rmapshaper::ms_simplify(keep = 0.2, keep_shapes = TRUE)

# Load in Trust shape map file and simplify 
fptp_trust_sf <- st_read("output_data/2022Elective_FPTP_Full/FPTP_Elective22_Full.shp", quiet = TRUE) %>%
  st_transform(4326) %>%
  rmapshaper::ms_simplify(keep = 0.5, keep_shapes = TRUE)

specialties <- sort(unique(rtt_icb$`Treatment Function Name`))

# Createviridis palette maker (reverse for %≤18 weeks)
make_bound_pal <- function(metric, vals) {
  pal_vec <- viridisLite::viridis(256, option = "D")
  if (metric != "PercentWithin18Weeks") pal_vec <- rev(pal_vec)
  colorNumeric(palette = pal_vec, domain = vals, na.color = "transparent")
}

# ---- Interpretation / licence text (static HTML) --------------------
about_html <- HTML('
<div style="font-size:90%">
  <h4>Interpretation of Information</h4>
  <p>
    This app blends several public NHS datasets to show how long people wait to
    get a routine GP appointment and to start elective (non‑admitted) treatment
    in hospital. Values are aggregated summaries (not individual waits) and have
    been cleaned and transformed as described below.
  </p>

  <ol>
    <li><strong>GP appointments (practice level)</strong>
      <ul>
        <li>Only <em>Attended</em> appointments were kept.</li>
        <li>Restricted to categories closest to routine access: “General Consultation Routine” and “Walk‑in”.</li>
        <li>Records with “Unknown / Data Issue” wait bands were dropped.</li>
        <li>Binned waits were converted to mid‑points (e.g. 2–7 days → 4.5) and a weighted mean and SD were calculated per practice.</li>
        <li>Practices with missing coordinates were excluded from the map (very few).</li>
      </ul>
    </li>

    <li><strong>Hospital RTT waits (trust &amp; ICB parent)</strong>
      <ul>
        <li>Filtered to “Completed Pathways for Non‑Admitted Patients”.</li>
        <li>Week‑band counts were reshaped to long format; cumulative sums gave the median (50th), 95th percentile and % within 18 weeks.</li>
        <li>Figures are summed across the 2025 months processed.</li>
      </ul>
    </li>

    <li><strong>Geographies &amp; joins</strong>
      <ul>
        <li>ICB polygons (42) were simplified for speed; joined by upper‑cased name (<code>icb23nm</code>).</li>
        <li>Trust polygons came from the FPTP elective shapefile and were joined via provider code (<code>TrustCd</code>).</li>
        <li>GPs were linked to ICBs via postcode (ONS directory) or spatial intersection when needed.</li>
      </ul>
    </li>
  </ol>

  <h3 id="data-suppression">Data Suppression</h3>
  <ul>
    <li><strong>FPTP Elective Trust catchment data</strong>: cells ≤7 patients are replaced with 0 and remaining values rounded to the nearest 5; MSOA values ≤7 are suppressed.</li>
    <li><strong>RTT full CSVs (NHS England)</strong>: no additional small‑number suppression in the provider tables used here (already large aggregates by week band).</li>
    <li><strong>GP Appointments (NHSE/Digital)</strong>: counts are routinely rounded to 5; tiny cells may be suppressed in cross‑tabs, but this app uses aggregated practice totals.</li>
    <li><strong>Reference files</strong> (epraccur, ONS Postcode Directory, ICB boundaries): lookup data only; no suppression applied.</li>
  </ul>

  <h4>Reading the colours</h4>
  <p>
    For median and 95th percentile waits, <em>darker colours = longer waits</em>.
    For “% within 18 weeks”, the scale is reversed: darker colours = <em>lower</em> percentages (worse performance).
  </p>

  <h4>Data sources</h4>
  <ul>
    <li>NHS England Referral to Treatment (RTT) statistics – 2024/25 &amp; 2025/26 releases (Jan 2025 - May 2025).</li>
    <li>NHS Digital / NHS England “Appointments in General Practice” (May 2025 cross‑tabs).</li>
    <li>NHS ODS epraccur GP directory &amp; ONS Postcode Directory.</li>
    <li>ICB boundaries (GeoJSON) and DHSC FPTP elective Trust shapefiles.</li>
  </ul>

  <h4>Licensing</h4>
  <p>
    Data are Crown copyright and licensed under the
    <a href="https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/" target="_blank">
      Open Government Licence v3.0
    </a>, unless stated otherwise in original sources. Please credit NHS England / NHS Digital / DHSC when re‑using.
  </p>

  <p style="font-size:85%;color:#555">
    This dashboard can be referenced as “NHS Wait Times Explorer” (2025). An interactive map of RTT and GP appointment waits.
  </p>
</div>
')

howto_html <- HTML('
<div style="font-size:90%">
  <h3>How to use this map</h3>
  <ol>
    <li><strong>Select a Treatment Function</strong> in the left panel. This modifies the ICB / Acute Trust heatmaps.</li>
    <li><strong>Pick a boundary layer</strong> (ICB / Acute Trust / None). Only one can be selected at a time.</li>
    <li><strong>Choose a heatmap metric</strong> (Median, 95th, or within 18w).</li>
    <li><strong>Show GP markers</strong> if you want to see the aproximate waiting time for an appointment. Use the slider to filter by average days waiting and the search box to find a practice.</li>
    <li><strong>Click on the map or markers</strong> to see pop‑ups with more detail. Use “Reset map view” to zoom back out.</li>
  </ol>
</div>
')

# ── 2. UI ─────────────────────────────────────────────────────────────
ui <- page_sidebar(
  title    = "NHS Wait Times Explorer",
  theme    = bs_theme(version = 5),
  fillable = TRUE,                     # <- lets the main area fill the page
  
  # ---- Collapsible sidebar ----
  sidebar = sidebar(
    id          = "controls",
    width       = 320,
    position    = "left",
    open        = TRUE,                # start open
    collapsible = TRUE,                # <- hamburger toggle
    
    selectInput("specialty", "Treatment function (RTT):",
                choices = specialties, selected = specialties[1]),
    
    radioButtons("boundary_layer", "Boundary layer:",
                 choices = c("ICB heatmap" = "icb",
                             "Acute Trust heatmap" = "trust",
                             "None" = "none"),
                 selected = "icb"),
    
    radioButtons("metric", "Heatmap metric:",
                 choices = c("Median wait (weeks)"       = "MedianWaitWeeks",
                             "95th percentile (weeks)"   = "P95WaitWeeks",
                             "% within 18 weeks"         = "PercentWithin18Weeks"),
                 selected = "MedianWaitWeeks"),
    
    checkboxInput("show_gp",    "Show GP markers",    FALSE),
    checkboxInput("cluster_gp", "Cluster GP markers", TRUE),
    
    sliderInput("gp_wait_range", "Filter GP Avg wait (days):",
                min = 0, max = 40, value = c(0, 40), step = 1),
    
    actionButton("reset_view", "Reset map view", class = "btn btn-sm btn-secondary"),
    actionLink("show_help", label = "How to use this map", icon = icon("question-circle"))
  ),
  
  # ---- Main content ----
  div(style = "height:80vh; width:100%;",
      leafletOutput("map", height = "100%", width = "100%")
  ),
  uiOutput("info_text"),
  wellPanel(uiOutput("about_block"))
)

# ── 3. SERVER ─────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  icb_spec <- reactive({
    met <- rtt_icb[.(input$specialty),
                   .(icb23nm, TotalPatients, MedianWaitWeeks,
                     P95WaitWeeks, PercentWithin18Weeks)]
    left_join(icb_sf, met, by = "icb23nm")
  })
  
  trust_spec <- reactive({
    left_join(
      fptp_trust_sf,
      rtt_prov[`Treatment Function Name` == input$specialty,
               .(icb23nm, TrustCd, TrustName, TotalPatients,
                 MedianWaitWeeks, P95WaitWeeks, PercentWithin18Weeks)],
      by = "TrustCd"
    ) %>%
      mutate(icbnm_simp = trimws(gsub("INTEGRATED CARE BOARD|NHS", "", icb23nm)))
  })
  
  gp_filtered <- reactive({
    rng <- input$gp_wait_range
    gp_sf %>% filter(AvgWaitDays >= rng[1], AvgWaitDays <= rng[2])
  })
  
  observe({
    updateSliderInput(session, "gp_wait_range",
                      min   = floor(min(gp_sf$AvgWaitDays, na.rm = TRUE)),
                      max   = ceiling(max(gp_sf$AvgWaitDays, na.rm = TRUE)),
                      value = c(floor(min(gp_sf$AvgWaitDays, na.rm = TRUE)),
                                ceiling(max(gp_sf$AvgWaitDays, na.rm = TRUE))))
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addMapPane("polys",   zIndex = 410) %>%
      addMapPane("markers", zIndex = 420) %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Base") %>%
      addTiles(group = "OSM fallback") %>%
      hideGroup("OSM fallback") %>%
      addLayersControl(
        baseGroups    = c("Base","OSM fallback"),
        overlayGroups = c("ICB","Trust","GP"),
        options       = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lng = -1.5, lat = 52.5, zoom = 6)
  })
  
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("ICB") %>% clearGroup("Trust") %>% clearGroup("GP") %>% clearControls()
    
    metric_col   <- input$metric
    metric_title <- switch(metric_col,
                           "MedianWaitWeeks"      = "Median wait <br/> (wks)",
                           "P95WaitWeeks"         = "95th percentile <br/> wait (wks)",
                           "PercentWithin18Weeks" = "% within <br/> 18 weeks")
    
    # GP palette stays magma
    pal_gp <- colorNumeric(viridisLite::magma(9, direction = -1),
                           gp_sf$AvgWaitDays, na.color = "transparent")
    
    # ---- Boundary layer ----
    if (input$boundary_layer == "icb") {
      df <- icb_spec()
      df$value_col <- df[[metric_col]]
      pal_bound <- make_bound_pal(metric_col, df$value_col)
      
      proxy %>%
        addPolygons(
          data        = df,
          group       = "ICB",
          layerId     = ~icb23nm,
          fillColor   = ~pal_bound(value_col),
          fillOpacity = 0.6,
          color       = "white",
          weight      = 1,
          label       = ~icbnm_simp,
          popup = ~paste0(icbnm_simp, "<br/>",
                          "Treatment function: ", input$specialty, "<br/>",
                          "# of completed appointments: ",label_comma(accuracy = 1)(TotalPatients),"<br/>",
                          "Median wait time: ", MedianWaitWeeks, " wks<br/>",
                          "95th percentile wait: ",  P95WaitWeeks,     " wks<br/>",
                          "% of patients within 18 weeks: ", sprintf("%.1f%%", PercentWithin18Weeks)),
          options     = pathOptions(pane = "polys"),
          highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
        ) %>%
        addLegend("bottomright",
                  pal    = pal_bound,
                  values = df$value_col,
                  title  = metric_title,
                  group  = "ICB")
      
    } else if (input$boundary_layer == "trust") {
      df <- trust_spec()
      df$value_col <- df[[metric_col]]
      pal_bound <- make_bound_pal(metric_col, df$value_col)
      
      proxy %>%
        addPolygons(data = icb_sf, group = "Trust",
                    fillOpacity = 0, color = "black", weight = 0.6,
                    options = pathOptions(pane = "polys")) %>%
        addPolygons(
          data        = df,
          group       = "Trust",
          fillColor   = ~pal_bound(value_col),
          fillOpacity = 0.6,
          color       = "white",
          weight      = 0.3,
          label       = ~paste0(icbnm_simp, ": ", TrustName),
          popup = ~paste0(TrustName, "<br/>",
                          "Treatment function: ", input$specialty, "<br/>",
                          "# of completed appointments: ",label_comma(accuracy = 1)(TotalPatients),"<br/>",
                          "Median wait time: ", MedianWaitWeeks, " wks<br/>",
                          "95th percentile wait: ",  P95WaitWeeks,     " wks<br/>",
                          "% of patients within 18 weeks: ", sprintf("%.1f%%", PercentWithin18Weeks)),
          
          options          = pathOptions(pane = "polys"),
          labelOptions     = labelOptions(direction = "top"),
          popupOptions     = popupOptions(maxWidth = 260),
          highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
        ) %>%
        addLegend("bottomright",
                  pal    = pal_bound,
                  values = df$value_col,
                  title  = metric_title,
                  group  = "Trust")
    }
    
    # ---- GP markers ----
    if (input$show_gp) {
      gp_now <- gp_filtered()
      
      proxy %>%
        addCircleMarkers(
          data        = gp_now,
          lng         = ~long, lat = ~lat,
          radius      = 5,
          stroke      = TRUE,
          weight      = 0.5,
          color       = "black",
          fillColor   = ~pal_gp(AvgWaitDays),
          fillOpacity = 0.9,
          label       = ~GP_NAME,
          popup       = ~paste0(
            "<strong>", GP_NAME, "</strong><br/>",
            "Postcode: ", Postcode, "<br/>",
            "Avg wait: ", round(AvgWaitDays,1), " days<br/>",
            "SD: ", round(SDWaitDays,1), " days<br/>",
            "Appointments (period): ", comma(TotalAppts), "<br/>",
            "ICB: ", icbnm_simp
          ),
          group       = "GP",
          options     = pathOptions(pane = "markers"),
          clusterOptions = if (input$cluster_gp)
            markerClusterOptions(disableClusteringAtZoom = 12, showCoverageOnHover = FALSE)
          else NULL
        ) %>%
        addLegend("topright",
                  pal    = pal_gp,
                  values = gp_sf$AvgWaitDays,
                  title  = "Avg GP wait (days)",
                  group  = "GP") %>%
        addSearchFeatures(
          targetGroups = "GP",
          options = searchFeaturesOptions(
            zoom = 12,
            openPopup = TRUE,
            firstTipSubmit = TRUE,
            autoCollapse = TRUE,
            hideMarkerOnCollapse = TRUE
          )
        )
    }
  })
  
  observeEvent(input$reset_view, {
    leafletProxy("map") %>% setView(lng = -1.5, lat = 52.5, zoom = 6)
  })
  
  # render the little bit of explanatory text
  output$info_text <- renderUI({
    # pick up the user’s choices
    met_label <- switch(input$metric,
                        MedianWaitWeeks      = "median wait time (wks)",
                        P95WaitWeeks         = "95th-percentile wait (wks)",
                        PercentWithin18Weeks = "% within 18 wks"
    )
    HTML(glue::glue("
      <div style='font-size:0.9em; color:#444; margin-top:12px; padding:8px; 
                      background:#f9f9f9; border-left:3px solid #bbb;'>
        <strong>Current Selection:</strong> The map is showing {met_label} 
        for the <em>{input$specialty}</em> treatment function. <br/>
        <strong>Data licence:</strong> © NHS England — OGL v3.0.

      </div>
    "))
  })
  
  output$about_block <- renderUI({
    tags$details(
      open = FALSE,  # set TRUE if you want it expanded by default
      tags$summary(
        tags$span(icon("info-circle"), " Interpretation, data sources & licensing (click to expand)")
      ),
      about_html
    )
  })
  
  observeEvent(input$show_help, {
    showModal(
      modalDialog(
        title = "How to use this map",
        howto_html,   # same HTML chunk as above
        easyClose = TRUE,
        size = "l",
        footer = modalButton("Close")
      )
    )
  })
}

shinyApp(ui, server)

