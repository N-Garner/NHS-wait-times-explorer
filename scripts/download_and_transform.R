
# Initial package load
## Load Libraries using package manager
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl,data.table,tidyverse,rvest)

# These packages are only needed if you uncomment the map-checking section: leaflet,sf,RColorBrewer

############### Loading in Map-related coding for GP practices ########
# Loading data, downloading data if necessary
## Create a download directory for the data if one doesnt exist
if (!dir.exists("raw_data")) {
  dir.create("raw_data")
}

## Create an output directory for the data if one doesnt exist
if (!dir.exists("output_data")) {
  dir.create("output_data")
}

## Downloading epraccur file if needed (Name and address information and identifying codes for GP Practices in England and Wales.)
if (!file.exists("raw_data/epraccur/epraccur.csv")) {
  message("Downloading and extracting epraccur...")
  epraccur <- "https://files.digital.nhs.uk/assets/ods/current/epraccur.zip"
  epraccur_filename <- file.path("raw_data/", basename(epraccur))
  download.file(url = epraccur, destfile = epraccur_filename, mode = "wb") 
  unzip(epraccur_filename, exdir = file.path("raw_data","epraccur"))
  if (!file.exists(epr_csv)) {
    stop("Failed to find epraccur.csv after extraction")
  }
} else {
  message("epraccur.csv already exists; skipping download.")
}

## Downloading ONS Postcode Directory file if needed (Name and address information and identifying codes for GP Practices in England and Wales.)
if (!file.exists("raw_data/ONS_PD/Data/ONSPD_MAY_2025_UK.csv")) {
  message("Downloading and extracting ONS Postcode Directory...")
  # Downloads ONS Postcode Directory (May 2025) for the UK
  ONS_Postcode_Directory <- "https://www.arcgis.com/sharing/rest/content/items/3be72478d8454b59bb86ba97b4ee325b/data" 
  ONS_filename <- file.path("raw_data", "ONS_Postcode_Directory.zip")
  download.file(url = ONS_Postcode_Directory, destfile = ONS_filename, mode = "wb") 
  unzip(ONS_filename, exdir = file.path("raw_data","ONS_PD"))
  ons_csv <- list.files("raw_data/ONS_PD", "\\.csv$", full.names = TRUE,recursive = T)[1]
  if (!file.exists(ons_csv)) {
    stop("Failed to find ONSPD CSV after extraction")
  }
} else {
  message("ONSPD CSV already exists; skipping download.")
}

## Load the files into R
### Load GP info data
epraccur_data <- fread("raw_data/epraccur/epraccur.csv", header = FALSE)

### Assign the column names per the spec pdf in raw_data/epraccur
setnames(epraccur_data, old = names(epraccur_data), new = c(
  "Organisation_Code",      # 1
  "Name",                   # 2
  "National_Grouping",      # 3
  "High_Level_Geography",   # 4
  "Address_1",              # 5
  "Address_2",              # 6
  "Address_3",              # 7
  "Address_4",              # 8
  "Address_5",              # 9
  "Postcode",               # 10
  "Open_Date",              # 11
  "Close_Date",             # 12
  "Status_Code",            # 13  A=Active, C=Closed, D=Dormant, P=Proposed
  "Org_SubType_Code",       # 14
  "Commissioner_Code",      # 15  sub-ICB/CT/board code
  "Join_Prov_Date",         # 16
  "Left_Prov_Date",         # 17
  "Contact_Telephone",      # 18
  "Null_1",                 # 19
  "Null_2",                 # 20
  "Null_3",                 # 21
  "Amended_Record_Indicator",#22
  "Null_4",                 # 23
  "Provider_Purchaser_Code",# 24
  "Null_5",                 # 25
  "Prescribing_Setting",    # 26
  "Null_6"                  # 27
))

### Load ONS information (see Userguide in raw_data/ONS_PD for information on this data set)
ONS_data <- fread( "raw_data/ONS_PD/Data/ONSPD_MAY_2025_UK.csv",
                   na.strings = c("", "NA"),
                   select     = c("pcd","doterm","icb","lat","long","ctry")) %>%
  .[ is.na(doterm)] %>%
  .[ , `:=`(doterm = NULL)] %>%
  .[, pcd := toupper(gsub("\\s+","", pcd))]

### Load ONS ICB naming key and match icb col name to ONS_data nomenclature
ONS_ICB_key <- fread("raw_data/ONS_PD/Documents/ICB names and codes UK as at 04_23.csv",
                     select = c("ICB23CD","ICB23NM"))%>% 
  setnames(., "ICB23CD", "icb")

# Transform data to produce wanted data set 
## Filter epraccur to just English practices
epr_active <- epraccur_data[Status_Code == "A" & !startsWith(Organisation_Code, "W")] %>%
  .[, c("Null_1","Null_2","Null_3","Null_4","Null_5","Null_6") := NULL]

## Remove white spaces from the postcode
epr_active <- epr_active[,Postcode_nospaces := toupper(gsub("\\s+","", Postcode))]

### Check that this has returned a non-zero number of practices 
cat("Practices currently selected:", nrow(epr_active), "\n")

## Attach ICB names to ONS_data
ONS_data <- ONS_data[ONS_ICB_key, on = "icb"] %>%
  setnames(., "pcd", "Postcode_nospaces")

## Attach ONS data to practice data by postcode
epr_active <- ONS_data[epr_active, on = "Postcode_nospaces"]

## Check missing data
cat("Practices which failed to match:",epr_active[is.na(icb), .N],"of",epr_active[, .N], "\n")
#### There may be some missing, but most of these appear to be overseas territories and therefore not useful for this project. 
epr_active[is.na(lat), unique(Postcode_nospaces)]
# MissingPractices <- epr_active[is.na(lat)]

## Filter to just practices within England
epr_england <- epr_active[ctry == "E92000001"]

# ## Filter and check mapping of practices
# epr_england_sf <- epr_england[, .(Organisation_Code, Name, Postcode, ICB23NM, lat, long)] %>%
#   st_as_sf(.,
#            coords = c("long", "lat"),
#            crs    = 4326,
#            remove = FALSE)
# 
# icb_levels <- sort(unique(epr_england_sf$ICB23NM))
# pal_icb <- colorFactor(
#   palette = brewer.pal(min(length(icb_levels), 8), "Set3"),
#   domain  = icb_levels
# )
# 
# leaflet(epr_england_sf) %>%
#   # addTiles(group = "Base map") %>%
#   addProviderTiles("CartoDB.Voyager", group = "Voyager") %>%
#   addCircleMarkers(
#     lng         = ~long,
#     lat         = ~lat,
#     radius      = 4,
#     color       = ~pal_icb(ICB23NM),
#     fillOpacity = 0.7,
#     stroke      = FALSE,
#     label       = ~Name,
#     popup       = ~paste0(
#       "<strong>", Name, "</strong><br/>",
#       "Postcode: ", Postcode, "<br/>",
#       "ICB: ", ICB23NM)) %>%
#   # addLegend(
#   #   "bottomright",
#   #   pal    = pal_icb,
#   #   values = ~ICB23NM,
#   #   title  = "ICB Region",
#   #   opacity = 0.8
#   # ) %>%
#   addScaleBar(
#     position = "bottomleft",
#     options  = scaleBarOptions(imperial = FALSE)
#   ) 

######### Loading in and calculating GP practice wait times ########
# GP Appointment Wait Times

## Downloading GP Appointment wait time data (May 2025) if not yet downloaded
if (!file.exists("raw_data/Appt_Wait/Mapping.csv")) {
  message("Downloading and extracting GP Appointment wait time data...")
  wt_url <- "https://files.digital.nhs.uk/F4/911CA1/Practice_Level_Crosstab_May_25.zip"
  wt_filename <- file.path("raw_data", "Practice_Level_Crosstable.zip")
  download.file(url = wt_url, destfile = wt_filename, mode = "wb") 
  unzip(wt_filename, exdir = file.path("raw_data","Appt_Wait"))
  ons_csv <- list.files("raw_data/Appt_Wait", "\\.csv$", full.names = TRUE,recursive = T)[1]
  if (!file.exists(ons_csv)) {
    stop("Failed to find GP appointment wait time CSVs after extraction")
  }
} else {
  message("GP appointment wait time CSVs already exists; skipping download.")
}

## Load wait time data in
appt_filelist <- list.files("raw_data/Appt_Wait", pattern = "^Pra.*\\.csv$", full.names = TRUE)
appt_data <- rbindlist(lapply(appt_filelist, fread), use.names = TRUE, fill = TRUE) %>%
  .[, c("APPOINTMENT_MONTH_START_DATE","GP_CODE","GP_NAME","HCP_TYPE","APPT_MODE","NATIONAL_CATEGORY","TIME_BETWEEN_BOOK_AND_APPT","COUNT_OF_APPOINTMENTS","APPT_STATUS")]
appt_mapping <- fread("raw_data/Appt_Wait/Mapping.csv")

cat("Months included in this dataset:",unique(appt_data$APPOINTMENT_MONTH_START_DATE))

# Calculate average wait time per practice

## Filter data down to nessasary dataset
### Only known attended appointments will be used for calculating wait times
### "General Consultation Routine" and "Walk-in" appointments are most relevant for non-urgent services when wanting to get reffered into a specialist 
### Appointment month, Health care professional type, appointment mode will not be taken into consideration, and summarised
### TIME_BETWEEN_BOOK_AND_APPT ==  "Unknown / Data Issue" will be NA 
appt_data_filtered <- appt_data[APPT_STATUS == "Attended"] %>%
  .[NATIONAL_CATEGORY == "General Consultation Routine" | NATIONAL_CATEGORY == "Walk-in"] %>%
  .[TIME_BETWEEN_BOOK_AND_APPT != "Unknown / Data Issue"] %>%
  .[,.(COUNT_OF_APPOINTMENTS = sum(COUNT_OF_APPOINTMENTS)), by = .(GP_CODE,GP_NAME,TIME_BETWEEN_BOOK_AND_APPT)]

## Use the complete function to ensure all practices have data for each field then 
## change TIME_BETWEEN_BOOK_AND_APPT to a mid-point average
appt_data_complete <- setDT(
  appt_data_filtered %>%
    group_by(GP_CODE,GP_NAME) %>%
    complete(TIME_BETWEEN_BOOK_AND_APPT = unique(appt_data_filtered$TIME_BETWEEN_BOOK_AND_APPT),
             fill = list(COUNT_OF_APPOINTMENTS = 0))
) %>%
  .[,AVDAYS := fcase(TIME_BETWEEN_BOOK_AND_APPT == "Same Day", 0,
                     TIME_BETWEEN_BOOK_AND_APPT == "1 Day", 1,
                     TIME_BETWEEN_BOOK_AND_APPT == "2 to 7 Days", 4.5,
                     TIME_BETWEEN_BOOK_AND_APPT == "8  to 14 Days", 11,
                     TIME_BETWEEN_BOOK_AND_APPT == "15  to 21 Days", 18,
                     TIME_BETWEEN_BOOK_AND_APPT == "22  to 28 Days", 25,
                     TIME_BETWEEN_BOOK_AND_APPT == "More than 28 Days", 35)]

## Summarise each practice's data using a weighted mean (to account for the fact that the data is binned)
appt_practice_summary <- appt_data_complete[,.(TotalAppts  = sum(COUNT_OF_APPOINTMENTS),
                                               AvgWaitDays = weighted.mean(AVDAYS, COUNT_OF_APPOINTMENTS),
                                               SDWaitDays  = sqrt(
                                                 weighted.mean(
                                                   (AVDAYS - weighted.mean(AVDAYS, COUNT_OF_APPOINTMENTS))^2,
                                                   COUNT_OF_APPOINTMENTS))),
                                            by = .(GP_CODE, GP_NAME)]

## Quick plot to visualise data
ggplot(appt_practice_summary, aes(x = AvgWaitDays)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "white") +
  # geom_density(aes(y = ..count..), alpha = 0.3) +
  labs(title = "Distribution of Average GP Wait Times",
       x = "Average Wait (days)",
       y = "Number of Practices") +
  theme_minimal()

##### Combine all GP relevant data into a single dataset ########
## Attach ONS data to practice data by postcode
appt_practice_full <- appt_practice_summary[appt_mapping, on = c("GP_CODE","GP_NAME")]
## Practices in the mapping document which don't have calculated appointments
appt_practice_full[is.na(TotalAppts), unique(GP_NAME)]
## Modify epr_england dataset to align with appointment data
epr_england_reduced <-  epr_england[, .(Organisation_Code, Name, Postcode, lat, long)] %>%
  setnames(old = c("Organisation_Code","Name"),new = c("GP_CODE","GP_NAME"))
## Bind Coordinates to GP appointment waiting time data
appt_and_coords <- epr_england_reduced[appt_practice_full, on = c("GP_CODE","GP_NAME")]
## Check where binding did not occur
appt_and_coords[is.na(lat), unique(GP_NAME)]
appt_and_coords[is.na(lat)|is.na(TotalAppts), unique(GP_NAME)]

cat("Practices which do not have complete information:",appt_and_coords[is.na(lat)|is.na(TotalAppts), .N],"of",appt_and_coords[, .N], "\n")

## Modify ICBs so they conform to a specific nomenclature across all datasets 
appt_and_coords <- setnames(appt_and_coords, "ICB_NAME", "icb23nm") %>%
  .[, icb23nm := str_to_upper(icb23nm)]

## Save this data for graphing / shiny
saveRDS(appt_and_coords, file = "output_data/appointment_data.rds")

##### Loading in and calculating Hospital Trust Waiting Times ########
# Loading in data
## Scrape waiting time monthly csvs 
### URL where RTT zip files will be taken from
RTT_24_25 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/"
RTT_25_26 <- "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/"

### Grab all of the zip files that contain the full CSV data files
RTT_Full_CSV_Links_Early25 <- read_html(RTT_24_25) %>% # Read the URL
  html_nodes("a") %>% # Select only links 
  html_attr("href") %>% # Grab the URLs 
  str_subset("\\.zip$") %>% # Filter them to be only zip files
  str_subset("Full-CSV-data") # Ensure these zips are just the FUll csv data files

RTT_Full_CSV_Links_Late25 <- read_html(RTT_25_26) %>% # Read the URL
  html_nodes("a") %>% # Select only links 
  html_attr("href") %>% # Grab the URLs 
  str_subset("\\.zip$") %>% # Filter them to be only zip files
  str_subset("Full-CSV-data")# Ensure these zips are just the FUll csv data files

### Join lists and ensure there are no duplicates 
RTT_Full_CSV_Links <- unique(c(RTT_Full_CSV_Links_Early25,RTT_Full_CSV_Links_Late25))

### Only keep 2025 data
RTT_Full_CSV_Links_2025 <- RTT_Full_CSV_Links[grepl("25-", basename(RTT_Full_CSV_Links))]

### Creates a new folder if one doesnt exist
dir.create("raw_data/nhw_wait", showWarnings = FALSE)
zip_dest <- file.path("raw_data/nhw_wait", basename(RTT_Full_CSV_Links_2025))

## Download files if needed 
for (i in seq_along(RTT_Full_CSV_Links_2025)) {
  if (!file.exists(zip_dest[i])) {
    message("Downloading: ", RTT_Full_CSV_Links_2025[i])
    download.file(RTT_Full_CSV_Links_2025[i], zip_dest[i], mode = "wb")
    unzip(zip_dest[i], exdir = file.path("raw_data","nhs_wait"))
  } else {message(basename(RTT_Full_CSV_Links_2025[i])," already downloaded; skipping.")
  }
}

## Read in and bind all CSVs
rtt_list <- list.files("raw_data/nhs_wait", "\\.csv$", full.names = TRUE,recursive = T)
rtt_read  <- lapply(rtt_list, fread)
RTT_Raw_Data <- rbindlist(rtt_read, use.names = TRUE, fill = TRUE)

## Filter down dataset
### Completed Pathways For Non-Admitted Patients only - This is most relevant to this project
RTT_Reduced <- RTT_Raw_Data[`RTT Part Description` == "Completed Pathways For Non-Admitted Patients"] %>%
  .[, c("RTT Part Description","Provider Parent Org Code",
        "Commissioner Parent Org Code", "Commissioner Parent Name",
        "Commissioner Org Code","Commissioner Org Name",
        "RTT Part Type",
        "Treatment Function Code"
  ) := NULL] %>% # Remove columns that are not necessary to looking at provider parents and organisation wait times
  .[, c("Total","Total All") := NULL] %>% # Remove patient totals (they can be calculated later)
  .[, "Patients with unknown clock start date" := NULL] %>% # Remove patient with an unknown start date - These won't be used for median wait time calculations
  .[, `Treatment Function Name` := fcase(`Treatment Function Name` == "Total", "All", 
                                         default = `Treatment Function Name`)] # Change total to all for easier understanding

## List all columns which are "Week - to Week -" 
week_cols <- grep("^Gt \\d{2} To \\d{2} Weeks", names(RTT_Raw_Data), value = TRUE)

RTT_Long <- melt(data = RTT_Reduced,
                 id.vars = c( "Provider Parent Name","Provider Org Name","Provider Org Code","Treatment Function Name"),
                 measure.vars = week_cols,
                 variable.name = "Weeks",
                 value.name = "Patients",
                 na.rm = TRUE)

## Take the first number (0 in Gt 0 to 1 weeks) and make it a numeric column
RTT_Long[, Weeks_Numeric := as.integer(str_extract(Weeks, "(?<=Gt )\\d+"))]

## Calculate median wait, 95th percentile wait and % within 18 weeks for ICBs and Organisations
### Summarise number of patients over all moths for ICBs
RTT_Y2D_ICBs <- RTT_Long[, .(Patients = sum(as.numeric(Patients), na.rm = TRUE)),
                         by = .(`Provider Parent Name`, `Treatment Function Name`, Weeks_Numeric)]

### Calculate wanted metrics
RTT_Summary_ICBs <- RTT_Y2D_ICBs[
  , {
    # total patients for this Trust × Specialty
    tot <- sum(Patients, na.rm = TRUE)
    # Order by week # & calculate cumulative sum
    dt <- .SD[order(Weeks_Numeric)]
    dt[, cumu := cumsum(Patients)]
    # Calculate median (50th percentile)
    med50 <- dt[cumu >= tot * 0.50, Weeks_Numeric][1]
    # 95th percentile
    p95   <- dt[cumu >= tot * 0.95, Weeks_Numeric][1]
    # percent within 18 weeks
    pct18 <- sum(dt[Weeks_Numeric <= 18, Patients], na.rm = TRUE) / tot * 100
    # return summary including total patients
    .(TotalPatients          = tot,
      MedianWaitWeeks        = med50,
      P95WaitWeeks           = p95,
      PercentWithin18Weeks   = pct18)
  },  by = .(`Provider Parent Name`, `Treatment Function Name`)] 

### Summarise number of patients over all moths for all providers under parent orgs
RTT_Y2D_Providers <- RTT_Long[, .(Patients = sum(as.numeric(Patients), na.rm = TRUE)),
                              by = .(`Provider Parent Name`, `Provider Org Name`,`Provider Org Code`,`Treatment Function Name`, Weeks_Numeric)] 

### Calculate wanted metrics
RTT_Summary_Providers <- RTT_Y2D_Providers[
  , {
    # total patients for this Trust × Specialty
    tot <- sum(Patients, na.rm = TRUE)
    # Order by week # & calculate cumulative sum
    dt <- .SD[order(Weeks_Numeric)]
    dt[, cumu := cumsum(Patients)]
    # Calculate median (50th percentile)
    med50 <- dt[cumu >= tot * 0.50, Weeks_Numeric][1]
    # 95th percentile
    p95   <- dt[cumu >= tot * 0.95, Weeks_Numeric][1]
    # percent within 18 weeks
    pct18 <- sum(dt[Weeks_Numeric <= 18, Patients], na.rm = TRUE) / tot * 100
    # return summary including total patients
    .(TotalPatients          = tot,
      MedianWaitWeeks        = med50,
      P95WaitWeeks           = p95,
      PercentWithin18Weeks   = pct18)
  },  by = .(`Provider Parent Name`,`Provider Org Name`,`Provider Org Code`,`Treatment Function Name`)]

# Save this data for graphing / shiny
saveRDS(RTT_Summary_ICBs, file = "output_data/nhs_wait_icb.rds")
saveRDS(RTT_Summary_Providers, file = "output_data/nhs_wait_trusts.rds")

######### Download Shapemaps ########
## Downloading file if not yet downloaded
if (!file.exists("output_data/icb-boundaries.geojson")) {
  message("Downloading ICB Boundary file...")
  wt_url <- "https://nihr.opendatasoft.com/api/explore/v2.1/catalog/datasets/icb-boundaries/exports/geojson?lang=en&timezone=Europe%2FLondon"
  wt_filename <- file.path("output_data", "icb-boundaries.geojson")
  download.file(url = wt_url, destfile = wt_filename, mode = "wb") 
} else {
  message("ICB geojson file already exists; skipping")
}

## This file unfortunately needs to be downloaded manually 
## Extract all files into output_data/2022Elective_FPTP_Full
"https://app.box.com/s/qh8gzpzeo1firv1ezfxx2e6c4tgtrudl/folder/170910088405?download=1"
## This can be found through a dashboard at: https://www.eastsussexjsna.org.uk/resources/nhs-acute-hospital-trust-catchment-populations/ 

