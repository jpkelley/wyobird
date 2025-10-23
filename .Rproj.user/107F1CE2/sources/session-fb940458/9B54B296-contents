# --- Setup ---
source("website_updates/load_packages.R")
library(googlesheets4)
library(dplyr)
library(glue)
library(readr)
library(stringr)
library(purrr)

# Disable authentication for public access
gs4_deauth()

# --- 1. Read the Google Sheet ---
sheet_url <- "https://docs.google.com/spreadsheets/d/1pSY6l2bWfEcewkLU8T09aM777wH8TFMRPkHl_rbMRU4/edit?gid=0#gid=0"
personnel_raw <- read_sheet(sheet_url)

# --- 2. Clean up and prepare fields ---
personnel <- personnel_raw %>%
  mutate(
    id = str_to_lower(paste(first_name, last_name, sep = "_")),
    name = full_name,
    # make image path site-root-relative
    image = file.path("/people", "images", "profile_pics", image_name),
    research = project_description_abbreviated
  ) %>%
  select(id, first_name, last_name, name, role, image, research)

# --- 3. Function to create .qmd file for each person ---
create_person_qmd <- function(id, first_name, last_name, name, role, image, research) {
  file_name <- glue("people/ind_profiles/{first_name}_{last_name}.qmd")
  
  # Build YAML fields (exclude NAs and empty strings)
  yaml_fields <- list(
    title = name,
    name = name,
    role = role,
    image = image,
    research = research,
    format = list(html = list(toc = FALSE))
  )
  yaml_fields <- yaml_fields[!sapply(yaml_fields, function(x) is.na(x) || x == "NA" || x == "")]
  
  # Construct YAML text manually
  yaml_header <- "---\n"
  for (field in names(yaml_fields)) {
    if (is.list(yaml_fields[[field]])) {
      yaml_header <- paste0(yaml_header, field, ":\n")
      for (sub in names(yaml_fields[[field]])) {
        yaml_header <- paste0(yaml_header, "  ", sub, ":\n")
        for (subsub in names(yaml_fields[[field]][[sub]])) {
          yaml_header <- paste0(
            yaml_header,
            "    ", subsub, ": ",
            yaml_fields[[field]][[sub]][[subsub]], "\n"
          )
        }
      }
    } else {
      yaml_header <- paste0(yaml_header, field, ": \"", yaml_fields[[field]], "\"\n")
    }
  }
  yaml_header <- paste0(yaml_header, "---\n")
  
  # Write YAML only (no Markdown body)
  write_lines(yaml_header, file_name)
}

# --- 4. Generate .qmd files ---
pwalk(personnel, create_person_qmd)