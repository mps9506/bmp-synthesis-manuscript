## Read data


read_data <- function(path, sheet) {
  
  df <- read_xlsx(path, sheet, na = "N/A") |>
    mutate(Longitude = as.numeric(Longitude),
           Latitude = as.numeric(Latitude),
           Pre = as.numeric(Pre),
           Post = as.numeric(Post),
           log_Pre = log(Pre),
           log_Post = log(Post),
           StudyID = as.factor(StudyID))
  
  ##calculate study length based on number of years in "Data_Year"
  tlength <- lapply(strsplit(df$Data_Year,
                             '\\s*;\\s*'),
                    function(x) unlist(lapply(strsplit(x,'-'), function(y){ z <- as.integer(y); if (length(z)==1L) z else z[1L]:z[2L]; })))
  
  df <- df |> 
    mutate(study_length = tlength) |> 
    mutate(study_length = map_dbl(study_length,
                                  \(x) length(x)))
  
  
  return(df)
  
}

load_df <- function(dataset) {
  if(dataset == "bacteria") {
    path <- "data/NPS-BMP-DB-Part1-v1.05.xlsx"
    sheet <- "FIB_Database"
    df <- read_data(path, sheet) |> 
      filter(Unit != "% Exceeded standard (2000/100ml)",
             Unit != "% Exceeded standard (200/100ml)") |> 
      filter(!(Parameter %in% c("Streptococci")),
             BMP != "No BMP")
  } else {
    path <- "data/NPS-BMP-DB-Part2-v1.05.xlsx"
    sheet <- "Database"
    df <- read_data(path, sheet) |> 
      mutate(Area = case_when(
        Area == "NA" ~ NA,
        Area != "NA" ~ Area
      )) |>
      mutate(Area = as.numeric(Area)) |> 
      filter(Unit == "mg/L")
  }
  return(df)
}


# download Global Aridity data https://figshare.com/ndownloader/files/34377245
download_aridity <- function(url) {
  dir <- "data"
  temp_path <- tempfile(fileext = ".zip")
  if (file.exists(temp_path))  'file alredy exists' else httr::GET(url = url, httr::write_disk(temp_path))
  unzip(zipfile = temp_path, exdir = dir)
}


# download 2021 NLCD data https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2021_land_cover_l48_20230630.zip
download_nlcd <- function(url) {
  options(timeout = max(3600, getOption("timeout")))
  dir <- "data/nlcd"
  temp_path <- tempfile()
  if (file.exists(temp_path))  'file alredy exists' else download.file(url = url, 
                                                                       destfile = temp_path,
                                                                       method = "libcurl",
                                                                       mode = "wb") 
  unzip(zipfile = temp_path, exdir = dir)
}



add_aridity <- function(df) {
  ai <- rast("data/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
  ai <- ai * 0.0001 
  
  df <- df |> 
    filter(!is.na(Longitude), !is.na(Latitude))
  ai_vals <- terra::extract(ai, vect(df, geom = c("Longitude", "Latitude"), crs ="+proj=longlat +datum=WGS84"))
  df$ai <- ai_vals$awi_pm_sr_yr
  
  ai_centered <- scale(df$ai, scale = FALSE)
  df <- df |> 
    mutate(ai_uncentered = ai) |> 
    mutate(ai = ai_centered)
  
  return(list(model_df = df,
              mean = attr(ai_centered, "scaled:center")))
  
}