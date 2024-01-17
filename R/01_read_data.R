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