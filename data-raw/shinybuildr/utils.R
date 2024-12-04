
fget_percentile <- function(climate){
  if (climate == 1) {
    percentile = 5 # default: 5
  } else if (climate == 2) {
    percentile = 35 # default: 35
  } else if (climate == 3) {
    percentile = 35 # default: 35
  }
  return(percentile)
}



fget_refugiaTarget <- function(climate){
  if (climate == 1) {
    refugiaTarget = 1 # default: 1
  } else if (climate == 2) {
    refugiaTarget = 0.3 # default: 0.3
  } else if (climate == 3) {
    refugiaTarget = NA # not needed for percentile
  }
}

fread_file <- function(input, inputId, ext = "rds"){

  # Load data object
  file <- input[[inputId]] # Filename
  file_ext <- tools::file_ext(file$datapath) # Extension

  shiny::req(file$datapath) # Check the file is correct type and available
  shiny::validate(shiny::need(file_ext == ext, paste0("Please upload a .", ext, " file")))

  #TODO I would like to somehow replace _rds and _csv with the supplied "ext".
  if (ext == "rds"){
    dat <- readr::read_rds(file$datapath)
  }

  if (ext == "csv"){
    dat <- readr::read_csv(file$datapath)
  }
  return(dat)

}
