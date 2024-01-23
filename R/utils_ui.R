#' Title
#'
#' @noRd
#'
fcreate_vars <- function(id, Dict = Dict, name_check = "check", categoryOut = FALSE) {

  vars <- Dict %>%
    dplyr::filter(.data$type == "Feature") %>%
    dplyr::select(-c("justification", "wsClass", "includeApp", "includeJust", "type")) %>%
    dplyr::mutate(
      id = id,
      id_in = paste(name_check, .data$nameVariable, sep = "")
    )

  if (categoryOut == TRUE) {
    vars <- vars %>%
      dplyr::select("id", "id_in", "nameCommon", "category", "targetMin", "targetMax", "targetInitial")
  } else {
    vars <- vars %>%
      dplyr::select("id", "id_in", "nameCommon", "targetMin", "targetMax", "targetInitial")
  }

  return(vars)
}



#' Title
#'
#' @noRd
#'
fcustom_checkbox <- function(id, id_in, Dict, titl) {
  Dict <- Dict %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
    tibble::deframe()

  shiny::checkboxGroupInput(shiny::NS(namespace = id, id = id_in),
                            shiny::h5(titl),
                            choices = Dict,
                            selected = unlist(Dict)
  )
}

#' Title
#'
#' @noRd
#'
fcustom_slider <- function(id, id_in, nameCommon, targetMin, targetMax, targetInitial) {
  shiny::sliderInput(
    inputId = shiny::NS(namespace = id, id = id_in),
    label = nameCommon,
    min = targetMin,
    max = targetMax,
    value = targetInitial
  )
}

#' Title
#'
#' @noRd
#'
fcustom_sliderCategory <- function(varsIn, labelNum) {
  ctgs <- unique(varsIn$category)

  shinyList <- vector("list", length = length(ctgs) * 2)

  for (ctg in 1:length(ctgs)) {
    feats <- varsIn %>%
      dplyr::filter(category == ctgs[ctg]) %>%
      dplyr::select(-"category")

    shinyList[ctg * 2] <- # times as many entries as you want to have for one category per list: here: title and sliders (=2); for example with gap between =3
      list(purrr::pmap(feats, fcustom_slider))
    shinyList[ctg * 2 - 1] <-
      list(shiny::h3(paste0(labelNum, ".", ctg, " ", ctgs[ctg])))
  }
  # browser()
  return(shinyList)
}

#' Custom Drop Down for Cost (Rational Use)
#'
#' @noRd
#'
fcustom_cost <- function(id, id_in, Dict) {
  choice <- Dict %>%
    dplyr::filter(categoryID == "Cost") %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
    tibble::deframe()

  shiny::selectInput(shiny::NS(namespace = id, id = id_in),
                     label = shiny::h3(" "),
                     choices = choice,
                     multiple = FALSE
  )
}




#' Fancy dropdown menu with categories
#'
#' @noRd
#'
create_fancy_dropdown <- function(id, Dict, id_in) {
  featureList <- Dict %>%
    dplyr::group_by(category) %>%
    dplyr::select(.data$nameCommon, .data$nameVariable, .data$category) %>%
    dplyr::group_split() %>%
    purrr::set_names(purrr::map_chr(., ~ .x$category[1])) %>%
    purrr::map(~ (.x %>% dplyr::select(.data$nameCommon, .data$nameVariable))) %>%
    purrr::map(tibble::deframe)

  shiny::selectInput(shiny::NS(namespace = id, id = id_in),
                     shiny::h4(" "),
                     choices = featureList,
                     # selected = "ANFS_breeding",
                     multiple = FALSE
  )
}
