

#' Title
#'
#' @noRd
#'
fcreate_vars <- function(id, Dict = Dict, name_check = "check"){

  vars <- Dict %>%
    dplyr::filter(type == "Feature") %>%
    dplyr::select(-c("justification", "wsClass","includeApp", "includeJust", "type")) %>%
    dplyr::mutate(id = id,
                  id_in = paste(name_check, .data$nameVariable, sep = "")) %>%
    dplyr::select("id", "id_in", "nameCommon", "targetMin", "targetMax", "targetInitial")

  return(vars)
}



#' Title
#'
#' @noRd
#'
fcustom_checkbox <- function(id, id_in, Dict, titl){

  Dict <- Dict %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
    tibble::deframe()

  shiny::checkboxGroupInput(shiny::NS(namespace = id, id = id_in),
                            shiny::h5(titl),
                            choices = Dict,
                            selected = unlist(Dict))
}



#' Title
#'
#' @noRd
#'
fcustom_slider <- function(id, id_in, nameCommon, targetMin, targetMax, targetInitial){

shiny:: sliderInput(inputId = shiny::NS(namespace = id, id = id_in),
                    label = nameCommon,
                    min = targetMin,
                    max = targetMax,
                    value = targetInitial)
}

#' Custom Drop Down for Cost (Rational Use)
#'
#' @noRd
#'
fcustom_cost <- function(id, id_in, Dict){

  choice <- Dict %>%
    dplyr::filter(categoryID == "Cost") %>%
    dplyr::select(.data$nameCommon, .data$nameVariable) %>%
    tibble::deframe()

  shiny::selectInput(shiny::NS(namespace = id, id = id_in),
                     label =  shiny::h3(" "),
                     choices = choice,
                     multiple = FALSE)
}




#' Title
#'
#' @noRd
#'
create_fancy_dropdown <- function(id, Dict, id_in){

  featureList <- Dict %>%
    dplyr::group_by(category) %>%
    dplyr::select(.data$nameCommon, .data$nameVariable, .data$category) %>%
    dplyr::group_split() %>%
    purrr::set_names(purrr::map_chr(., ~.x$category[1])) %>%
    purrr::map(~ (.x %>% dplyr::select(.data$nameCommon, .data$nameVariable))) %>%
    purrr::map(tibble::deframe)

  shiny::selectInput(shiny::NS(namespace = id, id = id_in),
                     shiny::h4(" "),
                     choices = featureList,
                     # selected = "ANFS_breeding",
                     multiple = FALSE)
}

