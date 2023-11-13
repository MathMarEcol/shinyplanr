

#' Title
#'
#' @noRd
#'
fcreate_vars <- function(id, Dict = Dict, name_check = "check"){

  vars <- Dict %>%
    dplyr::filter(Type == "Feature") %>%
    dplyr::select(-c("Justification", "WS_Class","IncludeApp", "IncludeJust", "Type")) %>%
    dplyr::mutate(id = id,
                  id_in = paste(name_check, .data$NameVariable, sep = "")) %>%
    dplyr::select("id", "id_in", "NameCommon", "TargetMin", "TargetMax", "TargetInitial")

  return(vars)
}



#' Title
#'
#' @noRd
#'
fcustom_checkbox <- function(id, id_in, Dict, titl){

  Dict <- Dict %>%
    dplyr::select(.data$NameCommon, .data$NameVariable) %>%
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
fcustom_slider <- function(id, id_in, NameCommon, TargetMin, TargetMax, TargetInitial){

shiny:: sliderInput(inputId = shiny::NS(namespace = id, id = id_in),
                    label = NameCommon,
                    min = TargetMin,
                    max = TargetMax,
                    value = TargetInitial)
}

#' Custom Drop Down for Cost (Rational Use)
#'
#' @noRd
#'
fcustom_cost <- function(id, id_in, Dict){

  choice <- Dict %>%
    dplyr::filter(CategoryID == "Cost") %>%
    dplyr::select(.data$NameCommon, .data$NameVariable) %>%
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
    dplyr::group_by(Category) %>%
    dplyr::select(.data$NameCommon, .data$NameVariable, .data$Category) %>%
    dplyr::group_split() %>%
    purrr::set_names(purrr::map_chr(., ~.x$Category[1])) %>%
    purrr::map(~ (.x %>% dplyr::select(.data$NameCommon, .data$NameVariable))) %>%
    purrr::map(tibble::deframe)

  shiny::selectInput(shiny::NS(namespace = id, id = id_in),
                     shiny::h4(" "),
                     choices = featureList,
                     # selected = "ANFS_breeding",
                     multiple = FALSE)
}

