
#' Export default color palettes to local environment
#'
#' @return
#' @export
#'
#' @examples
export_cols <- function() {
  em_red <- "#FE000C"
    em_white <- "#FFFFFF"
      em_grey <- "#5A5A5A"
        em_gray <- "#5A5A5A"
          em_black <- "#000000"
            em_blue <- "#0C479D"
              em_silver <- "#B5B5B5"
                em_burgundy <- "#A6192E"
                  em_vermillion <- "#F05822"
                    em_orange <- "#ED8B00"
                      em_amber <- "#F2AC33"
                        em_yellow <- "#FFD800"
                          em_lime <- "#B4D405"
                            em_green <- "#00A14D"
                              em_turquoise <- "#00ACA8"
                                em_sea_blue <- "#007096"
                                  em_cyan <- "#00A3E0"
                                    em_indigo <- "#002F6C"
                                      em_violet <- "#3a397B"
                                        em_purple <- "#7A3183"
                                          em_cerise <- "#BD2F7F"
                                            em_plum <- "#890c58"
                                              em_ruby <- "#D73872"

                                              # Return a list of the variables
vars=list(em_red, em_white, em_grey, em_gray, em_black, em_blue, em_silver,
 em_burgundy, em_vermillion, em_orange, em_amber, em_yellow, em_lime,
em_green, em_turquoise, em_sea_blue, em_cyan, em_indigo, em_violet,
em_purple, em_cerise, em_plum, em_ruby)


names(vars) <- c("em_red", "em_white", "em_grey", "em_gray", "em_black", "em_blue",
"em_silver", "em_burgundy", "em_vermillion", "em_orange", "em_amber",
 "em_yellow", "em_lime", "em_green", "em_turquoise", "em_sea_blue",
 "em_cyan", "em_indigo", "em_violet", "em_purple", "em_cerise",
  "em_plum", "em_ruby")
list2env(vars, envir = .GlobalEnv)

}

