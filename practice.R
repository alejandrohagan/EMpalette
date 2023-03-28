EMpalette<- list(
  all_cols = list(c("#FE000C","#FFFFFF","#5A5A5A","#000000","#0C479D","#B5B5B5","#A6192E","#F05822","#ED8B00","#F2AC33","#FFD800","#B4D405","#00A14D","#00ACA8","#007096","#00A3E0","#002F6C","#3a397B","#7A3183","#BD2F7F","#890c58","#D73872"),1:22, colorblind=TRUE),

primary_cols = list(c("#FE000C","#FFFFFF","#5A5A5A","#000000","#0C479D"),1:5,colorblind=FALSE),
accent_cols= list(c("#B5B5B5","#A6192E","#F05822","#ED8B00","#F2AC33","#FFD800","#B4D405","#00A14D","#00ACA8","#007096","#00A3E0","#002F6C","#3a397B","#7A3183","#BD2F7F","#890c58","#D73872"),1:17, colorblind=TRUE)

)


em_red="#FE000C"

em_white="#FFFFFF"

em_grey="#5A5A5A"

em_gray="#5A5A5A"

em_black="#000000"

em_blue="#0C479D"

em_silver="#B5B5B5"

em_burgundy="#A6192E"

em_vermillion="#F05822"

em_orange="#ED8B00"

em_amber="#F2AC33"

em_yellow="#FFD800"

em_lime="#B4D405"

em_green="#00A14D"

em_turquoise="#00ACA8"

em_sea_blue="#007096"

em_cyan="#00A3E0"

em_indigo="#002F6C"

em_violet="#3a397B"

em_purple="#7A3183"

em_cerise="#BD2F7F"

em_plum="#890c58"

em_ruby="#D73872"

library(monochromeR)

grDevices::colorRampPalette(c(em_vermillion,em_orange,em_amber,em_yellow))(25) %>%
  scales::show_col(labels = FALSE)

grDevices::colorRampPalette(c(em_red,em_cerise,em_ruby,em_plum,em_vermillion))(25) %>%
  scales::show_col(labels = FALSE)


grDevices::colorRampPalette(c(em_red,em_blue,em_purple,em_gray,em_black))(32) %>%
  scales::show_col(labels = FALSE)

grDevices::colorRampPalette(c(em_sea_blue,em_cyan,em_indigo,em_blue))(25) %>%
  scales::show_col(labels = FALSE)

grDevices::colorRampPalette(c(em_purple,em_burgundy,em_orange,em_lime))(32) %>%
  scales::show_col(labels = FALSE)

grDevices::colorRampPalette(c(EMpalette$all_cols[[1]]))(150) %>%
  scales::show_col(labels = FALSE)

document()
load_all()
build()

EMpalette::em.brewer("accent_cols",n = 1000,direction = -1)
