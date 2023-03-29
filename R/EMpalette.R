# List of Color Palettes and the order in which they are printed


#' Complete list of palettes.
#'
#' Use names(EMPalettes) to return all possible palette names. Current choices are:
#' \code{all_cols}, \code{primar_cols}, and \code{accent_cols}.
#' Use \code{\link{em.brewer}} to construct palettes.
#'
#' @export
EMpalettes<- list(
  all_cols = list(c("#FE000C","#FFFFFF","#5A5A5A","#000000","#0C479D","#B5B5B5","#A6192E","#F05822","#ED8B00","#F2AC33","#FFD800","#B4D405","#00A14D","#00ACA8","#007096","#00A3E0","#002F6C","#3a397B","#7A3183","#BD2F7F","#890c58","#D73872"),1:22, colorblind=TRUE),
  primary_cols = list(c("#FE000C","#FFFFFF","#5A5A5A","#000000","#0C479D"),1:5,colorblind=FALSE),
  accent_cols= list(c("#B5B5B5","#A6192E","#F05822","#ED8B00","#F2AC33","#FFD800","#B4D405","#00A14D","#00ACA8","#007096","#00A3E0","#002F6C","#3a397B","#7A3183","#BD2F7F","#890c58","#D73872"),1:17, colorblind=TRUE)
)

# Function for generating palettes

#' EM Palette Generator
#'
#' Color palettes inspired by Color pallets of ExxonMobil. This is an unofficial package based on the
#' branding guidelines found at \href{www.exxonmobil.com}{on Github}.
#' Use \code{\link{colorblind.friendly}} to check whether palettes are colorblind-friendly.
#'
#' @param palette Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols}.
#' @param n Number of desired colors. If number of requested colors is beyond the scope of the palette,
#' colors are automatically interpolated. If n is not provided, the length of the palette is used.
#' @param type Either "continuous" or "discrete". Use continuous if you want to automatically
#' interpolate between colors.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @return A vector of colors.
#' @examples
#' em.brewer("all_cols")
#'
#' em.brewer("accent_cols", direction=-1)
#'
#' em.brewer("primary_cols", 4, override.order=TRUE)
#'
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Petal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_manual(values=em.brewer("primary_cols", 3))
#'
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point(size=2) +
#' scale_color_manual(values=em.brewer("accent_cols", 3))
#'
#' ggplot(data=iris, aes(x=Species, y=Sepal.Width, color=Sepal.Width)) +
#' geom_point(size=3) +
#' scale_color_gradientn(colors=em.brewer("accent_cols"))
#' @keywords colors
#' @export
em.brewer <- function(palette_name, n, type = c("discrete", "continuous"), direction = c(1, -1), override.order=FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- EMpalettes[[palette_name]]

  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette can offer, \n use continuous instead.")
  }

  continuous <-  if(direction==1){grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction==1 & override.order==FALSE){
    palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
  }else if(direction==-1 & override.order==FALSE){
    rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
  } else if(direction==1 & override.order==TRUE){
    palette[[1]][1:n]
  } else{
    rev(palette[[1]])[1:n]
  }

  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  structure(out, class = "palette", name = palette_name)

}

# Function for printing palette

#' @export
#' @importFrom grDevices rgb
#' @importFrom graphics rect par image text

print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.92, n + 1, 1.08, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 2.5, family = "serif")
}


#' Names of colorblind-friendly palettes
#'
#' Lists all palettes that are colorblind-friendly in the package.
#' To be colorblind-friendly, all colors in the palettes must be distinguishable with deuteranopia, protanopia, and tritanopia.
#' Use \code{\link{em.brewer}}  to construct palettes or \code{\link{colorblind.friendly}} to test for colorblind-friendliness.
#'
#'
#' @export
colorblind_palettes <- c("primary_cols", "all_cols", "accent_cols")


# Names whether a palette is colorblind-friendly

#' Colorblind-Friendly Palette Check
#'
#' Checks whether a palette is colorblind-friendly. Colorblind-friendliness tested using the 'colorblindcheck' package.
#' To be colorblind-friendly, all colors in the palettes must be distinguishable with deuteranopia, protanopia, and tritanopia.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols},
#' @examples
#' colorblind.friendly("all_cols")
#' @return TRUE/FALSE value whether palette is colorblind-friendly
#' @export
colorblind.friendly <- function(palette_name){

  `%notin%` <- Negate(`%in%`)

  if (palette_name %notin% names(EMpalettes)) {
    stop("Palette does not exist.")
  }

  friendly <- palette_name %in% colorblind_palettes

  return(friendly)
}


# EMPalette  palettes for plotting with ggplot2

#' EMPalette palettes for plotting with ggplot2
#'
#' Function for using \code{EMPalettes} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primar_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_color_em_d("primary_cols")
#' @export
scale_color_em_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  em.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- EMpalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction==1 & override.order==FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }

  }

  discrete_scale(aesthetics = "colour", scale_name="em_d",
                 palette = em.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}

#' EMBrewer palettes for plotting with ggplot2
#'
#' Function for using \code{EMPalette} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primar_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species)) +
#' geom_violin() +
#' scale_fill_em_d("Lakota")
#' @export
scale_fill_em_d <- function(palette_name, direction=1, override.order=FALSE, ...){
  em.brewer.disc <- function(palette_name, direction = c(1, -1), override.order=FALSE) {

    `%notin%` <- Negate(`%in%`)
    palette <- EMPalettes[[palette_name]]
    if (is.null(palette)|is.numeric(palette_name)){
      stop("Palette does not exist.")
    }

    if (direction %notin% c(1, -1)){
      stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
    }

    function(n) if(direction==1 & override.order==FALSE){
      palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)]
    }else if(direction==-1 & override.order==FALSE){
      rev(palette[[1]][which(palette[[2]] %in% c(1:n)==TRUE)])
    } else if(direction==1 & override.order==TRUE){
      palette[[1]][1:n]
    } else{
      rev(palette[[1]])[1:n]
    }
  }

  discrete_scale(aesthetics = "fill", scale_name="em_d",
                 palette = em.brewer.disc(palette_name=palette_name, direction=direction, override.order=override.order),
                 ...)
}


#' EMpalette palettes for plotting with ggplot2
#'
#' Function for using \code{EMPalettes} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_color_em_c("primary_cols", direction=-1)
#' @export
scale_color_em_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_color_gradientn(colors=em.brewer(palette_name=palette_name, direction=direction, override.order = F),
                        ...)
}


#' EMpalette palettes for plotting with ggplot2
#'
#' Function for using \code{EMpalettes} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @export
scale_fill_em_c <- function(palette_name, direction=1, ...){

  `%notin%` <- Negate(`%in%`)

  if (direction %notin% c(1, -1)){
    stop("Direction not valid. Please use 1 for standard palette or -1 for reversed palette.")
  }

  scale_fill_gradientn(colors=em.brewer(palette_name=palette_name, direction=direction, override.order = F),
                       ...)
}


#' EMpalette palettes for plotting with ggplot2
#'
#' Function for using \code{EMpalettes} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @param ... Other arguments passed on to \code{\link[ggplot2]{discrete_scale}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
#' geom_point() +
#' scale_colour_em_d("primary_cols")
#' @export

scale_colour_em_d <- scale_color_em_d

#' EMpalette palettes for plotting with ggplot2
#'
#' Function for using \code{EMpalettes} colors schemes in \code{ggplot2}. Use \code{\link{scale_color_em_d}} and \code{\link{scale_fill_em_d}}
#' for discrete scales and \code{\link{scale_color_em_c}} and \code{\link{scale_fill_em_c}} for continuous scales.
#'
#' @param palette_name Name of Palette. Choices are:
#' \code{all_cols}, \code{primary_cols}, and \code{accent_cols}.
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param ... Other arguments passed on to \code{\link[ggplot2]{scale_color_gradientn}}
#' @import ggplot2
#' @examples
#' library(ggplot2)
#' ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Length)) +
#' geom_point() +
#' scale_colour_em_c("primary_cols", direction=-1)
#' @export

scale_colour_em_c <- scale_color_em_c



#' View all Palettes available
#'
#' Function for viewing all palettes available in EMpalette.
#'
#' @param n Number of requested colors. If n is left blank, default palette is returned.
#' @param colorblind_only Should only colorblind friendly palettes be returned? Default is set to FALSE.
#' @param sequential Should palettes displayed all at once, or one at a time. Default is all at once (FALSE).
#' @param direction Sets order of colors. Default palette is 1. If direction is -1, palette color order is reversed
#' @param override.order Colors are picked from palette to maximize readability and aesthetics. This means
#' that colors are not always selected in sequential order from the full palette. If override.order is set to TRUE,
#' colors are selected in sequential order from the full palette instead. Default is FALSE.
#' @examples
#' # All Palettes
#' display_all(sequential = FALSE, colorblind_only = FALSE)
#'
#' # All Colorblind Palettes
#' display_all(sequential = FALSE, colorblind_only = TRUE)
#'
#' # 5 Colors of all Palettes
#' display_all(5, sequential = FALSE, colorblind_only = FALSE)
#' @export
#' @importFrom graphics rect par layout polygon


display_all <- function(n, sequential = FALSE, colorblind_only = FALSE, direction = 1, override.order=FALSE){
  if(colorblind_only){
    N = length(colorblind_palettes)
    pal_names = colorblind_palettes
  }else{
    N = length(EMpalettes)
    pal_names = names(EMpalettes)
  }

  orig_pars <- par()

  plot_palette = function(name,n){
    par(mar = c(0.1,0.1,1,0.1))
    nn = ifelse(missing(n), length(em.brewer(name)), n)
    plot(0,type='n',bty='n',xaxt='n',yaxt='n',xlab='',ylab='',
         ylim = c(0,1),xlim=c(0,nn), main = name)
    for(j in 1:nn){
      polygon(x = c(j-1,j-1,j,j),
              y = c(0,1,1,0),
              border = NA,
              col = em.brewer(name, nn, direction= direction,override.order=override.order)[j])
    }
  }

  if(sequential){
    for(i in 1:N){

      if(missing(n)){

        plot_palette(pal_names[i])
        if(i < N) cat("Hit 'Enter' for next palette");readline()

      }else{

        plot_palette(pal_names[i],n)
        if(i < N) cat("Hit 'Enter' for next palette");readline()
      }
    }
  }else{

    if(missing(n)){

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i])

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i])
      }

    } else{

      if(colorblind_only){

        layout(matrix(1:N,6,4))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }else{

        layout(matrix(1:N,8,7))
        for(i in 1:N) plot_palette(pal_names[i],n)

      }

    }

    layout(matrix(1,1,1))
    par(mar = orig_pars$mar)

  }
}
