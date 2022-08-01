

######## lib.show() #####
#' lib.show
#'
#' @description
#' This function runs the shiny app that provides easy and interactive access and 
#' vizualisation of the Raman spectres of RaMPP. Additionally, it allows the 
#' comparison of the available spectra and also with a user input spectrum.
#'
#'
#' @param data Data.frame containing your data.
#'
#' @return 
#' 
#' @example lib.show() # this will directly start the shiny app
#' 
#' @export
#' @import shiny
#' @import pracma
#' @import ggplot2
#'
lib.show <- function(){
  shiny::runApp('R')
}

