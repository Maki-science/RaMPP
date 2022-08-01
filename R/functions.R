

######## lib.show() #####
#' lib.show
#'
#' @description
#' This function runs the shiny app that provides easy and interactive access and 
#' vizualisation of the Raman spectres of RaMPP. Additionally, this app allows the 
#' comparison of the available spectra and also with a user input spectrum.Upon mouse hovering 
#' on the provided graphs, it will further show which components underly the current positions'
#' pectrum and wether this is a typical peak for a certain polymer.
#'
#'
#' @param 
#' 
#' @example lib.show() # this will directly start the shiny app
#' 
#' @references TODO: Link to the Publication
#' 
#' @export
#' @import shiny
#' @import pracma
#' @import ggplot2
#'
lib.show <- function(){
  shiny::runApp('R')
}

