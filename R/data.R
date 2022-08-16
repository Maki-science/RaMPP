#' Raman spectra of 11 polymers.
#'
#' A data set containing Raman spectra of 11 polymers and their plastisphere after incubation 
#' in fresh and sea water. Details how the spectra were recorded can be taken from the
#' corresponding publication. The dataset was already processed and ready to plot.
#' @examples
#' # load data from package 
#' data("specData", envir = environment())
#' 
#' # get a summary
#' summary(specData)
#' 
#' # see which polymers are included in the data set
#' levels(specData$pol)
#' 
#' @seealso RaMPP.lib()
#' @usage data("specData", envir = environment())
#' @format A data frame with 206400 rows and 6 variables:
#' \describe{
#'   \item{wavenumber}{the wavenumber of the corresponding amplitude (amp)}
#'   \item{pol}{the abreviation of the polymer}
#'   \item{amp}{the amplitude of the corresponding wavenumber}
#'   \item{v}{the polymer variant recorded (1 corresponds always to pristine particle)}
#'   \item{incWater}{the incubation water of the corresponding particle (fresh or sea water)}
#'   \item{polV}{combination of pol and v to have unique identifier, e.g., for plotting}
#' }
#' @source \url{http://www.maki-science.org}
"specData"