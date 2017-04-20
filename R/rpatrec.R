#' rpatrec: A package to recognise Patterns in (financial) time series Data
#'
#' Generating visual charting patterns and noise, smoothing to find a signal in noisy time series and enabling users to apply their findings to real life data.
#'
#' @section A - Data generation and Input:\itemize{
#' \item{\link{generator}{ Generate your own Patterns}}
#' \item{\link{noise}{ Generate artificial Noise}}
#' \item{\link{sample.pre}{ Prepare real sample data for use with the package}}
#'}
#' @section B - Smoothing and Signal Processing:\itemize{
#' \item{\link{mav}{ Moving Average/Median}}
#' \item{\link{kernel}{ Kernel Regression}}
#' \item{\link{savgolay}{ Savitzky-Golay Filter}}
#' \item{\link{splines}{ Smoothing Splines}}
#' \item{\link{loess.rpatrec}{ LOESS}}
#'}
#' @section C - Recognising Patterns & Testing:\itemize{
#' \item{\link{interpret}{ Recognise different Patterns in time series data}}
#' \item{\link{slicer}{ Recognise multiple Patterns in time series data}}
#' \item{\link{test.smoother}{ Test smoothing algorithms with generated data}}
#'}
#'
#'@section Annexe - Data:\itemize{
#'\item{\link{data}{ Stock Market Indices - Daily Closing Prices}}
#'}
#'
#'
#'
#' @docType package
#' @name rpatrec
NULL
