#' StablePopulation: constrained Weibull survivorship profiles
#'
#' @description
#' StablePopulation reconstructs discrete Weibull survivorship profiles under a
#' stable and stationary population assumption. Its core constraint is
#' \eqn{R_0 = \sum_x l_xm_x = 1}, where \eqn{l_x} is survivorship and
#' \eqn{m_x} is age-specific fertility. All calculations use consecutive
#' internal age-class indices \code{0, 1, 2, ..., n - 1}.
#'
#' @details
#' The package is the species-level demographic layer of a wider modelling
#' workflow. It reconstructs survivorship and derives a stable age structure and
#' exit-by-death profile. It does not yet calculate body-mass-weighted biomass,
#' aggregate prey communities, or distribute resources among predators.
#'
#' @section Choosing a reconstruction route:
#' \itemize{
#'   \item Use [reconstruct_population()] when a single Weibull
#'   \eqn{\beta} is already specified.
#'   \item Use [select_beta()] when observed \eqn{l_x} is available. It
#'   scans candidate \eqn{\beta} values, solves \eqn{\alpha} under
#'   \eqn{R_0 = 1} for each candidate, and selects the smallest RMSE.
#'   \item Use [scan_beta()] when observed \eqn{l_x} is unavailable. It
#'   returns candidate scenarios; an optional terminal-survivorship window can
#'   retain profiles compatible with an explicit final-class criterion.
#'   \item Use [fit_weibull_free()] only as a descriptive free two-parameter
#'   comparison. It does not impose \eqn{R_0 = 1}.
#' }
#'
#' @section Fertility normalisation:
#' [normalize_fertility()] explicitly rescales a fertility schedule relative to
#' a reference survivorship profile. It preserves the fertility pattern while
#' changing its overall scale so that the reference \eqn{R_0} equals one.
#' Normalisation is never performed silently by [select_beta()] or
#' [run_reconstruction_excel()]. Use it only when that rescaling is the stated
#' analytical choice.
#'
#' @section Derived demographic quantities:
#' [derive_demographic_profile()] calculates the normalized stable structure
#' \eqn{R_x = l_x / \sum_x l_x}, the raw exit-by-death profile
#' \eqn{D_x = R_x - R_{x+1}} with \eqn{D_n = R_n}, its relative version
#' \eqn{D_x / \sum_x D_x}, and conditional survival
#' \eqn{B_x = l_{x+1}/l_x}. The raw \eqn{D} profile and the relative
#' \eqn{D} profile are distinct objects.
#'
#' @section Excel workflows:
#' [run_reconstruction_excel()] is the recommended external-workbook workflow.
#' In \code{mode = "auto"}, a sheet with observed survivorship uses the
#' selection route; without observed survivorship, a one-value beta column uses
#' the fixed-beta route, and otherwise the scan route is used. The historical
#' no-argument [run_analysis()] function is retained only for compatibility with
#' the StablePopulation 1.0.3 source-tree workbook layout.
#'
#' @references
#' Martín-González, J. A., Rodríguez-Gómez, G., and Palmqvist, P. (2019).
#' Survival profiles from linear models versus Weibull models: Estimating stable
#' and stationary population structures for Pleistocene large mammals. \emph{Journal
#' of Archaeological Science: Reports}, 25, 119--127.
#'
#' @docType package
#' @name StablePopulation
"_PACKAGE"
