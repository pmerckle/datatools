# stats
#
# Statistical functions
#



# chisq.stars ----

#' Graphical interpretation of the chi-square test p-value
#'
#' Returns a graphical interpretation of the chi-square test p-value
#'
#' @param x a numeric vector or matrix. x and y can also both be factors.
#' @param y a numeric vector; ignored if x is a matrix. If x is a factor, y should be a factor of the same length
#' @param correct	a logical indicating whether to apply continuity correction when computing the test statistic for 2 by 2 tables: one half is subtracted from all |O - E| differences; however, the correction will not be bigger than the differences themselves. No correction is done if simulate.p.value = TRUE.
#' @param p a vector of probabilities of the same length of x. An error is given if any entry of p is negative.
#' @param rescale.p	a logical scalar; if TRUE then p is rescaled (if necessary) to sum to 1. If rescale.p is FALSE, and p does not sum to 1, an error is given.
#' @param simulate.p.value	a logical indicating whether to compute p-values by Monte Carlo simulation.
#' @param B an integer specifying the number of replicates used in the Monte Carlo test.
#' @return A character vector.
#' @details
#' The function returns a graphical interpretation of a chi-square test p-value, under the form of a variable number of asterisks : (ns) means p >= 0.05; * means p < 0.05; ** means p < 0.01 ; *** means p < 0.001.
#' @seealso \code{\link[stats]{chisq.test}}
#' @examples
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent", "Republican"))
#' chisq.test(M))  # Prints test summary with chisq.test() function
#' chisq.stars(M) # Prints graphical test summary with stars
#' @export

chisq.stars <- function(x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), rescale.p = FALSE, simulate.p.value = FALSE, B = 2000) {
  res <- chisq.test(x, y, correct, p, rescale.p, simulate.p.value, B)$p.value
  res <- cut(res, breaks=c(0, 0.001, 0.01, 0.05, 1), include.lowest=TRUE, labels=c("***", "**", "*", "(ns)"))
  res(as.character(res))
  return(res)
}



# cramer.v.stars ----

#' Graphical interpretation of Cramer's V
#'
#' Returns a graphical interpretation of the Cramer's V.
#'
#' @param tab table on which to compute  Cramer's V test.
#' @details
#' The function returns a graphical interpretation of the Cramer's V measure of the strength of assocation between two nominal variables, under the form of a variable number of asterisks : no star means V < 0.05; * means V >= 0.05; ** means V >= 0.15 ; *** means V < 0.30.
#' @seealso \code{\link[questionr]{cramer.v}}
#' @examples
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"), party = c("Democrat","Independent", "Republican"))
#' cramer.v(M))  # Computes Cramer's V with cramer.v() function from questionr package
#' cramer.v.stars(M) # Prints graphical Cramer's V summary with stars
#' @import questionr
#' @export

cramer.v.stars <- function(tab) {
  res <- cramer.v(tab)
  res <- cut(res, breaks=c(0, 0.05, 0.015, 0.03, 1), include.lowest=TRUE, labels=c("", "*", "**", "***"))
  res(as.character(res))
  return(res)
}
