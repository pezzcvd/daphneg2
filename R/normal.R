#' normal
#'
#' Internal function. It checks normality of the data as they are or applying some
#' data transformation (log, logit, sqrt, inverse).
#' The function is part of the preprocessing procedure for any GWAS analysis.
#'
#'
#' @param n.par Numeric. Vector of numeric values for a specific
#' phenotype/environmental parameter
#'
#' @return The same vector if the data are normal, the vector of transformed data otherwise
#' @noRd
#' @importFrom stats ks.test rnorm var
#'
#' @examples
#' # Example with a vector of fake data
#' normal(rnorm(1131))
#'
normal = function(n.par) {
  # Control on input
  checkmate::assert_numeric(x = n.par, min.len = 2, max.len = 1131)

  # Testing Normality (kolmogorov-smirnoff)
  # for each of the transformations the test checks whether p-value > 0.001
  m = mean(n.par)
  s = sqrt(var(n.par))
  if (suppressWarnings(ks.test(n.par, rnorm(100, m, s))$p.val) > 0.001) {
    print("Normal")
    return(n.par)
  }

  # Checking log
  m = mean(log(n.par))
  s = sqrt(var(log(n.par)))
  if (suppressWarnings(ks.test(log(n.par), rnorm(100, m, s))$p.val) > 0.001) {
    print("Log is normal")
    return(log(n.par))
  }

  # Checking logit  log(n.par/(1 - exp(-n.par)))
  m = mean(log(n.par/(1 - exp(-n.par))))
  s = sqrt(var(log(n.par/(1 - exp(-n.par)))))
  if (suppressWarnings(ks.test(log(n.par/(1 - exp(-n.par))), rnorm(100, m, s))$p.val) > 0.001) {
    print("Logit is normal")
    return(log(n.par/(1 - exp(-n.par))))
  }

  # Checking sqrt
  m = mean(sqrt(n.par))
  s = sqrt(var(sqrt(n.par)))
  if (suppressWarnings(ks.test(sqrt(n.par), rnorm(100, m, s))$p.val) > 0.001) {
    print("Sqrt is normal")
    return(sqrt(n.par))
  }

  # Checking inverse
  m = mean(n.par^-1)
  s = sqrt(var(n.par^-1))
  if (suppressWarnings(ks.test(n.par^-1, rnorm(100, m, s))$p.val) > 0.001) {
    print("Inverse is normal")
    return(n.par^-1)
  }

  return(NA)
}
