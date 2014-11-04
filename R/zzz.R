.onAttach <- function(...) {
  packageStartupMessage('cvAUC version: ', utils::packageDescription('cvAUC')$Version)
}
