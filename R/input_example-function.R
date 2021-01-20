#' Make an input example for clixo package
#'
#' This function create an input example for clixo package.
#'
#' @return output A list of inputs: 1) value, a data frame with rows for
#' instances and columns for features; 2) similarity, a square matrix of
#' numerics containing feature-feature similarity measures.
#'
#' @keywords example data
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' input=input_example()

input_example=function(){
  ## Set an empty list of inputs and random seed
  input=list()
  set.seed(33)

  ## Create example of instance-feature data frame
  input$value=
    mtcars %>%
    as.matrix() %>%
    as.numeric() %>%
    matrix(
      nrow(mtcars)
      ,ncol(mtcars)
      ,T
      ,list(paste0('I',1:nrow(mtcars)),paste0('F',1:ncol(mtcars)))) %>%
    as.data.frame() %>%
    mutate_all(scale) %>%
    .[sample(1:nrow(.),3000,T),] %>%
    `rownames<-`(paste0('I',1:nrow(.)))

  ## Create example of feature similarity matrix using Pearson correlation
  input$similarity=
    input$value %>%
    cor(method='pearson')

  ## Return
  input
}
