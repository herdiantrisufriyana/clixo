#' Make an input example for clixo package
#'
#' This function create an input example for several function in clixo package.
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
    rnorm(3000*5) %>%
    matrix(3000,5,T,list(paste0('I',1:3000),paste0('F',1:5))) %>%
    as.data.frame()

  ## Create example of feature similarity matrix using Pearson correlation
  input$similarity=
    input$value %>%
    cor(method='pearson')

  ## Return
  input
}
