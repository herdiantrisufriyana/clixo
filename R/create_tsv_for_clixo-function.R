#' Create a TSV file for CliXO input
#'
#' This function create a tab-separated value (TSV) file from a similarity
#' matrix for CliXO input.
#'
#' @param similarity A square matrix of numerics containing
#' feature-feature similarity measures.
#' @param path A character of target file path (do not include file
#' extension).
#' @param heading A logical indicating if heading is included or not. By
#' default, CliXO required no heading.
#'
#' @return output A tab-separated value (TSV) file containing similarity table
#' with rows for each pair of features and three columns for source, target,
#' and similarity.
#'
#' @keywords tab-separated value (TSV), CliXO input
#'
#' @export
#'
#' @examples
#'
#' ## Create input example
#' input=input_example()
#'
#' ## Create a TSV file from a similarity matrix for CliXO input
#' create_tsv_for_clixo(input$similarity,'example')

create_tsv_for_clixo=function(similarity,path,heading=FALSE){
  similarity %>%
    as.data.frame() %>%
    rownames_to_column(var='source') %>%
    gather(target,similarity,-source) %>%
    .[lower.tri(similarity) %>% as.logical(),] %>%
    arrange(desc(similarity)) %>%
    write_tsv(paste0(path,'.tsv'),col_names=heading)
}
