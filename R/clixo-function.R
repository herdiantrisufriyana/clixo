clixo=function(similarity
               ,alpha=0.01
               ,beta=0.5
               ,feature_name='gene'
               ,onto_prefix='CliXO'){

  # Clone forked clixo C++ program from GitHub
  system(paste(c(
    'bash -c'
    ,'"git clone https://github.com/herdiantrisufriyana/clixo_0.3"'
  ),collapse=' '))

  # Format and write similarity to tsv for input
  similarity %>%
    as.data.frame() %>%
    rownames_to_column(var='source') %>%
    gather(target,similarity,-source) %>%
    .[lower.tri(similarity) %>% as.logical(),] %>%
    arrange(desc(similarity)) %>%
    write_tsv(paste0('clixo_0.3/input.tsv',collapse=''),col_names=F)

  # Run clixo algorithm
  system(paste(c(
    'bash -c'
    ,'"clixo_0.3/clixo clixo_0.3/input.tsv'
    ,alpha
    ,beta
    ,feature_name
    ,'> clixo_0.3/ontology.cx"'
  ),collapse=' '))

  # Read output file into R
  cx=
    suppressMessages(suppressWarnings(read_tsv(
      'clixo_0.3/ontology.cx',
      col_names=c('target','source','relation','similarity'),
      col_types=list(
        col_character(),
        col_character(),
        col_character(),
        col_double()
      ),
      skip=sum(
        1,
        suppressMessages(
            suppressWarnings(read_tsv('clixo_0.3/ontology.cx'))
          ) %>%
          setNames('column') %>%
          filter(str_detect(column,'#')) %>%
          nrow()
      )
    ))) %>%
    mutate(
      relation=ifelse(relation=='default','is_a',feature_name),
      source=ifelse(relation=='is_a',paste0('CliXO:',source),source),
      target=paste0(onto_prefix,':',target)
    ) %>%
    select(source,target,similarity,relation)

  # Format target name
  cx$target=
    paste0(
      onto_prefix
      ,':'
      ,str_pad(
        as.numeric(
          gsub(paste0(onto_prefix,':'),'',cx$target)
        ),
        str_count(
          max(as.numeric(
            gsub(paste0(onto_prefix,':'),'',cx$target)
          ))
        ),
        'left',
        '0'
      )
    )

  # Format source name
  cx$source[cx$relation=='is_a']=
    paste0(
      onto_prefix
      ,':'
      ,str_pad(
        as.numeric(
          gsub(paste0(onto_prefix,':'),'',cx$source[cx$relation=='is_a'])
        ),
        str_count(
          max(as.numeric(
            gsub(paste0(onto_prefix,':'),'',cx$source[cx$relation=='is_a'])
          ))
        ),
        'left',
        '0'
      )
    )

  # Remove clixo C++ program
  system('bash -c "rm -r clixo_0.3"')

  # Remove
  cx
}
