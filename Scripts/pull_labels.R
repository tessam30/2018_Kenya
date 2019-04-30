# From Lhughes the Unicorn
# requires a labelled dataframe (survey data)

pull_labels = function(df) {
  cols = colnames(df)
  
  get_labels = function(df, col) {
    label_name = attr(df[[col]], 'label')
    
    labels = attr(df[[col]], 'labels')
    
    if (is.null(labels)) {
      return(NA)
    } else{
      data.frame(descrip = label_name, codes = labels) %>% mutate(labels = row.names(.))
    }
  }
  
  lapply(cols, function(x) data.frame(var_name = x, get_labels(df, x))) %>% 
    bind_rows() %>% 
    select(-`get_labels.df..x.`)
}