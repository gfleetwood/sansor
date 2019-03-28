library(TSclust)
library(parallelDist)
library(rsample)

train_test_split <- function(df, ratio){

    split <- initial_split(data, prop = ratio)
    train <- training(data_split)
    test  <- testing(data_split)

    return(list(train, test))
}

ts_clustering <- function(mat, dist = "dtw", linkage = "average"){
    mat_dist <- parDist(x = stories_temp, method = dist)
    stories_clust <- hclust(mat_dist,  method = linkage)
    return(stories_clust)
}

mode_stats <- function(vec){
    return((tabyl(vec) %>% arrange(desc(n)) %>% pull(1))[1])
}

#' Rounded Mean Function
#'
#' https://stats.stackexchange.com/questions/78609/outlier-detection-in-very-small-sets/78617#78617
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

small_sample_outliers <- function(vec){
    mi <- .6745*(vec - mean(vec))/mad(vec)
    return(vec[abs(mi) > 3.5])
}


#' Rounded Mean Function
#'
#' This function return the mean of vector to n decimal places while ignoring missing values.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

write_txt <- function(fobj, fname){
    con <- file(fname)
    writeLines(c(fobj),  con)
    close(con)
}

#' Rounded Mean Function
#'
#' This function return the mean of vector to n decimal places while ignoring missing values.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

get_dummies <- function(df, col){

    dmy <- dummyVars(paste("~", col), data = df)
    dummied_cols <- data.frame(predict(dmy, newdata = df))
    df2 <- df %>% select_(paste("-", col)) %>% cbind(., dummied_cols)

    return(df2)
}

#' Rounded Mean Function
#'
#' This function return the mean of vector to n decimal places while ignoring missing values.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords
#' @export
#' @examples
#' mean_dp()

make_formula <- function(target, additions, subtractions = NULL, powers = NULL){

    add_sub_sep = ifelse(is.null(subtractions), '', '-')
    add <- paste0(additions, collapse = ' + ')
    subtract <- paste0(subtractions, collapse = ' - ')
    add_subtract <- paste(add, subtract, sep = add_sub_sep)

    return(as.formula(paste(target, add_subtract, sep = '~')))

}

#' Read CSVs Function
#'
#' This function takes a path and reads all the csvs in it into a single dataframe.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

read_csvs <- function(path){
    dfs <- dir(path, pattern ='\\.csv', full.names = T) %>%
        map_df(read.csv, sep =',', header = T, stringAsFactors = F)
    return(dfs)
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

cor_interactive <- function(df){

    plot <- df %>%
        select_if( function(x) is.numeric(x) | is.integer(x)) %>%
        cor() %>%
        plot_ly(
            x = rownames(.),
            y = rownames(.),
            colorscale = "Greys",
            z = .,
            type = "heatmap"
        )
    return(plot)
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

optimal_hist_bins <- function(vec){
    return(diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3)))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

standard_scaler_vec <- function(x){
    return((x - mean(x, na.rm = T))/sd(x, na.rm = T))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

standard_scaler_df <- function(x){
    return((x - mean(x, na.rm = T))/sd(x, na.rm = T))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

mode_stat <- function(vec){
    vals <- vec %>% tabyl() %>% arrange(desc(n)) %>% select(1)
    return(as.character(vals[1,]))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

min_max_scaler_vec <- function(x, max_, min_){
    x_std = (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
    x_scaled = x_std * (max_ - min_) + min_
    return(X_scaled)
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

min_max_scaler_df <- function(df, max_, min_){

    df_other <- df %>%
        select_if(
            function(y) !is.numeric(y) & !is.integer(y)
        )

    df_num <- df %>%
        select_if(
            function(y) is.numeric(y) | is.integer(y)
        )

    result <- map_df(df_num, function (x) min_max_scaler_vec(x, max_, min_))
    return(cbind(df_other, result))
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

ss_sampleseed <- function(x, frac){
    set.seed(8)
    x[sample.int(nrow(x), frac*nrow(x)), ]
    return(x)
}

#' Init Project Function
#'
#' This function sets up all I need to start a new project.
#' @param col A numeric/integer vector
#' @param num_dp The numeric of decimal places to round the mean to.
#' @keywords cats
#' @export
#' @examples
#' mean_dp()

cohens_h <- function(p1, p2){
    return(abs(2*(asin(sqrt(p1)) - asin(sqrt(p2)))))
}


#' A Function
#'
#' A print out of some base R functions which replicate command line operations.
#' @export
#' @examples
#' bash_baseR()

bash_baseR <- function(){
  print('
A list of useful base R bash functions:

* dir
* list.files
* dir.exists
* dir.create
* untar
* file.info
* file.append: Like rbind
* count.fields: Like ncol
* file.rename
        ')
}


#' A Function
#'
#' A command line version of nrow
#' @param f1 Name/path of the file
#' @export
#' @examples
#' bash_nrows()

bash_nrows <- function(f1){
  system(
    glue("wc -l {f1}")
  )
}

#' A Function
#'
#' Returns the number of columns in a file
#' @param f1 Name/path of the file.
#' @param sep File separator. Defaults to csv.
#' @export
#' @examples
#' cat_function()

bash_ncols <- function(f1, sep = ','){
  return(unique(count.fields(f1, sep = sep)))
}

#' A Function
#'
#' Show the top n rows of a file.
#' @param f1 The name/path of the file.
#' @param nrows The number of rows to show.
#' @export
#' @examples
#' bash_head()

bash_head <- function(f1, nrows){
  system(
    glue("head -n {nrows} {f1}")
  )
}

#' A Function
#'
#' Finds the sum of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the sum of.
#' @export
#' @examples
#' bash_col_sum()

bash_col_sum <- function(f1, col){
  col_num <-  which(names(f1) == col)
  system(
    glue("awk -F"," '{x+=${col_num}}END{print x}' {f1}")
  )
}

#' A Function
#'
#' Finds the sum of a given column.
#' @param f1 The name/path of the file.
#' @param col The name of the column to find the mean of.
#' @export
#' @examples
#' bash_col_mean()

bash_col_mean <- function(f1, col){
  col_num <-  which(names(f1) == col)
  system(
    glue("awk '{ sum += ${col_num} } END { if (NR > 0) print sum / NR }' {f1}")
  )
}

#' A Function
#'
#' Sorting
#' @param f1 The name/path of the file.
#' @param col_sort The name of the column to find the mean of.
#' @export
#' @examples
#' bash_arrange()

bash_arrange <- function(f1, col_sort){

  #Sorting a CSV file by the second column alphabetically

  col_num <-  which(names(f1) == col_sort)

  system(
    glue("sort -t, -k{col_num} {f1}")
  )

  # Numerically
  #sort -t, -k2n filename.csv
  # Reverse order
  #sort -t, -k2nr filename.csv
}

#' A Function
#'
#' This function allows you to express your love of cats.
#' @param f1 The name/path of the file.
#' @param cols_to_remove Names of the columns to removes.
#' @param f_new Optional name for new file if the results should be saved.
#' @export
#' @examples
#' bash_select()

bash_select <- function(f1, cols_to_remove, f2 = NA){

  # which means "select columns 2 through 5 and columns 8, using comma as the separator".
  #cut -f 2-5,8 -d , values.csv

  #Remove columns

  col_nums <-  which(names(f1) == cols_to_remove)
  cols_to_remove_nums <- paste0(col_nums, collapse = ',')


  if(is.na(f2)){
    system(
      glue("cut -d, -f {cols_to_remove_nums} {f1}")
    )

  } else{
    system(
      glue("cut -d, -f {cols_to_remove_nums} {f1} > {f2}")
    )
  }

}

#' A Function
#'
#' Join two files.
#' @param f1 The name/path of the first file.
#' @param col1 Column to use for joining from the first file.
#' @param f2 The name/path of the second file.
#' @param col2 Column to use for joining from the second file.
#' @param f_new Nme for new file.
#' @export
#' @examples
#' bash_join()

bash_join <- function(f1, col1, f2, col2, f_new){

  # Join the first file (-1) by the second column and the second file (-2) by the first

  col_num1 <-  which(names(f1) == col1)
  col_num2 <-  which(names(f2) == col2)

  system(
    glue("join -t, -1 2 -2 1 {f1} {f2} > {f_new}")
  )
}

#' A Function
#'
#' Join two files.
#' @param f1 The name/path of the first file.
#' @param col1 Column to use for joining from the first file.
#' @param f2 The name/path of the second file.
#' @param col2 Column to use for joining from the second file.
#' @param f_new Nme for new file.
#' @export
#' @examples
#' bash_join()

remove_col_dups <- function(df){
    return(df[!duplicated(names(df), fromLast=TRUE)])
}

get_model_diagnostics <- function(mdl){
    return(inner_join(broom::tidy(mdl), tidy(confint(mdl)), by = c("term" = ".rownames")))
}

calc_accuracy <- function(labels, preds){

    results <- table(labels, preds) %>%
        data.frame %>%
        mutate(correct = ifelse(labels==preds, 1, 0)) %>%
        group_by(correct) %>%
        summarise(freq = sum(Freq)) %>%
        ungroup() %>%
        mutate(prop = freq/sum(freq)) %>%
        filter(correct == 1)

    return(results)
}

accuracy_mine <- function(mdl, threshold, df){
    return(
        round(
            mean(as.integer(predict(mdl, data = df %>% select(-target), type = "response") >= threshold) == df$target),
            2)
    )
}

model_glm <- function(df) {
    glm(y ~ ., data = df, family = binomial(link="logit"))
}

read_csv_sample <- function(fpath, nrows, seed = 8, header = "-r"){

    sample <- system(glue("subsample {header} -s {seed} -n {nrows} {fpath}"), intern = T)

    # Convert the character vector into a string
    sample_cleaned <- paste(sample, collapse = '\n')

    df <- read_csv(sample_cleaned)

    return(df)

}
