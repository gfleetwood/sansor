#' @title Counts Rows Of On Disk File
#' @description A command line version of nrow:
#' https://gist.github.com/stevenworthington/3178163
#' ipak(c("ggplot2", "plyr", "reshape2"))
#' @param f1 Name/path of the file
#' @export

ipak <- function(pkg){

    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

    if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)

    sapply(pkg, library, character.only = TRUE)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

partial_recursive <- function(func, args){

    if(length(args) == 1) return(func(as.formula(args[1])))

    func <- partial_recursive(
        partial(func, as.formula(head(args, 1))),
        tail(args, -1))

    return(func)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

check_key <- function(dict, key){

    result <- ifelse(key %in% names(fromJSON(dict)), fromJSON(dict)[key], NA)

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

drop_anomalous_cols <- function(df){

    anomalous_cols <- df %>%
        xray::anomalies() %>%
        purrr::pluck("problem_variables", "Variable")

    result <- select(df, -one_of(anomalous_cols))

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

train_test_split <- function(df){

    index <- caret::createDataPartition(df$target, p = 0.8, list = FALSE)
    train <- slice(df, index)
    test <- slice(df, -index)

    return(list(train = train, test = test))

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

drop_na_col_some <- function(df, frac = 0.1, method = "all"){

    result <- discard(df, ~ mean(is.na(.x)) > frac)
    result <- discard(df, ~ all(is.na(.x)))
    result <- discard(df, ~ sum(is.na(.x)) > 0)

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

model_calibration <- function(df){

    result <- df %>%
        ggplot(aes(fitted_probability, binary_class_indicator)) +
        geom_smooth()

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

heatmap <- function(df){

    result <- df %>%
        select_if(is.numeric) %>%
        cor() %>%
        corrplot(method = "circle", is.corr = FALSE)

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export
#'
lm_check <- function(mdl){

    result <- list(
        car::avPlots(mdl),
        car::influencePlot(mdl), # Outlier Analysis
        car::vif(mdl), # multi-colinearity
        car::durbinWatsonTest(mdl), # Autocorrelated Errors
        lmtest::bptest(mdl)
        ) # heteroskedasticity of errors

    return(result)

}

#' @title Recursive Function Builder
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

dfs_equal <- function(df1, df2){

    vec_compare <- function(df, col_loc) {

        result <- df %>%
            pull(col_loc) %>%
            keep(~ !is.na(.x))

        return(result)

    }

    stopifnot(ncol(df1) ==  ncol(df2))
    cols <- seq(1, ncol(df1))

    result <- cols %>%
        map_lgl(
            ~ all(vec_compare(df1, .x) == vec_compare(df2, .x))
            ) %>%
        all()

    return(result)

}

#' @title Imputation Indicator Columns
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

update_label_nas <- function(df){

    cols_with_na <- df %>%
        keep(~ sum(is.na(.)) > 0) %>%
        names()

    result <- reduce(cols_with_na, ~ update_label_na(.x, .y), .init = df)

    return(result)

}

#' @title Imputation Indicator Columns
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

update_label_na <- function(df, col_with_na){

    col_new <- paste(col_with_na, "is_na", sep = "_")
    result <- mutate(
        df,
        {{col_new}} := ifelse(is.na((!!sym(col_with_na))), 1, 0)
        )

    return(result)

}

#' @title Imputation Indicator Columns
#' @description Recursively creates partial functions to intake each
#' formula except the last. For the last formula evaluation takes place
#' @param f1 Name/path of the file
#' @export

date_decomposition <- function(df){

    result <- mutate(df,
        day = day(ts),
        month =month(ts),
        year = year(ts))

    return(result)

}

#' @title NULL to NA Conversion
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

na_conversion <- function(df){

    result <- df %>%
        map_df(~ ifelse(.x == "#NULL!", NA, .x)) %>%
        map_df(~ ifelse(.x == "", NA, .x))

    return(df)

}


#' @title Unpack List Col As String
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

list_col_to_str <- function(df, col){

   result <- mutate(df, !!sym(col) := map_chr(!!sym(col), ~ paste(.x, collapse = "-")))

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

cols_missing_data <- function(df){

    df_smry <- skimr::skim_to_wide(df)

    cols_missing_vals <- df_smry %>%
        mutate(missing_ratio = as.integer(missing)/as.integer(n)) %>%
        filter(missing_ratio >= .8) %>%
        pull(variable)

    return(result)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

pc_algo <- function(df){

    result <- df %>%
        bnlearn::pc.stable() %>%
        pull(arcs) %>%
        data.frame()

    return(result)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

# Given a two sided formula of the form "a ~ b" the user can choose to recover "a" or "b"
get_node <- function(formula_str, loc){

    result <- unlist(str_split(formula_str, " ~ "))[[loc]]

    return(result)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

# Causal Discovery To DAG: Recursively creates partial functions to intake each formula except the last

causal_disc_to_daggity <- function(func, args){

    if(length(args) == 1) return(func(as.formula(args[1])))

    func <- causal_disc_to_dag(partial(func, as.formula(head(args, 1))), tail(args, -1))

    return(func)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

# Extracts child-parent relationships from object created with the pc algorithm

get_dag_formulas <- function(pc_gauss){

    dag_formulas <- as(pc_gauss, "matrix") %>%
        data.frame() %>%
        tibble::rownames_to_column(var = "child") %>%
        tidyr::pivot_longer(-child, names_to = "parent", values_to = "parent_to_child_edge") %>%
        filter(parent_to_child_edge == 1) %>%
        by_row(~ paste(.x$child, .x$parent, sep = " ~ "), .collate = "col") %>%
        pull(.out)

    return(dag_formulas)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

# List all direct causal effects that are identifiable via backdoor adjustment
# The causal effect of parent on child is identifiable controlling for adjustment set

get_df_adjustment_sets <- function(dag){

    df_parent <- data.frame(parent = names(dag)) %>%
        mutate(parent = as.character(parent))

    df_parent_child = df_parent %>%
        by_row(~ get_children(dag, .x$parent), .collate = "rows") %>%
        #select(-.row) %>%
        rename(child = .out) %>%
        filter(child != "none")

    df_adjustment_sets = df_parent_child %>%
        by_row(~ get_adjustment_sets(dag, .x$parent, .x$child), .collate = "list") %>%
        rename(adjustment_sets = .out)

    return(df_adjustment_sets)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

get_children <- function(dag, parent){

    result <- children(dag, parent)

    if(length(result) < 1){
        result <- "none"
    }

    return(result)

}

#' @title Identify Columns With A Percentage Missing
#' @description Function to convert GSheets #NULL values and
#' empty strings to NAs.
#' @param df A dataframe
#' @return A dataframe with NAs instead of #NULLs and empty strings.
#' @export

get_adjustment_sets <- function(dag, parent, child, effect = "direct"){

    result <- adjustmentSets(x = dag, exposure = parent, outcome = child, effect = effect)

    if(length(result) < 1){
        result <- "none"
    }

    return(result)

}
