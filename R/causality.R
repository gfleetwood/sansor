library(tidyverse)
library(purrrlyr)

library(dagitty)
library(ggdag)
library(pcalg)

library(causaleffect)
library(igraph)
library(latex2exp)

pc_alg <- bnlearn::pc.stable(df)
pc_alg %>% pull(arcs) %>% data.frame() %>% write_csv("pc-dag-edges.csv")

# Given a two sided formula of the form "a ~ b" the user can choose to recover "a" or "b"
get_node <- function(formula_str, loc){

    result <- unlist(str_split(formula_str, " ~ "))[[loc]]

    return(result)

}

# Causal Discovery To DAG: Recursively creates partial functions to intake each formula except the last

causal_disc_to_daggity <- function(func, args){

    if(length(args) == 1) return(func(as.formula(args[1])))

    func <- causal_disc_to_dag(partial(func, as.formula(head(args, 1))), tail(args, -1))

    return(func)

}

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


get_children <- function(dag, parent){

    result <- children(dag, parent)

    if(length(result) < 1){
        result <- "none"
    }

    return(result)

}

get_adjustment_sets <- function(dag, parent, child, effect = "direct"){

    result <- adjustmentSets(x = dag, exposure = parent, outcome = child, effect = effect)

    if(length(result) < 1){
        result <- "none"
    }

    return(result)

}
