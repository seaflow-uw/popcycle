#!/usr/bin/env Rscript
# Script to visually compare refiltering/regating results.
library(popcycle)
library(tidyverse)
library(grid)
library(gridExtra)

match_dbs <- function(olddir, newdir) {
    old_dbs <- list.files(olddir, full.names=T, recursive=T, pattern=".*\\.db$")
    new_dbs <- list.files(newdir, full.names=T, recursive=T, pattern=".*\\.db$")
    #new_dbs <- list.files("newcode-oldparams", full.names=T, recursive=T, pattern=".*\\.db$")

    # Find the cruises that were re-analyzed
    old_results <- stringr::str_match(old_dbs, ".*/([^/]+)\\.db$")
    new_results <- stringr::str_match(new_dbs, ".*/([^/]+)\\.db$")
    colnames(old_results) <- c("db", "cruise")
    colnames(new_results) <- c("db", "cruise")
    old_results <- tibble::as_tibble(old_results)
    new_results <- tibble::as_tibble(new_results)

    common <- dplyr::inner_join(old_results, new_results, by="cruise", suffix=c(".old", ".new"))
    return(common)
}

get_opp_counts <- function(row) {
    old_opp <- popcycle::get.opp.table(row$db.old)
    old_opp <- old_opp[old_opp$quantile == 50, c("date", "opp_count")]
    old_opp$name = "old"

    new_opp <- popcycle::get.opp.table(row$db.new)
    new_opp <- new_opp[new_opp$quantile == 50, c("date", "opp_count")]
    new_opp$name = "new"

    res <- rbind(old_opp, new_opp)
    res$dt <- lubridate::ymd_hms(res$date)
    res$cruise <- row$cruise

    # date, opp_count, name, dt, cruise
    return(tibble::as_tibble(res))
}

get_pop_counts <- function(row) {
    old_vct <- popcycle::get.vct.table(row$db.old)
    old_vct <- old_vct[old_vct$quantile == 50, c("date", "count", "pop", "gating_id")]
    old_vct$name = "old"

    new_vct <- popcycle::get.vct.table(row$db.new)
    new_vct <- new_vct[new_vct$quantile == 50, c("date", "count", "pop", "gating_id")]
    new_vct$name = "new"
    
    res <- rbind(old_vct, new_vct)
    res$dt <- lubridate::ymd_hms(res$date)
    res$cruise <- row$cruise

    # date, count, pop, gating_id, name, dt, cruise
    return(tibble::as_tibble(res))
}

usage <- "usage: validate-reanalysis.R olddir newdir plotdir

- olddir should contain old popcycle databases.
- newdir should contain new popcycle databases with updated filtering and
  classification results. DB files will be be matched with those in dirA by
  cruise name, assuming DB files are named as <cruise.db>.
- plotdir will contain PNG files comparing new and old OPP and VCT results.
"

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 3) {
  stop(usage, call.=FALSE)
} else {
  if (! is.na(file.info(args[1])$isdir)) {
    olddir <- args[1]
  } else {
    stop(paste0("Error: argument ", args[1], " is not a directory"), call.=FALSE)
  }
  if (! is.na(file.info(args[2])$isdir)) {
    newdir <- args[2]
  } else {
    stop(paste0("Error: argument ", args[2], " is not a directory"), call.=FALSE)
  }
  plotdir <- args[3]
  dir.create(plotdir, showWarnings=FALSE, recursive=TRUE)
}

common <- match_dbs(olddir, newdir)
options(tibble.print_max = Inf)
print(common)

print("creating opp plots")
oppplotdir <- file.path(plotdir, "opp-plots")
dir.create(oppplotdir, showWarnings=FALSE, recursive=TRUE)
for (i in rownames(common)) {
    filename <- file.path(oppplotdir, sprintf("%s.png", common[i, ]$cruise))
    png(filename, width=1280, height=1024, res=100)

    # date, opp_count, name, dt, cruise
    counts <- get_opp_counts(common[i, ])
    counts1 <- counts[counts$name == "old", c("dt", "opp_count")]
    counts2 <- counts[counts$name == "new", c("dt", "opp_count")]
    # Set zero count to NA to prevent log errors
    counts1[counts1$opp_count == 0, "opp_count"] <- NA
    counts2[counts2$opp_count == 0, "opp_count"] <- NA
    counts_common <- dplyr::full_join(counts1, counts2, by="dt", suffix=c(".old", ".new"))
    counts_common$diff <- counts_common$opp_count.old - counts_common$opp_count.new

    plot1 <- ggplot(counts1, aes(x=dt, y=opp_count)) +
        geom_line(alpha=0.5, color="red") +
        scale_y_log10() +
        labs(title=paste("old n =", nrow(counts1)), x="time", y="opp count")

    plot2 <- ggplot(counts2, aes(x=dt, y=opp_count)) +
        geom_line(alpha=0.5, color="blue") +
        scale_y_log10() +
        labs(title=paste("new n =", nrow(counts2)), x="time", y="opp count")

    plotdiff <- ggplot(counts_common, mapping=aes(x=dt, y=diff)) +
        geom_point() +
        labs(title=paste("count diffs NA =", sum(is.na(counts_common$diff))), x="time", y="old - new")

    grid.arrange(plot1, plot2, plotdiff, ncol=3, top=textGrob(common[i, ]$cruise))
    dev.off()
}
warnings()

print("creating vct plots")
vctplotdir <- file.path(plotdir, "vct-plots")
dir.create(vctplotdir, showWarnings=FALSE, recursive=TRUE)
for (i in rownames(common)) {
    filename <- file.path(vctplotdir, sprintf("%s.png", common[i, ]$cruise))
    png(filename, width=1280, height=1024, res=100)

    # date, count, pop, gating_id, name, dt, cruise
    counts <- get_pop_counts(common[i, ])
    counts1 <- counts[counts$name == "old", ]
    counts2 <- counts[counts$name == "new", ]
    # Set zero count to NA to prevent log errors
    counts1[counts1$count == 0, "count"] <- NA
    counts2[counts2$count == 0, "count"] <- NA
    counts_common <- dplyr::full_join(counts1, counts2, by=c("dt", "pop"), suffix=c(".old", ".new"))
    counts_common$diff <- counts_common$count.old - counts_common$count.new

    plot1 <- ggplot(counts1, aes(x=dt, y=count, color=pop)) +
        geom_line(alpha=0.5) +
        scale_y_log10() +
        labs(title=paste("old n =", nrow(counts1)), x="time", y="pop count")

    plot2 <- ggplot(counts2, aes(x=dt, y=count, color=pop)) +
        geom_line(alpha=0.5) +
        scale_y_log10() +
        labs(title=paste("new n =", nrow(counts2)), x="time", y="pop count")

    plotdiff <- ggplot(counts_common, mapping=aes(x=dt, y=diff)) +
        geom_point() +
        labs(title=paste("count diffs, NA =", sum(is.na(counts_common$diff))), x="time", y="old - new")

    grid.arrange(plot1, plot2, plotdiff, ncol=3, top=textGrob(common[i, ]$cruise))
    dev.off()
}
warnings()