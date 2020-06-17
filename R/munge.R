# assemble data for BAIT web app
library(tidyverse)

#input all tables of longitudinal age associations into a list
myfiles <- list.files(path = "data_raw/tables_age/femur", pattern = ".csv", full.names = TRUE)

myround <- function(x, digits){
  if_else(x > 1,
          round(x, digits),
          signif(x, digits))
}

table_reader <- function(x){
  table1 <- read_csv(x) 
  names(table1)[1] <- "trait"
  drop_cols <- c("ageSD", "beta_percDiff_allVsMult", "tStat_percDiff_allVsMult", "PvalCohort", "tStat", "tStat_males", "tStat_females")
  table1 <- table1 %>%
    select(-any_of(drop_cols)) 
  if(str_detect(x, "controls")){
    table1 <- table1 %>%
      rename(N_total = Nunique, N_male_mice = Nunique_males, N_female_mice = Nunique_females)
  }
  if(str_detect(x, "controls", negate = TRUE)){
    table1 <- table1 %>%
      select(matches("trait") | matches("*manyCont")) 
  }
  table1 <- map_if(table1, is.numeric, myround, digits = 4) %>%
    as_tibble
  return(table1)
}

tables <- map(myfiles, table_reader)
listnames <- myfiles %>%
  str_replace_all("data_raw/tables_age/femur/", "") %>%
  str_replace_all(".csv", "")
names(tables) <- listnames
names(tables[["femur_HBX_both"]])
tables[["femur_HBX_both"]]

element_bindr <- function(mylist, dataset, intervention){
  #list_name <- paste0(dataset, "_", intervention)
  list_name_both <- paste0(dataset, "_", intervention, "_both")
  list_name_males <- paste0(dataset, "_", intervention, "_males")
  list_name_females <- paste0(dataset, "_", intervention, "_females")
  bind_rows("Both" = mylist[[list_name_both]], 
            "Males" = mylist[[list_name_males]],
            "Females" = mylist[[list_name_females]],
            .id = "sex")
}

tables[["femur_BS"]] <- element_bindr(tables, "femur", "BS")
tables[["femur_BS"]] %>%
  glimpse()

tables[["femur_BS"]] <- bind_rows("Both" = tables[["femur_BS_both"]], 
                                  "Males" = tables[["femur_BS_males"]],
                                  "Females" = tables[["femur_BS_females"]],
                                  .id = "sex")

write_rds(tables, path = "data/tables_age/femur/tables_age.rds")


#### Sample size calculation tables
#input all ss tables into a list
dataset <- "femur"
mypathIn <- paste0("data_raw/tables_power/", dataset)
mypathOut <- paste0("data/tables_power/", dataset, "/tables_ss.rds")
myfiles <- list.files(path = mypathIn, pattern = ".csv", full.names = TRUE)

table_reader <- function(x){
  table1 <- read_csv(x) 
  names(table1)[1] <- "effect_size"
  varname <- x %>%
    str_replace_all(mypathIn, "") %>%
    str_replace_all("/", "") %>%
    str_replace_all("_dat_mult_power.csv", "")
  table1 <- table1 %>%
    mutate(trait = varname)
  return(table1)
}

tables <- map(myfiles, table_reader)

listnames <- myfiles %>%
  str_replace_all(mypathIn, "") %>%
  str_replace_all("/", "") %>%
  str_replace_all("_dat_mult_power.csv", "")
names(tables) <- listnames
tables[["BV.TV"]] %>%
  glimpse()


write_rds(tables, path = mypathOut)



