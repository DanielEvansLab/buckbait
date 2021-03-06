# assemble data for BAIT web app
library(tidyverse)

#input all tables of longitudinal age associations into a list
myfiles <- list.files(path = "data_raw/tables_age/femur", pattern = ".csv", full.names = TRUE)
myfiles <- list.files(path = "data_raw/tables_age/kyphosis", pattern = ".csv", full.names = TRUE)
myfiles <- list.files(path = "data_raw/tables_age/metabolic", pattern = ".csv", full.names = TRUE)
myfiles <- list.files(path = "data_raw/tables_age/weight", pattern = ".csv", full.names = TRUE)

myrawpath <- "data_raw/tables_age/weight/"
datasets <- "weight"
interventions <- c("BS", "CQ", "HBX", "L")
myoutpath1 <- "data/tables_age/weight/tables_age.rds"

myround <- function(x, digits){
  if_else(x > 1,
          round(x, digits),
          signif(x, digits))
}

table_reader <- function(x){
  table1 <- read_csv(x) 
  names(table1)[1] <- "trait"
  drop_cols <- c("ageSD", "beta_percDiff_allVsMult", "tStat_percDiff_allVsMult", 
                 "PvalCohort", "tStat", "tStat_males", "tStat_females", "tStat_manyCont")
  table1 <- table1 %>%
    select(-any_of(drop_cols)) 
  if(str_detect(x, "femur")){
    drop_traits <- c("Av.Obj.Ar", "Av.Obj.ECDa", "Av.MMI.x.", "Av.MMI.y.", 
                     "Av.MMI.max.", "Av.MMI.min.", "Crd.X", "Crd.Y", "Crd.Z",
                     "i.S", "i.S.TS.per.", "Obj.N", "Tb.Th.pl.", "Tb.Sp.pl.",
                     "Tb.N.pl.", "Tb.Dm.rd.", "Tb.Sp.rd.", "Tb.N.rd.", "Tb.Pf")
    table1 <- table1 %>%
      filter(!(trait %in% drop_traits))
  }
  if(str_detect(x, "controls")){
    table1 <- table1 %>%
      rename(N_total = Nunique, N_male_mice = Nunique_males, N_female_mice = Nunique_females)
  }
  if(str_detect(x, "controls", negate = TRUE)){
    table1 <- table1 %>%
      select(matches("trait") | matches("*manyCont")) %>%
      rename(N_total = Nunique_manyCont, N_controls = Ncontrol_manyCont,
             N_treated = Ntreat_manyCont, beta = beta_manyCont, SE = SE_manyCont,
             Pval = Pval_manyCont, Pval_intSex = intSex_Pval_manyCont)
  }
  table1 <- table1 %>%
    map_if(is.numeric, myround, digits = 4) %>%
    as_tibble
  return(table1)
}

tables <- map(myfiles, table_reader)
listnames <- myfiles %>%
  str_replace_all(myrawpath, "") %>%
  str_replace_all(".csv", "")
names(tables) <- listnames

element_bindr <- function(mylist, dataset, intervention){
  list_name_both <- paste0(dataset, "_", intervention, "_both")
  list_name_males <- paste0(dataset, "_", intervention, "_males")
  list_name_females <- paste0(dataset, "_", intervention, "_females")
  bind_rows("Both" = mylist[[list_name_both]], 
            "Males" = mylist[[list_name_males]],
            "Females" = mylist[[list_name_females]],
            .id = "sex")
}

for(i in datasets){
  for(j in interventions){
    tables[[paste(i, j, sep = "_")]] <- element_bindr(tables, i, j)
  }
}

keep_elements <- paste(datasets, c("controls", interventions), sep = "_")
tables <- tables[keep_elements]

write_rds(tables, path = myoutpath1)

#### Survival analysis files
myfiles <- list.files(path = "data_raw/tables_age/survival", pattern = ".csv", full.names = TRUE)
myrawpath <- "data_raw/tables_age/survival/"
datasets <- "survival"
interventions <- c("BS", "CQ", "HBX", "L")
myoutpath1 <- "data/tables_age/survival/tables_age.rds"

myround <- function(x, digits){
  if_else(x > 1,
          round(x, digits),
          signif(x, digits))
}

table_reader_survival <- function(x){
  table1 <- read_csv(x) 
  names(table1)[1] <- "intervention"
  table1 <- table1 %>%
    map_if(is.numeric, myround, digits = 4) %>%
    as_tibble
  return(table1)
}

tables <- map(myfiles, table_reader_survival)
names(tables) <- c("both", "males", "females")

element_bindr <- function(mylist){
  list_name_both <- "both"
  list_name_males <- "males"
  list_name_females <- "females"
  bind_rows("Both" = mylist[[list_name_both]], 
            "Males" = mylist[[list_name_males]],
            "Females" = mylist[[list_name_females]],
            .id = "sex")
}

tables2 <- element_bindr(tables)
tables2 <- tables2 %>%
  add_row(sex = "Both", intervention = "controls")

write_rds(tables2, path = myoutpath1)



#### Sample size calculation tables
#input all ss tables into a list
dataset <- "metabolic"
mypathIn <- paste0("data_raw/tables_power/", dataset)
mypathOut <- paste0("data/tables_power/", dataset, "/tables_ss.rds")
myfiles <- list.files(path = mypathIn, pattern = ".csv", full.names = TRUE)

if(sum(str_detect(myfiles, "femur")) > 0){
  drop_traits <- c("Av.Obj.Ar", "Av.Obj.ECDa", "Av.MMI.x.", "Av.MMI.y.", 
                   "Av.MMI.max.", "Av.MMI.min.", "Crd.X", "Crd.Y", "Crd.Z",
                   "i.S", "i.S.TS.per.", "Obj.N", "Tb.Th.pl.", "Tb.Sp.pl.",
                   "Tb.N.pl.", "Tb.Dm.rd.", "Tb.Sp.rd.", "Tb.N.rd.", "Tb.Pf")
  myfiles <- myfiles[str_detect(myfiles, pattern = "Av.*", negate = TRUE)]
  myfiles <- myfiles[str_detect(myfiles, pattern = "Crd.*", negate = TRUE)]
  myfiles <- myfiles[str_detect(myfiles, pattern = "i.S.*", negate = TRUE)]
  myfiles <- myfiles[str_detect(myfiles, pattern = fixed("Obj.N"), negate = TRUE)]
  myfiles <- myfiles[str_detect(myfiles, pattern = "Tb.*", negate = TRUE)]
}

table_reader <- function(x){
  table1 <- read_csv(x) 
  names(table1)[1] <- "effect_size"
  varname <- x %>%
    str_replace_all(mypathIn, "") %>%
    str_replace_all("/", "") %>%
    str_replace_all("_dat_mult_power.csv", "") %>%
    str_replace_all("mean_power.csv", "") %>%
    str_replace_all("_power.csv", "")
  table1 <- table1 %>%
    mutate(trait = varname)
  return(table1)
}

tables <- map(myfiles, table_reader)

listnames <- myfiles %>%
  str_replace_all(mypathIn, "") %>%
  str_replace_all("/", "") %>%
  str_replace_all("_dat_mult_power.csv", "") %>%
  str_replace_all("mean_power.csv", "") %>%
  str_replace_all("_power.csv", "")
names(tables) <- listnames
tables[["BMD"]] %>%
  glimpse()


write_rds(tables, path = mypathOut)



