# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# list packages to use
pkgs <- c( "tidyverse", "dplyr", # data wrangling
           "ggplot2", "patchwork", # plotting
           "rcompanion" # calculate Spearman Rho
           )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# read the data
d.outs <- read.table( "20220904_dbs_op_pats.csv", sep = ",", header = T )
d.prds <- read.table( "Predictions_1.csv", sep = ",", header = T ) %>% rename( "id" = "VTA" )


# ---- data wrangling ----

# keep only patients with VATs and correlations to normative templates calculated (i.e., included in d.prds)
d.outs <- d.outs %>% slice( which( id %in% unique(d.prds$id) ) ) %>%
  # keep only variables of interest
  select( id, pre_ledd, r1_ledd, pre_mds_updrs_iii_off, r1_mds_updrs_iii_stim_on, r1_stimtest_stim_on_med,
          contains("_drs_ii"), contains("bdi") ) %>%
  # rename variables
  rename( "led_pre" = "pre_ledd", "led_post" = "r1_ledd",
          "drs_pre" = "pre_drs_ii", "drs_post" = "r1_drs_ii",
          "bdi_pre" = "pre_bdi_ii", "bdi_post" = "r1_bdi_ii",
          "updrs_iii_pre" = "pre_mds_updrs_iii_off", "updrs_iii_post" = "r1_mds_updrs_iii_stim_on",
          "stimtest_med" = "r1_stimtest_stim_on_med"
          ) %>%
  # compute gain scores
  mutate( drs_gain = drs_post - drs_pre, drs_perc = drs_gain / drs_pre,
          bdi_gain = bdi_post - bdi_pre, bdi_perc = bdi_gain / bdi_pre,
          updrs_iii_gain = updrs_iii_post - updrs_iii_pre, updrs_iii_perc = updrs_iii_gain / updrs_iii_pre,
          led_gain = led_post - led_pre, led_perc = led_gain / led_pre
          )

# pivot the independent variables' dataset wider so that it can be easily glued to outcomes' dataset
# and join the datasets together to create a full final data-4-analysis data frame ready
d0 <- d.prds %>%
  # recode the templates such that their names include variable to be predicted by their value
  mutate( template = case_when( Map == "Cog_Dec_Reich" ~ "drs",
                                Map == "Depression_Siddiqi" ~ "bdi",
                                Map == "PD_Horn_orig" ~ "updrs_iii" )
          ) %>%
  # pivot to a wider format
  pivot_wider( id_cols = id, names_from = template, values_from = c(R,R_category) ) %>%
  # join the outcome
  left_join( d.outs, by = "id" )

# loop through percentage scores and recode infinities to NAs (because they could not be calculated courtesy of dividing by zero)
for ( i in paste0(c("drs","bdi","updrs_iii"),"_perc") ) d0[[i]] <- ifelse( d0[[i]] == Inf, NA, d0[[i]] )


# ---- Spearman's Rho ----

# prepare a table
t1 <- data.frame( outcome = c("drs","bdi","updrs_iii"), gain = NA, perc = NA )

# loop through all outcomes and calculate Spearman' Rhos with bootstapped 95% CIs
for ( i in t1$outcome ) {
  for ( j in c("gain","perc") ) {
    t1[ t1$outcome==i, j ] <- paste0(
      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$rho %>% round(3) %>% sprintf("%.3f",.), " [",
      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$lower.ci %>% round(3) %>% sprintf("%.3f",.), " ,",
      spearmanRho( as.formula( paste0(i,"_",j,"~R_",i) ), data = d0[ complete.cases( d0[[paste(i,j,sep="_")]] ), ], method = "spearman", ci = T )$upper.ci %>% round(3) %>% sprintf("%.3f",.), "]"
    )
  }
}

# print table 1 as csv
write.table( t1 %>% rename( "percentage gain" = "perc"), "Tab 1 Spearman's Rho.csv", sep = ",", row.names = F )
