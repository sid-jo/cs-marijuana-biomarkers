make_calculations <- function(dataset, dataset_removedups, split, compound, 
                              start = start, stop = stop, tpt_use = tpt_use){
  ## remove NAs
  df <- dataset_removedups %>% 
    dplyr::select(treatment, compound, timepoint_use) %>%
    rename(compound = 2) %>%
    filter(complete.cases(.))
  if(nrow(df)>0){
    if(stop <= 0){
      output <- df %>% 
        summarise(TP = 0,
                  FN = 0,
                  FP = sum(compound >= split),
                  TN = sum(compound < split)) 
    }else{
      if(split == 0){
        output_pre <- df %>% 
          filter(tpt_use == "pre-smoking") %>%
          summarise(TP = 0,
                    FN = 0,
                    FP = sum(compound >= split),
                    TN = sum(compound < split)) 
        
        output <- df %>% 
          filter(tpt_use != "pre-smoking") %>%
          summarise(TP = sum(treatment != "Placebo" & compound > split),
                    FN = sum(treatment != "Placebo" & compound <= split),
                    FP = sum(treatment == "Placebo" & compound > split),
                    TN = sum(treatment == "Placebo" & compound < split))
        
        output <- output + output_pre
      }else{
        ## calculate values if pre-smoking
        output_pre <- df %>% 
          filter(tpt_use == "pre-smoking") %>%
          summarise(TP = 0,
                    FN = 0,
                    FP = sum(compound >= split),
                    TN = sum(compound < split)) 
        
        output <- df %>% 
          filter(tpt_use != "pre-smoking") %>%
          summarise(TP = sum(treatment != "Placebo" & compound >= split),
                    FN = sum(treatment != "Placebo" & compound < split),
                    FP = sum(treatment == "Placebo" & compound >= split),
                    TN = sum(treatment == "Placebo" & compound < split))
        
        output <- output + output_pre
      }
    }
  }
  # clean things up; make calculations on above values
  output <- output %>%
    mutate(detection_limit = split,
           compound = compound,
           time_start = start,
           time_stop = stop,
           time_window = tpt_use,
           NAs = nrow(dataset) - nrow(df),
           N = nrow(dataset_removedups),
           N_removed = nrow(dataset) - nrow(dataset_removedups),
           Sensitivity = (TP/(TP + FN)), 
           Specificity = (TN /(TN + FP)),
           PPV = (TP/(TP+FP)),
           NPV = (TN/(TN + FN)),
           Efficiency = ((TP + TN)/(TP + TN + FP + FN))*100
    )
  
  return(output)
}