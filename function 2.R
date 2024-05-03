des_tab = functionon(data, filter_needed = FALSE, filter_column = NULL, fname){
  
  data_an = data %>%
    as.data.frame()
  
  if (filter_needed) {
    data_an <- data_an %>% select((!!sym(filter_column)), yscd)
  }
  
  out = data_an %>%
    tbl_summary(by = yscd, missing = "no") %>% 
    add_n() %>%
    bold_labels()%>%
    modify_table_body( ~ .x %>%
                         mutate(across(where(is.character),~ ifelse(. == "0 (NA%)", "-", .))))
  
  print = out %>% 
    as_gt() %>%             
    gt::gtsave(output_wd, filename = paste0(fname, ".docx"))
  
  return(out)
  
} 

des_tab_fil = function(data, filter_needed = FALSE, filter_column = NULL, filter_value = NULL, fname){
  
  data_an = data %>%
    as.data.frame()
  
  if (filter_needed) {
    data_an <- data_an %>% filter((!!sym(filter_column)) == filter_value)
  }
    
  out = data_an %>%
    select(-pathogen_type) %>% 
    tbl_summary(by = yscd, missing = "no") %>% 
    add_n() %>%
    bold_labels()%>%
    modify_table_body( ~ .x %>%
                         mutate(across(where(is.character),~ ifelse(. == "0 (NA%)", "-", .))))
  
  print = out %>% 
    as_gt() %>%             
    gt::gtsave(output_wd, filename = paste0(fname, ".docx"))
  
  return(out)
  
} 
library(bibliometrix)
