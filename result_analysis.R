library(tidyverse)
result <- read_rds("hts_result.rds")
error <- result$fct_result[[1]]$
error2 <- result$fct_result[[2]]$error_all_level

result %>% select(.id,fct_result) %>% bind_rows()
