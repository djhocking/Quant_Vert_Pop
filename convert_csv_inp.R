library(dplyr)
library(tidyr)

sal_long <- read.csv("Lab/Data/sal_robust_long.csv", header = TRUE, stringsAsFactors = FALSE)

counts <- sal_long %>% 
  group_by(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) %>% 
  group_size() 

sal <- distinct(sal_long)