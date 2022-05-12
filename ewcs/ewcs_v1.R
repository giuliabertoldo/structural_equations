library(haven)
library(tidyverse)


df <- read_sav("data/ewcs_2015.sav")
df_it <-  df %>%
  filter(Country == 15,# Only Italy
         Q7 == 1)  %>% # Only employed
  select(Q61n,
         Q61d,
         Q61c,
         Q61i,
         Q61e,
         Q61h,
         Q61j,
         Q61k,
         Q61g,
         Q61l,
         Q61a,
         Q61b,
         Q87a,
         Q87b,
         Q87c,
         Q87d,
         Q87e,
         Q78a,
         Q78b,
         Q78c,
         Q78d,
         Q78e,
         Q78f,
         Q78g,
         Q78h,
         Q78i,
         Q78j,
         Q88)



df_sp <- df %>%
  filter(Country == 26,  # Only Spain
         Q7 == 1) %>%   # Only employed
  select(Q61n,
         Q61d,
         Q61c,
         Q61i,
         Q61e,
         Q61h,
         Q61j,
         Q61k,
         Q61g,
         Q61l,
         Q61a,
         Q61b,
         Q87a,
         Q87b,
         Q87c,
         Q87d,
         Q87e,
         Q78a,
         Q78b,
         Q78c,
         Q78d,
         Q78e,
         Q78f,
         Q78g,
         Q78h,
         Q78i,
         Q78j,
         Q88)


df_ge <- df %>%
  filter(Country == 11, # Only Germany
         Q7 == 1) %>%   # Only employed
  select(Q61n,
         Q61d,
         Q61c,
         Q61i,
         Q61e,
         Q61h,
         Q61j,
         Q61k,
         Q61g,
         Q61l,
         Q61a,
         Q61b,
         Q87a,
         Q87b,
         Q87c,
         Q87d,
         Q87e,
         Q78a,
         Q78b,
         Q78c,
         Q78d,
         Q78e,
         Q78f,
         Q78g,
         Q78h,
         Q78i,
         Q78j,
         Q88)

