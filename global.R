library(data.table)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

library(synapseClient)
synapseLogin()

txt <-'<div id="progress" class="graph"><div id="bar" style="width:%s%%"><p>%s</p></div></div>'

tbl <- synTableQuery("select * from syn2767694")
df <- tbl@values %>% select(Host_Species, Originating_Lab, Cell_Type, 
                            Cell_Line_Type, Cell_Type_of_Origin,
                            Reprogramming_Vector_Type, Reprogramming_Gene_Combination)

totalFeats <- nrow(df)

df.melted <- melt(df, id.vars=NULL)

df.melted.count <- df.melted %>% 
  count(variable, value) %>% 
  mutate(percent=round((n/totalFeats) * 100),
         Count=sprintf(txt, as.character(100 - percent), as.character(n))) %>% 
  arrange(desc(n)) %>% 
  select(variable, value, Count)

dfs <- dlply(df.melted.count, .(variable), 
             function(x) {
               x %>% select(-variable)
             })

# df1 <- data.frame(Name=c('A', 'B', 'C', 'D', 'E'), 
#                  Count=unlist(lapply(c(1, 25, 50, 75, 100),
#                                    function(x) sprintf(txt, 
#                                                        as.character(100 - x), 
#                                                        as.character(x)))))
# 
# df2 <- data.frame(Name=c('F', 'G', 'H', 'I'), 
#                   Count=unlist(lapply(c(1, 1, 10, 50),
#                                     function(x) sprintf(txt, 
#                                                         as.character(100 - x), 
#                                                         as.character(x)))))
# 
# dfs <- list(Foo=df1, Bar=df2)
