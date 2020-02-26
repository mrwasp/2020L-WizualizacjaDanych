library(ggplot2)
library(dplyr)

gd <- read.csv("./data/grants_small.csv")

stats::filter
dplyr::filter

# %>%  ctrl shift m
# <- alt -

# edit(gd)
# summary(gd)

# factorsy

x_c <- c("a", "b", "a")
x_f <- factor(x_c)
as.numeric(x_f)

gd <- read.csv("./data/grants_small.csv", stringsAsFactors = FALSE)
gd[["budget"]]
gd$budget

toupper(gd[["budget"]])
mutate(gd, budget_large = toupper(budget))

strsplit(gd[["budget"]], " ")

gsub(pattern = "Przyznana kwota: ", "", gd[["budget"]])

as.numeric(gsub(pattern = "[a-z: ]", "", gd[["budget"]], ignore.case = TRUE))

mutate(gd, budget_numeric = as.numeric(gsub(pattern = "[a-z: ]", "", budget, ignore.case = TRUE)))

gd[["institution"]]
pull(gd, institution)

# my solution - regex xd
gsub(pattern = ",.*", "", gd[["institution"]], ignore.case = TRUE)


lapply(strsplit(gd[["institution"]], ", "), first)

sapply(strsplit(gd[["institution"]], ", "), function(i) i[1])



mutate(gd, 
       budget_numeric = as.numeric(gsub(pattern = "[a-z: ]", "", budget, ignore.case = TRUE)),
       institution = sapply(strsplit(gd[["institution"]], ", "), function(i) i[1])) %>%
  group_by(institution) %>%
  summarise(mean_budget = mean(budget_numeric)) %>% 
  filter(institution %in% c("Politechnika Warszawska", 
                            "Politechnika Łódzka", 
                            "Akademia Górniczo-Hutnicza im. St.Staszica w Krakowie",
                            "Uniwersytet Wrocławski")) %>%
  ggplot(aes(x = institution, y = mean_budget)) + 
  geom_col() + 
  coord_flip()


