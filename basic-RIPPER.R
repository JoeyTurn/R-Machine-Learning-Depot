#rules learning, decision tree (RIPPER)

mushroom <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter05/mushrooms.csv", stringsAsFactors = FALSE)
str(mushroom)
mushroom$veil_type <- NULL
table(mushroom$type)
#given we have ~8k types, we can assume this is essentially every mushroom type, so no need to train

#if already loaded,
#detach("package:RWeka", unload=TRUE) #RWeka's OneR is weird with (overrides) OneR package?? idk why that is

#if not loaded, and conflictRules not MaJ
conflictRules(pkg = 'RWeka', exclude = 'OneR')

library(OneR)
mushroom_1R <- OneR(type ~ ., data = mushroom)
mushroom_1R
#mushroom_classifier <- OneR(type ~ odor + cap_color, data = mushroom)
#mushroom_classifier #same since the only derived rules were for odor
summary(mushroom_1R)

library(RWeka)
cleaned_mushroom <- lapply(mushroom, as.factor)
mushroom_JRip <- JRip(type ~ ., data = cleaned_mushroom)
mushroom_JRip
