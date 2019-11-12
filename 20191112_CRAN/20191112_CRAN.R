library("dplyr")
library("ggplot2")

# get rid of scientific notation
options(scipen=999)

# load data
cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

## remove entries without any lines of code
cran_code <- cran_code[cran_code$code != 0,]

# calculate ratio of comment to code
cran_code$ratio <- cran_code$comment/cran_code$code

#calculate number of packages per language
cran_code$language <- as.factor(cran_code$language)
summary <- as.data.frame(summary(cran_code$language))
names(summary)[names(summary) == "summary(cran_code$language)"] <- "count"

# merge number of packages with full dataset
summary$language <- row.names(summary)
cran_code_rank <- merge(cran_code, summary, by = "language", all = TRUE)

## remove all but top 30 languages
sort <- cran_code_rank[order(-cran_code_rank$count),]
unique <- sort[!duplicated(sort$language),]
top30 <- unique[c(1:30),]
minvalue <- min(top30$count)
top30_toplot <- sort[sort$count >= minvalue,]

# get rid of NA values
top30_toplot <- na.omit(top30_toplot)

# plot
g <- ggplot(top30_toplot, aes(x= reorder(language, -ratio, FUN = median), y = ratio, color = top30_toplot$ratio)) + geom_boxplot(show.legend= FALSE, alpha = 0.5) + geom_point(show.legend = FALSE) + scale_color_gradient2(high = "white", mid = "red", low = "red", midpoint = 0.0000001, trans = "pseudo_log") + ylim(0,5)

theme <- theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_dark()

final <- g + theme + ylab("Ratio: Commented Lines/Lines of Code") + xlab("Language")

plot(final)
