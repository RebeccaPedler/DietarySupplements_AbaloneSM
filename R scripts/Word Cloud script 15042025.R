# Install and load necessary packages
install.packages("wordcloud")  # Install if not already installed
install.packages("wordcloud2") 
install.packages("RColorBrewer")  # Install for color palette
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

# Define words and their frequencies
words <- c("abalone", "haliotis", "growth", "discus", "dietary", "hannai", "immunity", "laevigata", "effects", "immune", 
           "extract", "vibrio", "survival", "response", "vitamin", "temperature", "resistance", "supplementation", "water", 
           "ino", "performance", "iris", "probiotic", "disease", "aquaculture", "juvenile", "feed", 
           "nutrition", "Ulva", "probiotics", "diet", "Zealand", "antioxidation", "antioxidant", "seed", "midae", "acid", 
           "grape", "shell", "Undaria", "pinnatifida", "alginolyticus", "diets", "product", "high", "new", "substitution", 
           "meal", "challenge", "Shewanella", "alginate", "exposure", "composition", "feeding", "Taiwan", "macroalgae", "bacterial", 
           "Australian", "air", "capacity", "levels", "bacteria", "fed", "hybrid", "improve", "olleyana", "mannan", 
           "colwelliana", "tea", "pigment", "quality", "mollusc", "role", "cultured", "enzymes", "glucan", "stressor", 
           "proteins", "oxidative", "black", "single", "supertexta", "administration", "hydrolysed", "frequency", "sy9", "fatty", 
           "ascorbic", "diversicolor", "stablility", "L-proline", "sodium", "reeve", "methanotrophic", "dried", "farm", "protease", "bacillus", 
"green", "nitrogen", "pigmentation", "survivability", "tissue", "astaxanthin", "oligosaccharide", "beta13", "stimulants", "K1", "yeast", "parahaemolyticus", "flesh",
"infection", "against", "algae", "cell", "pyridoxine", "rufescens", "innate", "leaf", "Pacific", "responses", "small", "onoin", "donovan", "gracilaria", "oxygen", "linchformis", 
"effect", "metabolism", "apoptosis", "antioxidative", "herbs", "footed", "encapsulated", "peanut", "chromium", "passive", "carrot" )

freqs <- c(49, 44, 22, 20, 16, 15, 14, 13, 13, 12, 
           10, 9, 8, 8, 7, 7, 7, 6, 6, 6, 
           6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 
           5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 
           4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
           4, 4, 4, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
           3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

categories <- c("Population", "Population", "Outcome", "Population", "Intervention", "Population", "Outcome", "Population", "Outcome", "Outcome", "Intervention", "Context", "Outcome", "Outcome", "Intervention", "Intervention", "Context", "Outcome", "Context", "Context", "Intervention", "Intervention", "Population", "Intervention", "Population", "Context", "Intervention", "Intervention", "Intervention", "Population", "Intervention", "Outcome", "Population", "Intervention", "Intervention", "Context", "Intervention", "Intervention", "Intervention", "Outcome", "Outcome", "Intervention", "Population", "Intervention", "Context", "Outcome", "Intervention", "Intervention", "Intervention", "Context", "Population", "Intervention", "Context", "Outcome", "Intervention", "Intervention", "Population", "Intervention", "Outcome", "Context", "Outcome", "Population", "Intervention", "Population", "Population", "Intervention", "Outcome", "Population", "Intervention", "Context", "Context", "Intervention", "Intervention", "Outcome", "Outcome", "Intervention", "Intervention", "Intervention", "Intervention", "Outcome", "Outcome", "Outcome", "Outcome", "Outcome", "Intervention", "Intervention", "Intervention", "Intervention", "Intervention", "Outcome", "Outcome", "Outcome", "Outcome", "Outcome", "Intervention", "Intervention", "Intervention", "Intervention", "Intervention", "Intervention", "Outcome", "Context", "Outcome")

# Assign colors based on category
category_colors <- c("Population" = "red", "Outcome" = "black", "Intervention" = "blue")
word_colors <- sapply(categories, function(cat) category_colors[cat])

# Create a data frame
word_data <- data.frame(word = words, freq = freqs, color = word_colors, stringsAsFactors = FALSE)

# Generate the word cloud
wordcloud2(word_data, color = word_data$color)

# Generate the word cloud
set.seed(1234)  # For reproducibility
wordcloud(words, freqs, scale=c(4, 0.5), min.freq=1, max.words=100, 
          colors=brewer.pal(8, "Dark2"), random.order=FALSE, rot.per=0.35)
R.version.string
