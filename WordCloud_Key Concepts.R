setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

Packages <- c("dplyr", "ggplot2", "readr",
              "pdftools","stopwords","tidytext",
              "stringi", "stringr", "scales",
              "tidyr", "widyr", "ggraph", "igraph",
              "quanteda", "topicmodels","lattice",
              "robustbase", "cvTools", "NLP", "tm",
              "readxl", "htmltools","xlsx","SnowballC", 
              "RColorBrewer", "ggthemes",
              "extrafont","readr","wordcloud",
              "wordcloud2", "ggwordcloud","reshape2")

lapply(Packages, library, character.only = TRUE)

#Upload data
data <- file("data.txt")
data <- readLines(data)
data<- paste(data,collapse = " ")
data <- strsplit(data, split = " ") %>% unlist()
dataTable <- table(data) %>% as.data.frame()

#Clean data
data <- sapply(data,"removePunctuation",USE.NAMES = FALSE) 
data <- sapply(data,"tolower",USE.NAMES = FALSE) 
data <- sapply(data,"stripWhitespace",USE.NAMES = FALSE) 
data <- sapply(data,"removeNumbers",USE.NAMES = FALSE) 
data <- data[data!=""]
data <- data[data!=" "]

remove <- c(stopwords("eng"))
data <- sapply(data,"removeWords",words=remove, USE.NAMES = FALSE)
data <- data[data!=""]
data <- data[data!=" "]

dataTable <- as.data.frame(line = 1:885, data)

#Count data
count_table <- dataTable %>%
  dplyr::count(data, sort = TRUE)

#Save and export data
write.xlsx(count_table,"data.xlsx")
finaltable <- read_excel("data_final.xlsx")

# Extract an sample
sample <- finaltable %>% filter(n>4)

#Visualization of WordCloud
p<- ggplot(sample, aes(label = data, size = n)) +
  geom_text_wordcloud(area_corr = TRUE, color= '#78b1e0',
                      eccentricity = 1.3) +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(text = element_text(family="Segoe UI"),
        plot.title = element_text(hjust = 0.5, color = "#78b1e0", size = 16, face= "bold"),
        plot.subtitle = element_text(hjust = 0.5, color = "white", size = 10,face= "bold"),
        plot.caption = element_text(hjust = 0, color = "white", size = 7,face= "bold"),
        plot.background = element_rect(fill = "#292927")) +
  labs(title = "Wordcloud of Key Concepts",
       subtitle = "Chapter 2: Private Actors",
       caption = "Data source: Chapter 2")

