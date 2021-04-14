#Establish of the workplace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#Upload the packages used
Packages <- c("dplyr", "ggplot2", "readr",
              "pdftools","stopwords","tidytext",
              "stringi", "stringr", "scales",
              "tidyr", "widyr", "ggraph", "igraph",
              "quanteda", "topicmodels","lattice",
              "robustbase", "cvTools", "NLP", "tm",
              "readxl", "ggnet", "network", "sna",
              "visNetwork", "threejs", "networkD3",
              "ndtv", "htmltools","xlsx")
lapply(Packages, library, character.only = TRUE)

#Consolidation of the data
text1 <- file("data.txt")
text_lines1 <- readLines(text1)
text_lines1<- paste(text_lines1,collapse = " ")
text_lines1 <- strsplit(text_lines1, split = " ") %>% unlist()
part1 <- table(text_lines1) %>% as.data.frame()

#Cleaning of the data
text_lines1 <- sapply(text_lines1,"removePunctuation",USE.NAMES = FALSE) 
text_lines1 <- sapply(text_lines1,"tolower",USE.NAMES = FALSE) 
text_lines1 <- sapply(text_lines1,"stripWhitespace",USE.NAMES = FALSE) 
text_lines1 <- sapply(text_lines1,"removeNumbers",USE.NAMES = FALSE) 
text_lines1 <- text_lines1[text_lines1!=""]
text_lines1 <- text_lines1[text_lines1!=" "]

remove <- c(stopwords("eng"))
text_lines1 <- sapply(text_lines1,"removeWords",words=remove, USE.NAMES = FALSE)
text_lines1 <- text_lines1[text_lines1!=""]
text_lines1 <- text_lines1[text_lines1!=" "]

#Creation of a data frame (table) once the data was standardized
part1 <- as.data.frame(line = 1:8265, text_lines1)

#Word frequency analysis (creation of bar graph with the frequencies)
count_table <- part1 %>%
  dplyr::count(text_lines1, sort = TRUE)

count_table %>%
  head(20) %>%
  mutate(text_lines1 = reorder(text_lines1, n)) %>%
  ggplot(aes(text_lines1, n)) +
  geom_col(fill = "blue") +
  theme_gray()+
  theme(text = element_text(family="Segoe UI"),
        axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 10))+
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(
    x = " ",
    y = "Mentions",
    title = "Text Analysis",
    subtitle = "Word frequency of references used") +
  geom_text(aes(label = n, hjust = 1.2), 
            color = "white", fontface = 2)

# Prepare table for comparison for each part
backupdata1 <- cbind(Part1 = 4, count_table)

#Save table in xlsx format
write.xlsx(backupdata1,"backupdata4.xlsx")

#Creation of bigrams
bigrams1<-lapply(ngrams(text_lines1,2), paste, collapse=" ") %>% unlist()

bigrams1 <- table(bigrams1) %>% as.data.frame()
bigrams1 <- bigrams1 %>% separate(bigrams1,into=c("word1","word2"),sep=" ") 

#Extraction of the sample
samplefinal <- bigrams1 %>% filter(Freq>1)

write.xlsx(samplefinal,"samplefinal.xlsx")
samplefinal <- read_excel("samplefinal.xlsx") %>% as.data.frame()

#Implementation of the network graph
firstposition <- samplefinal$word1
secondposition <- samplefinal$word2

network <- data.frame(firstposition,secondposition, stringsAsFactors = FALSE)

# make a nodes data frame out of all unique nodes in networkData
nodes <- data.frame(name = unique(c(network$firstposition,
                                    network$secondposition)))

# make a group variable where nodes in networkData$src are identified
nodes$group <- ifelse(nodes$name %in% network$firstposition, "Stronger", "Weaker")

# make a links data frame using the indexes (0-based) of nodes in 'nodes'
links <- data.frame(source = match(network$firstposition, nodes$name) - 1,
                    target = match(network$secondposition, nodes$name) - 1)

#Network graph
network <-forceNetwork(Links = links, 
                       Nodes = nodes, 
                       Source = "source",
                       Target = "target", 
                       NodeID ="name", 
                       Group = "group",
                       opacity = 1, 
                       opacityNoHover = -1,
                       height = NULL, width = NULL,
                       colourScale = JS('d3.scaleOrdinal().domain(["Stronger","Weaker"]).range(["#81BEF7", "#81F781"]);'), 
                       fontSize = 15,
                       fontFamily = "serif", linkDistance = 30,
                       linkWidth = JS("function(d) { return Math.sqrt(d.value); }"),
                       radiusCalculation = JS(" Math.sqrt(d.nodesize)+6"), charge = -30,
                       linkColour = "#FBFBEF", zoom = TRUE, legend = TRUE,
                       arrows = FALSE, bounded = FALSE, clickAction = FALSE)

network <- htmlwidgets::prependContent(network, htmltools::tags$h1("Network of correlation between words"))
network <- htmlwidgets::prependContent(network, htmltools::tags$h2("To see the connected labels, please click on one of the nodes"))

#Adding style parameters
network <- htmlwidgets::onRender(
  network,
  'function(el, x) { 
    d3.selectAll(".legend text").style("fill", "white");
    d3.select("body").style("background-color", "black");
    d3.select("h1").style("color", "white").style("font-family", "sans-serif");
    d3.select("h2").style("color", "white").style("font-family", "sans-serif");
    d3.select("body")
      .style("background-image","url(file://C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/TUMProject/Part4/first.jpg)")
      .style("background-repeat", "no-repeat")
      .style("background-position", "right bottom");
  }'
)

customJS <- '
function(el,x) { 
    var link = d3.selectAll(".link")
    var node = d3.selectAll(".node")

    var options = { opacity: 1,
                    clickTextSize: 10,
                    opacityNoHover: 0.1,
                    radiusCalculation: "Math.sqrt(d.nodesize)+6"
                  }

    var unfocusDivisor = 4;

    var links = HTMLWidgets.dataframeToD3(x.links);
    var linkedByIndex = {};

    links.forEach(function(d) {
      linkedByIndex[d.source + "," + d.target] = 1;
      linkedByIndex[d.target + "," + d.source] = 1;
    });

    function neighboring(a, b) {
      return linkedByIndex[a.index + "," + b.index];
    }

    function nodeSize(d) {
            if(options.nodesize){
                    return eval(options.radiusCalculation);
            }else{
                    return 6}
    }

    function mouseover(d) {
      var unfocusDivisor = 4;

      link.transition().duration(200)
        .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });

      node.transition().duration(200)
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;});

      node.select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", 24 + "px ")
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
    }

    function mouseout() {
      node.style("opacity", +options.opacity);
      link.style("opacity", +options.opacity);

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      node.select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ")
        .style("opacity", 0);
    }

    d3.selectAll(".node").on("mouseover", mouseover).on("mouseout", mouseout);
}
'

network <- onRender(network, customJS)

#Export network graph (html format)
saveNetwork(network, "C:/Users/isaac/OneDrive/Escritorio/Trabajos_R/TUMProject/Network_Graph_Final.html", selfcontained = TRUE)


