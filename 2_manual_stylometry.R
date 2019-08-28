######## ONLY RUN THIS IF YOU ARE ON UNIVERSITY COMPUTER ###############
########################################################################
dir.create("C:/Users/Public/Documents/Rlibs")
.libPaths("C:/Users/Public/Documents/Rlibs")
# change to "C:Users/YOUR_ACCOUNT_NAME/Documents/Rlibs" if you're not KASUTAJA

############ END ######################################################
#######################################################################

# check for libraries and download if needed
lapply(c("tidytext","tidyverse","ggdendro"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))

#load libraries
library(tidyverse)
library(tidytext)
library(ggdendro)



### Bad-looking manual function that counts delta distance (takes z-scored frequencies matrix with documents in row names as input)

burrows.delta = function(x, nmfw) {
  
  #cut by supplied number of MFW
  df = x[,1:nmfw]
  delta = c()
  dist.table = c()
  #nested loop: goes through matrix and counts delta for each pairs of vectors
  for (i in 1:nrow(df)) {
    for (j in 1:nrow(df)) {
      ## the mighty delta itself
      delta = c(delta, sum(abs(df[i,]-df[j,]))/nmfw)
    }
    ## bind each column (distance from text X to all other texts) together
    dist.table = cbind(dist.table, delta)
    delta = NULL
  }
  ## supply labels
  colnames(dist.table) <- rownames(x)
  rownames(dist.table) <- rownames(x)
  ##returns matrix as table of distances
  return(as.dist(dist.table))
  
}


############ DATA

### list files in the folder
files = list.files("corpus/", full.names = T)
files

### get dataframe of titles and texts as LONG strings
########## 

corpus = data_frame(title = files, 
                    text = sapply(files, read_file)) %>%
  mutate(title = str_replace(title, "corpus/(.*?).txt", "\\1")) 

# NB if you're on linux, use double // in "corpus//"

### count word freqs and sort by most frequent (it will be handy to know the order of MFW) 

rank = corpus %>% 
  #tokenize by word -> new column "word" out of column "text"
  unnest_tokens(input = text, output = word, token = "words") %>%
  #count & sort
  count(word, sort = T)



### tokenize corpus, count freqs in each document, compute relative frequencies, spread to matrix-like format

freqs = corpus %>%
  unnest_tokens(input = text, output = word, token = "words") %>%
  count(title, word) %>% # count words within each text
  group_by(title) %>%
  mutate(n = n/sum(n)) %>% # because of group_by() will sum word counts only within titles -> we get relative frequncies
  rename(text_title = title) %>%
  mutate(word = factor(word, levels = rank$word)) %>% #reorder words by their rank so in the long format they would appear 
  spread(key="word", value="n", fill = 0)

freqs[1:5,1:10]

#normalize with scale()
z_freqs = freqs %>%
  select(-text_title) %>% # remove title column
  as.matrix() %>% # transform to matrix
  scale() # z-scores

# supply document titles (they go in alphabetical order so we can take them from "corpus" variable)
rownames(z_freqs) = corpus$title 

z_freqs[1:5,1:10]

### now when we have z-scored Document-Term Matrix, we can proceed to calculate distances

### Classic delta (with my ugly function)
manual_delta = burrows.delta(z_freqs, 400)
### The same thing!
delta = dist(z_freqs[,1:400],method = "manhattan", diag=T,upper=T)/400



### cluster by Ward's method (minimize divergence between clusters)

clusters = delta %>%
  hclust(method = "ward.D2") %>%
  as.dendrogram()

### fast dendrogram with "ggdendro" package

ggdendrogram(clusters,rotate=T)

############ larger code to customize plot

## first prepare a ggdendro object
ggclust = dendro_data(clusters)

## then modify the table of labels within "ggclust$labels"
ggclust$labels
ggclust$labels = ggclust$labels %>%
  separate(label, c("author", "title", "year"), sep="_") %>%
  mutate(label = paste(author, title, year, sep="_"))

# plot: segments & labels as separate geometry
### Don't worry, its less intimidating than it looks! most of the code is for cleaning the background!
ggplot() + 
  geom_segment(data=ggclust$segments,aes(x,y,xend=xend, yend=yend),size=1) +
  geom_text(data=ggclust$labels, aes(x,y,label=label, color=author), hjust=1, angle=0, size=4) +
  coord_flip() + 
  scale_y_continuous(expand=c(1, 0)) + 
  theme_classic() +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.line.x=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x=element_text(size=10),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())
