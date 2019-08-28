######## ONLY RUN THIS IF YOU ARE ON UNIVERSITY COMPUTER ###############
########################################################################
dir.create("C:/Users/Public/Documents/Rlibs")
.libPaths("C:/Users/Public/Documents/Rlibs")
# change to "C:Users/YOUR_ACCOUNT_NAME/Documents/Rlibs" if you're not KASUTAJA

############ END ######################################################
#######################################################################

lapply(c("tidytext","tidyverse","stylo", "gridExtra", "ggrepel"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))


library(stylo)
library(tidyverse)
library(tidytext)
library(gridExtra)
library(ggrepel)


### list files with path from folder
files = list.files("news_data/", full.names = T)

### read csv files from files and combine tables together (in loop)

all_news = c()

for (i in files) {
   all_news = rbind(all_news, read_csv(i))
}

### check the dataset! BUT probably it will be bad idea to open it in view!
all_news

##### supplying political "leaning" for each media

## Left (L), Left-Center (LC), C (Center, Non-partisan), Right-Center (RC), Right (R), Far-Right (FR)

news_politics = all_news %>% 
  count(publication) %>% 
  mutate(bias = c("LC", "FR", "LC", "LC", "L", "R","LC",
                  "R", "RC", "LC", "LC", "C", "L", "L", "LC")) %>% 
  left_join(all_news) %>%
  mutate(name_id = paste(id, publication, bias, sep="_")) %>% # new name_id combining several variables
  select(-c(n, X1,date,month))

#check data
news_politics

# check new id column
news_politics %>% 
  select(name_id)



### count approx. length

news_politics = news_politics %>% 
  mutate(length = nchar(content)/5) %>% 
  filter(length > 1000) # filter everthing below 1000 "words"


########### TRAINING AND TEST SETS #########
############################################

news_train = news_politics %>%
  filter(bias %in% c("R", "L")) %>% #choose only R & L sources
  group_by(bias) %>% # group by the bias variable
  sample_n(500) %>% # when grouped, sample_n() will take equal amount of samples from each group (= from each political stance)
  mutate(set = "training") %>% # supply label to not lose info
  ungroup()


news_test = news_politics %>%
  filter(!bias %in% c("R", "L")) %>%
  group_by(bias) %>%
  sample_n(200) %>%
  mutate(set = "test") %>%
  ungroup()


####### Word frequency rank (1st, 2nd, etc.)

rank = news_train %>% 
  bind_rows(news_test) %>% #combine with test set for now
  unnest_tokens(input = content, output = word, token="words") %>% #unnest "content" column by words
  filter(is.na(as.numeric(word))) %>% #filter numericals out
  count(word,sort=T) %>% # count all occurences of words across set and arrange by frequency
  mutate(word_rank = row_number()) %>%
  filter(word_rank < 401) %>% # select MFWs that will be used later
  select(-n)




########## Document-Term Matrices

dtm_train = news_train %>%
  unnest_tokens(input = content, output = word, token="words") %>% # tokenize
  filter(is.na(as.numeric(word))) %>% # filter numericals
  count(word, name_id) %>% # count words within each document separately
  group_by(name_id) %>%
  mutate(n = n/sum(n)) %>% # get relative frequencies (% from the size). Because of "group_by" sum will only work for separate docs
  ungroup() %>%
  inner_join(rank, by="word") %>% # leave only words in MFW subset!
  select(-word_rank) %>%
  mutate(word = factor(word, levels = rank$word)) %>% # reorder future columns
  spread(key="word", value = "n", fill=0) # "spread" to "wide" table


dtm_test = news_test %>%
  unnest_tokens(input = content, output = word, token="words") %>%
  filter(is.na(as.numeric(word))) %>%
  count(word, name_id) %>%
  group_by(name_id) %>%
  mutate(n = n/sum(n)) %>%
  ungroup() %>%
  inner_join(rank, by="word") %>%
  select(-word_rank) %>%
  mutate(word = factor(word, levels = rank$word)) %>%
  spread(key="word", value = "n", fill=0)

# we will also need vector of document names in the proper (alphabetical) order, save for later
names_train = dtm_train %>%
  select(name_id)

names_test = dtm_test %>%
  select(name_id)

names_test



############### Scaling #########

dtm_train = dtm_train[,2:ncol(dtm_train)] %>% # take data without fist column (name_id)
  as.matrix() %>% # transform to matrix
  scale() %>% # scale
  as.data.frame() # transform back to dataframe

dtm_test = dtm_test[,2:ncol(dtm_test)] %>%
  as.matrix() %>%
  scale() %>%
  as.data.frame()

dtm_train[1:10,1:6]


######### names for classes

class_train = names_train %>%
  separate(name_id, c("id", "publication", "bias"), sep="_")

class_test = names_test %>%
  separate(name_id, c("id", "publication", "bias"), sep="_")


########### Supply model with neccessary things

svm_results = perform.svm(dtm_train, # training set
                          dtm_test,  # test set
                          classes.training.set = class_train$bias, # training classes (which classes will be predicted)
                          classes.test.set = class_test$bias, # test classes (which classes will be compared with predicted)
                          tune.parameters = FALSE,
                          svm.kernel = "linear", # standart SVM kernel is linear (hyperplane is a line)
                          svm.degree = 3, #for polynomial kernel
                          svm.coef0 = 0,  #for polynomial kernel
                          svm.cost = 1) 

#### "confusion" matrix from results (which classes were predicted as R & L)

attr(svm_results, "confusion_matrix")

#### visualisation of shares of R & L for different political biases

as_tibble(attr(svm_results, "confusion_matrix")) %>% # transform the confusion matrix to old good tidy table
  ggplot(aes(x=actual_classes, y=predicted_classes)) + # map aesthetics (data for the plot) 
  geom_point(aes(size=n/200,color=predicted_classes)) + # add points: the size depends on "n/200": percents of predictions, color by R or L
  scale_size_continuous(range = c(5, 20)) + # adjust point scale
  theme_classic() + # classics
  scale_color_manual(values=c("#666666", "#cc3300")) + # manually set colors (one for R, one for L)
  guides(color=F) # disabel legend for color


#### SPECIAL: left and right z-scores



names_train %>%
  bind_cols(as.tibble(dtm_train)) %>%
  gather(key="word",value = "z_score", 2:401) %>%  # opposite of spread! hello long table
  separate(name_id, c("id", "publication", "bias"), sep="_") %>%
  group_by(bias,word) %>%
  summarise(mean_z=mean(z_score)) %>%
  arrange(desc(mean_z)) %>%
  top_n(100) %>%
  ggplot(aes(x=bias,y=mean_z, color=bias)) + 
           geom_text_repel(aes(label=word, size=mean_z),segment.alpha = 0, force=0.5) +
  theme_classic() +
  scale_color_manual(values=c("#666666", "#cc3300")) + 
  guides(color=F, size=F)

