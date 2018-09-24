# project:    mashable
# date:       03/09/2018
#set directory
setwd("C:/Users/s159655/Documents/JADS/Semester 1/Strategy and Business Models") #create a folder Text_mining (or other name). 
#Now create 3 subfolders: 'input', 'tmp', and 'output'
#always / (slash)


#package
#did you already install the package? #install.packages("data.table")
library(data.table) #as.data.table
library(stm) #stm
library(splitstackshape) #cSplit
library(stringr) #str_pad
library(quanteda) #dfm
library(plyr) #ddply
library(dplyr) #percent_rank
library(stopwords) #Stopwords
library(corrplot) #to plot correlation
library(wordcloud) #to plot wordcloud

#expanding the memory ram of the program.
#You may increase this value up to 2GB or the maximum amount of physical RAM you have installed. 
#You need to be using 64-bit in order to take real advantage of this.
memory.limit(size=1000000000000) #where the size is in MB. 

#functions 
#myhead_func helps when the dataset is to heavy, it displays just the first 200 characters of each variable.
#we have to input the name of the table, and the number of lines. 
myhead_func<-function(data,nb_lines){
  ln<-length(as.data.table(data))
  data<-data[1:nb_lines,]
  View(apply(data,2, function(x) substring(x,1,200)))
}

# %in% is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand.
# %in% it is provided by R
# %!in% it's not provided by R, so I create it bellow. it does the opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

#load data using format data.table
table<-as.data.table(readRDS(paste0(getwd(),"/mashable_database.rds")))

##################
#Data exploration#
##################
myhead_func(table,10) #better than View in cases of heavy data.tables
#display the name of the columns
colnames(table)

#next to each variable I will add DV (for dependent variable), IV for independent variable
#"url" (id)*                     "shares" (DV)                         "title" (IV / text)        
#"text" (IV / text)              "author" (IV/id / factor)             "date" (IV / date)
#"topics" (IV / factor)*         "channel" (IV / factor)               "content_type" ( KO )
#"post_lead_type" (IV / factor)* "geo" ( KO )                           "sourced_from" ( KO )


#additional variables. I will create this variables. 
#We need this variables to analyze the number of shares, or in some other cases to elaborate the topic modeling
# top_OK (scope)
#"my_id" (id)                    "top_partner" (IV / factor)            "nb_days" (IV / date ~ numeric)  
#"nb_char_text" (IV / #)         "nb_topics" (IV / factor)              "topics_matrix" (matrix / IV / factor)
#"first_author"* (IV / factor)   "post_lead_type2"
#"topic_modeling1" CTM (IV )     "top_series" ( IV / factor)
#"topic_modeling2 (matrix / IV / numeric)

#the topic modeling 1 is created only with the text using CTM (similar to LDA) We will learn this algorithms on next monday
#the topic modeling 2 is created with the text + covariates (CV, for example metadata) using STM (structural topic modeling)

#topic_modeling2 variables (what are the metadata variables) and in brackets the relationship
# not all the variables can be interesting
#"post_lead_type" ( CV )*        "first_author" (CV)                    "nb_days" (CV)
#"topics" ( - )                  "nb_topics" (CV / - )                  "top_partner" ( - )
#"title"  ( DV / - )             "shares" ( - / ? )                     "channel" (CV)
#"text" (DV)                     "top_series" ( CV )                    "topics_matrix" (CV)



# now we explore the data
table[,.N,geo]
#sourced_from, geo, content_type do not give any relevant information
table[,.N,sourced_from] 
table[,.N,content_type]

table[,.N,channel]
#we will remove those channel with very few observations

tmp<-table[,.N,author]
#we see in the tmp table that there are 806 authors, many of them published only one article, and very few published more than 100 articles

plot(density(table$shares))
boxplot(table$shares)
tmp<-table[,.N,shares]
summary(table$shares)
plot(density(nchar(table$shares)))
#some different ways to understand the distribution of the variable shares. Very few have more than 10K shares. Almost all have more than 100 shares ...

tmp<-table[,.N,date]
tmp<-table[,.N,paste0(substring(date,7,11),substring(date,1,2))]
#two years of data, we have observations for almost all the days in the year. 
#from 2013 to 2014

table[,.N,post_lead_type]
#we might create three groups: Article, Video, and Image. #longform is not an online news, thus we have to remove it
tmp<-table[,.N,topics]
tmp<-table[,.N,by=c("topics","author")] #author and topics!
#the topics are given by the authors. we might look for those general topics: technology, business, world, us... We must split the topics
tmp<-table[,.N,title]
tmp<-table[,.N,by=c("title","author")]
tmp<-table[,.N,by=c("title","author","date")][N>2]
#some authors do a series of articles related, having all the same title. 



###creation of variables
#top_OK
#we keep only channels with more than 300 observations
list_channels<-table[,.N,channel][order(-N)][N>300]$channel

#we want the post_lead_type that are not in the vector "longform, full width and NA. This three are not considered as online news
# this operators [] allows to filter the information inside. 
tmp<-unique(table$post_lead_type)
list_post_lead_type<-tmp[tmp%!in% c("Longform",NA,"Full Width")]

#we create top_OK. True: we keep the observation, FALSE: we remove the observation
table[,"top_OK":=(channel %in% list_channels &  post_lead_type %in% list_post_lead_type) &  is.na(text)==FALSE & nchar(text)>0]
table[,.N,top_OK] #33 961
length(table$url)
## we remove duplicates
setkey(table,"url") #defining the key variable of the data.table
table<-unique(table,by="url") #this works when the format of the table is data.table
length(table$url)
#in this case we didn't have any duplicate, but it was important to test!

######################
#additional variables#
######################
#we create a stronger identifier 
#my_id 
table[,"my_id":=str_pad(1:.N,5,pad="0")] ;length(table$my_id) #33 961

#top_partner
table[,"author":=tolower(author)]
table[,"top_partner":=grepl(" and ",author)|grepl(" & ",author)|grepl(", ",author)] #searching for couples of authors

# number of days
table[top_OK==TRUE,"date_min":=min(date)] #this might not work if there are NA in the date. solution might be min(date,na.rm=TRUE)
table[top_OK==TRUE,"nb_days":=as.numeric(as.character(difftime(as.Date(date,format='%m/%d/%Y'),as.Date(date_min,format='%m/%d/%Y'), units= c('days'))))]
#we need to transform the variable into a numeric variable. the new variable represents the variation from date and date_base

# number of characters in text
table[,"nb_char_text":=nchar(text)]
#how many characters has a text. It's important to analyze if all the texts are alike. 

# number of topics
table[,"nb_topics":=((nchar(topics)-nchar(gsub("[,]","",topics)))-1)] #we identify how many ',' are in the topic. So this gives the number of topics. 
#there are topics with the word 'uncategorized' so we subtract one. 

# topics_matrix
# lets to a matrix, only with the most important words
topics_Dfm <- dfm(table$topics, verbose = TRUE,ngram=1,tolower = TRUE
             ,remove=c("uncategorized",","))
#this constructs a sparse document-feature matrix from the text. feature= words + combination of words depending of the position of the word
#in the sentence "I love data science", 
#if we consider ngram=1 => we have 4 features: I, love, data, science
#if we consider ngram=2 => we have 7 features: I, love, data, science, I_love, love_data, data_science
#if we consider ngram=3 => we have 9 features: I, love, data, science, I_love, love_data, data_science, I_love_data, love_data_science
#it has options like tolower, remove words (it could be included a vector of stopwords), we can include dictionary, so all features are going to be matched with the equivalent value from a dictionary
#we want to have a DFM to count the number of times the word (feature) appears
#we don't include stopwords because we assume the authors don't include stopwords in the topics.
topfeatures(topics_Dfm,10)
#this function gives the top 10 of features. We see that stopwords are not included.
#the top10 are the most frequent features. 
topics_Dfm.2<-dfm_trim(topics_Dfm, min_termfreq = 1000); length(topics_Dfm.2@Dimnames$features)
# we trim the matrix: keeping only those with at least 1000 appereance. 
#min_termfreq and min_docfreq are not the same condition. The second one count the number of documents(observations) were the word appears at least one time
topics_Dfm.2<-dfm_trim(topics_Dfm, min_docfreq = 3396); length(topics_Dfm.2@Dimnames$features)
topics_Dfm.2<-dfm_trim(topics_Dfm, sparsity = 0.90); length(topics_Dfm.2@Dimnames$features) 
#we use sparsity, that has a easier criteria selection than min_docfreq.
#sparsity:p=0.90; min_docfreq=length(table$my_id)*(1-p) = 
tmp<-as.matrix(topics_Dfm.2)
colnames(tmp)
#we remove last variable because it is already represented by video
tmp<-tmp[,1:9]
#we want to have a matrix of 0 and 1, so everything that is greater than 1 is going to be equal to 1
tmp[tmp>1]<-1
#now we want to have a matrix of 0 and 1 where TRUE is equal to 1, and FALSE equal to 0
tmp<-tmp==1
#condition notations: equal -> '=='; greater than -> '>' greater or equal than -> '=>'
#we modify the name of the variables
colnames(tmp)<-paste0("topic_",str_pad(substring(colnames(tmp),1,3),3,pad="_"))
tmp<-as.data.table(tmp)
#now we match the matrix and the table
length(table$my_id)==length(tmp$topic_mob)
table<-cbind(table,tmp)

# in case we want to consider the specific words for a certain group of words, we might use the tf-idf measure
# topics_Dfm_tfidf <- dfm_tfidf(topics_Dfm)
# topfeatures(topics_Dfm_tfidf,10)
# using this measure with the variable topics does not make any sense because the words are used only one time per observation. 
# it make sense to use this measure with descriptions, or text of the article, privacy policy.. 

#keep first author
table[,"first_author":=gsub(", .*","",gsub(" & .*","",gsub(" and .*","",author)))]
#the '.*' means everything after or before something, depending of 
#post lead type 2
table[,"post_lead_type2":=ifelse(grepl("Video",post_lead_type),"Video",
                                 ifelse(grepl("Default",post_lead_type),"Article","Gallery"))]

#top series
tmp<-table[,.N,by=c("title","author")][N>2]
list_titles<-tmp$title

#there are many ways to create this variable. May you have a better way than mine?
ID_in<-which(table$title %in% list_titles)
table$top_series<-FALSE
table[ID_in,"top_series":=TRUE]
table[,.N,top_series]



#size of text
boxplot(table$nb_char_text) 
# we see that some texts are very long. We must remove the very long ones to ameliorate the topic modeling by giving same importance to all texts
plot(density(table[top_OK==TRUE,nb_char_text])); mean(table[top_OK==TRUE,nb_char_text]); median(table[top_OK==TRUE,nb_char_text]) #rigth skewed
#additional rule for our variable top_OK
ID_in<-which(nchar(table$text)>200&percent_rank(nchar(table$text))<0.95); length(ID_in) #32 119 
ID_in2<-which(table$my_id %in% table[ID_in]$my_id); length(ID_in2) #32 119 
table[!ID_in2,"top_OK":=FALSE]
table[,.N,top_OK] #32119




#############
#TEXT MINING#
#############

############
#cleansing #
############
# you can always save the R elements, specially before removing elements, as we are going to do in the next lines
# saveRDS(table,paste0(getwd(),"/tmp/","tableALL_withnewvariables.rds")) 
# table<-readRDS(paste0(getwd(),"/tmp/","tableALL_withnewvariables.rds"))

table<-table[top_OK==TRUE]
#now we are just working with our optimal data. 
#Scope: only articles with at least 200 characters, from 2013-2014. 
#Three types of article: only article, gallery and article, and video and article
#Channels: entertainment, business, tech, us, culture, world, social-good


#last variable creation: top_city
#for example observation 00005: LAS VEGAS - ....
#city identification
table[,"top_city":=grepl("[ ]+\\p{Pd}",substring(text,1,20),perl=TRUE),by="my_id"] #this will match ' -' in the first 20 characters
table[,.N,top_city]
#Now we remove the city from the text
table[,"text":=paste0(gsub(".*\\p{Pd}","",substring(text,1,20),perl=TRUE),substring(text,21,nb_char_text))]

#word vector
# we don't want to modify the original text variable, so we create a copy of the table
#we save our my_id to keep track of the real data
wv<-table[,c("my_id","text")]
#we remove special characters
wv[,"text":=gsub("\n"," ",text)] #jump of line
wv[,"text":=gsub("\t"," ",text)] #tab
wv[,"text":=gsub("\r"," ",text)] #new paragraph
#we split the text by words. 
wv <- cSplit(wv,"text", sep = " ", direction = "long") #if you have problems with the memory of your pc, try to expand the memory limit, or split the data.table
#we want to remember the order of the words in the text
wv[,"order":=sequence(.N),c("my_id")]

#we remove websites, we could replace them with a word like 'website'. it is up to you!
wv[,Word:=gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", text)]
#we remove email address
wv[,Word:=gsub("[^\\s]*@[^\\s]*", " ", Word)]
#we remove punctuations
wv[,Word:=gsub("[[:punct:]]", " ", Word)]
#we remove words in other languages that are not recognized by the used encoding, in this case UTF-8
wv[,Word:=sub("\\<U\\+\\w+>", " ", Word, perl=T)]
#we remove digits and numbers
wv[,Word:=gsub("[[:digit:]]+", " ", Word)]
#we remove everything that is not a letter or a number
wv[,Word:=gsub("[^a-zA-Z]", " ", Word)]

wv[,Word:=tolower(Word)]
#all the spaces that we added previously are going to be removed
wv[,Word:=gsub(" ", "",Word)]

# you can always save the R elements, specially before removing elements, as we are going to do in the next lines
# saveRDS(wv,paste0(getwd(),"/tmp/","wordvector.rds")) 
# wv<-readRDS(paste0(getwd(),"/tmp/","wordvector.rds"))

#all the words with less than 3 characters are not usefull, so we remove that too. 
#do you think words with less than 4 characters are not importnat neither? you should try that too!
wv[,"top_nbcar":=nchar(as.character(Word)),]
ID_text<-which(wv$top_nbcar>3)
length(wv$my_id); length(unique(wv$Word)) #10 113 585 features, and 161 804 unique words. features considers ngram=1
wv<-wv[ID_text,]; length(wv$my_id); length(unique(wv$Word)) #now you can see how many words we removed.

#################
# Lemmatization #
#################
#we want to normalize our text. so we use lemmatization. if you don't have a vocabulary/dictionary you do stemming
#we charge the lemmatisator, you find this file in the blackboard. please save the file in the tmp folder
lemmas<-as.data.table(readRDS(paste0(getwd(),"/lemmatisator.rds")))
length(unique(lemmas$Word));length(unique(lemmas$Lemma)) 
#the lemmatisator has 182 684 unique words and 84 369 unique lemmas. 
# you can see more about this lemma database here: https://github.com/skywind3000/lemma.en

#we fix the key id variables on both tables, prior to the merge. The variables must have the same name.
setkey(lemmas,Word);setkey(wv,Word)
wv<-merge(x=wv,y=lemmas, by = "Word", all.x = TRUE, all.y = FALSE, sort = FALSE)
#we keep all of the content from our wv table, and we don't need all of the content of the lemmas table

#now we remove the lemmas with less than 3 characters
wv[,"top_nbcar2":=nchar(as.character(Lemma)),]
ID_text<-which(wv$top_nbcar2>3)
length(wv$Word); lemmav<-wv[ID_text,]; length(lemmav$Word) 
length(unique(wv$Word)); length(unique(lemmav$Word))
#we started with 10 113 585 features, then we cleaned and we had 6 235 617 and now we normalize and we have 4 554 251. 
#we started with    161 804 features, then we cleaned and we had   157 659 and now we normalize and we have    44 737. 

#with normalization we had removed all the words that are not in our vocabulary. 
#So we might check if there are important words that we need to include in the lemmatisator
tmp<-wv[!ID_text,]
tmp<-tmp[is.na(Lemma)]
#we check for the 600 most frequent words, that do not have lemma
tmp<-topfeatures(dfm(tmp$Word,verbose = TRUE,ngram=1),600) 
View(tmp) 
add_lemmas<-names(tmp)

#dealing with entities
#We see that there are many entities in this vocabulary, it is normal that they do not have a lemma. 
#we can either add the entities to the lemmatisator and use a lemma like "entity" or use groups like "firm", "celebrity", "website", "product"
#or we can add the entities using the lemma=word


#for this excercise we are going to include all the entities. and use lemma=word
#consider that some entities have a name that can be already included in the lemmatisator list by default. for example amazon
# list_remove<-c("facebook","mashable","iphone","instagram","flickr","microsoft","ipad","smartphone","samsung",
#                "tumblr","obama","istockphoto","francisco","netflix","reddit","sony","kickstarter","xbox","angeles",
#                "facebooks","linkedin","ferguson","chris","steve","ferguson","uber","spotify","snowden","itunes",
#                "snapchat","bitcoin","pinterest","islamic","mashables","playstation","microsofts","iphones","hong",
#                "eric","kevin","bluetooth","zuckerberg","brian","nokia","samsungs","verizon","kong","gmail","justin",
#                "ryan","bloomberg","obamas","jeff","imgur","sxsw","espn","sochi","intel","ipads","airbnb","tony",
#                "nintendo","whatsapp","alex","jennifer","skype","motorola","william","cameron","dropbox","icloud",
#                "tmobile","comcast","hulu","starbucks","wikileaks","diego","itll","emoji","ebay","ipod","miley","siri",
#                "fallon","beiber","fifa","evernote","wikipedia","andy","sonys","spacex","vladimir","charlie","inner",
#                "pete","iwatch","walmart","indiegogo","gameplay","etsy","kate","dave","kimmel","ballmer","larry",
#                "steven","macbook","mcdonalds","comiccon","michelle","paypal","neil","instagrams","buzzfeed","forbes",
#                "firefox","roku","beyonce","sharknado","swartz","abbott","nielsen","beyonc","nypd","emmys","cupertino",
#                "marissa","nadella","lebron","francis","gaga","phil","keith","christina","elon","poroshenko","sean",
#                "alibaba","ebook","kinect","craigslist","youtube")
# If you want to remove the entities you can use this:
#add_lemmas<-add_lemmas[add_lemmas %!in% list_remove]

#the rest of the words are going to be included in the lemmatisator, using lemma=word
lemmas<-rbind(lemmas,as.data.table(cbind(Word=add_lemmas,Lemma=add_lemmas)))
length(unique(lemmas$Word));length(unique(lemmas$Lemma)) 

#we match again with the new lemmas, but first we remove two variables
wv[,"Lemma":=NULL]
wv[,"top_nbcar2":=NULL]
wv<-merge(x=wv,y=lemmas, by = "Word", all.x = TRUE, all.y = FALSE, sort = FALSE)
wv[,"top_nbcar2":=nchar(as.character(Lemma)),]
ID_text<-which(wv$top_nbcar2>3)
length(wv$Word); lemmav<-wv[ID_text,]; length(lemmav$Word) 
length(unique(wv$Word)); length(unique(lemmav$Word))
#finally we have: 5 464 900 features and 45 337 unique words
#Normalization and cleaning allow us to remove all the words and characters that do not provide any relevant information 

#you can enhance the topic modeling with the lemmatisator. The clue is to give the right input to the algorithm.
#Once you finish with this you can save the lemmas
# saveRDS(lemmav,paste0(getwd(),"/tmp/","lemmavector.rds"))
# lemmav<-readRDS(paste0(getwd(),"/tmp/","lemmavector.rds"))

#now we reshape the text. we want to have two new variables: clean.word.text, lemma.text
wv<-wv[order(my_id,order)]
clean_word_text<-ddply(wv, .(my_id), summarise, clean.word.text=paste0(Word, collapse=" "))
lemmav<-lemmav[order(my_id,order)]
lemma_text<-ddply(lemmav, .(my_id), summarise, lemma.text=paste0(Lemma, collapse=" "))
length(clean_word_text$my_id);length(lemma_text$my_id) #we didn't lost any observation.

clean_word_text<-as.data.table(clean_word_text)
lemma_text<-as.data.table(lemma_text)
setkey(lemma_text,my_id)
setkey(clean_word_text,my_id)
clean_word_text<-merge(x=clean_word_text, y=lemma_text, by = "my_id", all.x = TRUE, all.y = FALSE, sort = FALSE)

setkey(clean_word_text,my_id)
setkey(table,my_id)
table<-unique(table,by="my_id")
table<-merge(x=table, y=clean_word_text, by = "my_id", all.x = TRUE, all.y = FALSE, sort = FALSE)
# saveRDS(table,paste0(getwd(),"/tmp/","tableOK_withlemmas.rds"))
# table<-as.data.table(readRDS(paste0(getwd(),"/tmp/","tableOK_withlemmas.rds")))

myhead_func(table,30) 


##################
# topic modeling #
##################
stopwords.1<-stopwords("english", source = "snowball")
#what is a stopword http://snowball.tartarus.org/algorithms/english/stop.txt
#stopword identification

#now let's the most frequent words in the text. We might find more stopwords
myDfm <- dfm(table$lemma.text, verbose = TRUE,valuetype = "regex"#,ngram=1:3
            #,remove=stopwords.1
)
#dfm ALWAYS transform the plural to singular and the ing form of the verb
#see the differences of the two elements. 
#tmp<-unique(lemmav$Word); tmp2<-unique(myDfm@Dimnames$features) 
topfeatures(myDfm,100)
paste0("'",paste(names(topfeatures(myDfm,100)),collapse="','"),"'")
#stopwords ellaboration
#we evaluate from the previous vector, the words that are stopwords
stopwords.2<-c('also','make','time','year','like','take','just','people','video','user','first','work',
'image','show','come','well','know','look','good','include','even','world','tell','game','want'
,'find','last','call','think','week','many','give','post','mashable','late','still','medium','need','dont',
'back','much','share','around','since','thing','month','youre','team','part','thats','long','next','another')

#now that we have modified our data, we change the format of it to run a tm
table<-as.data.frame(table)
#see textProcessor help to understand the package
#mainly we wrap the data, removing stopwords and running sparsity to words. 
processed<- textProcessor(table$lemma.text,metadata=table,
                          lowercase=FALSE, removestopwords=FALSE, removenumbers=FALSE, 
                          removepunctuation=FALSE, stem=FALSE, sparselevel=0.9975, 
                          language="en",verbose=TRUE, onlycharacter= FALSE, striphtml=FALSE
                          ,customstopwords=c(stopwords.1,stopwords.2)
                          )
#textProcessor is based on quanteda package, so the function always transforms the plural and ing. 
#After the transformation the number of unique words is 26 129. 
#applying sparsity we remove 21 508 words, because these words do not appear in at least 80 docs. (length(table$my_id)*(1-0.9975))

#We are going to remove words that appear in more than 5000 docs. this words may correspond to words from the website. 
out <- prepDocuments(processed$documents, processed$vocab, processed$meta, upper.thresh = 5000)
#we remove 11 terms more. 
#if we don't run sparsity we can remove words with a lower.threshold
#out <- prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = 50, upper.thresh = Inf)

#After removing stopwords and running sparsity,
#removing the very frequent words,
#and transforming plurals and ings.
#we have 2 527 134 tokens (features using ngrams too), and 4610 terms (unique words)

docs <- out$documents
#docs keep the original text structure
vocab <- out$vocab
#vocab allows STM to calculate the distribution of words for specific topic
meta <- out$meta
#meta allows STM to calculate the covariates.


###############
# CTM         #
###############
#time to run our first topic modeling. 
#we start with 10 topics. See at the bottom a code to choose the number of topics
nbTopics <- 10
MASHABLEctm <- stm(out$documents, out$vocab, K = nbTopics,data = out$meta,verbose=TRUE,
              #iterations before convergence:50 is already a good number. it depends of how much time do you have
              #The maximum number of EM iterations.
              max.em.its = 50, 
              #see a preview of the topics
              reportevery=10,
              #how to determine the prior parameters: Spectral corresponds to a spectral decomposition of the word co-ocurrence matrix
              #it is very usefull, it is based on the method of the moments and it is very consistent (see the paper https://cran.r-project.org/web/packages/stm/vignettes/stmVignette.pdf)
              #if the init.type is not spectral then we need to set seed.
              seed=NULL, init.type = "Spectral",
              #when there are no content covariates we need this algorithm specification
              #LDA tends to assign many rare words to a topic
              #if LDAbeta=FALSE, the model performs SAGE style (mandatory if content covariates)
              #SAGE: regularization method that ensures that words load onto tpoics only when they have sufficient counts to overwhelm the prior
              #SAGE topics have fewer unique words that distinguish one topic from another, but words are more likely to be meaningful.
              LDAbeta=TRUE,
              #sigma.prior a scalar between 0 and 1 which defaults to 0. This sets the strength of regularization towards a diagonalized covariance matrix
              #Setting the value above 0 can be useful if topics are becoming too highly correlated.
              sigma.prior=0
              )
#see the toic frequency
plot(MASHABLEctm, type="summary", xlim=c(0,.4))
#the histogram for each topic. in red the mean value. can you see the most frequent topics?
plot(MASHABLEctm, type="hist")

#the output of the topic-modeling is a matrix
View(MASHABLEctm$theta)
#we are going to combine it to the metadata. and give names to the topics
fittedData <- cbind(out$meta,MASHABLEctm$theta)
colnames(fittedData)<-c(colnames(fittedData)[1:34],paste0("tm_",str_pad(1:10,2,pad="0")))
#some characteristics of the matrix: the sum of every line is equal to 1, and the sum of the mean of every column is equal to 1
head(apply(MASHABLEctm$theta,1,sum),100)
sum(round(apply(MASHABLEctm$theta,2,sum)/length(MASHABLEctm$theta[,1]),digits=2))
#the frequency of the topic is given by the mean of the column. for example topic tm_09 has a frequency of 0.15
cbind(paste0("tm_",str_pad(1:10,2,pad="0")),round(apply(MASHABLEctm$theta,2,sum)/length(MASHABLEctm$theta[,1]),digits=2))

#each topic is a variable. 
summary(MASHABLEctm$theta)
#scalar between 0 and 1. it has the responsability that the topic explains the content of the text, where 1 is full responsability 

#see the quality of the topics in terms of exclusivity and coherence
#coherence: (Mimmo et al. 2011) maximized when the most probable words in a given topic frequently co-occur together
#exclusivity: FREX metric (Bischof and Airoldi 2012,2016) is the weighted harmonic mean of the word's rank in terms of exclusivity and frequency
topicQuality(model=MASHABLEctm, documents=docs)

#see the correlation of the topics 
M<-topicCorr(MASHABLEctm)$cor
M[M==1]<-0
corrplot(M, method="circle")
#the topics with positive correlations are represented together. positive correlation indicates that both topics are likely to be discussed within a documet
plot(topicCorr(MASHABLEctm))

#In term of topic_quality: the best topics are tm_02 and tm_08
#the most frequent topics are tm_09, tm_01, and tm_06
#some interesting relationship might be tm_02 and tm_04; tm_08, tm_10 and tm_05
#the most frequent topics are also related. maybe they have the common elements of an online news
#see the main words of the topic, lets display the topic 2 and 8
plot(MASHABLEctm, type="labels", topics=c(2,8))
plot(MASHABLEctm, type="perspectives", topics=c(2,8))
#topic 2 and 8 may be consider as one group: technology
par(mfrow=c(1,2))
cloud(MASHABLEctm,topic = 2, scale=c(4,.25))
cloud(MASHABLEctm,topic = 8, scale=c(4,.25))
#now we want to verify with some examples
findThoughts(MASHABLEctm,texts=table$text,n=3,topics=2,where=nb_char_text<500,meta=table)$docs[[1]]
findThoughts(MASHABLEctm,texts=table$text,n=3,topics=8,where=nb_char_text<500,meta=table)$docs[[1]]
#we can see that there is a relationship between these two topics. We might even say that the topics are: social network and ITCs

#we can do the same thing for the rest of topics
#Here is where you have to think: it is important to infere the meaning of the topics.
#We can save the topic modeling output
# saveRDS(MASHABLEctm,paste0(getwd(),"/output/","CTM_tm_spectral_k10.rds"))
# saveRDS(fittedData,paste0(getwd(),"/output/","CTM_data_spectral_k10.rds"))

###################
# STM             #
###################

#now let's include covariates to see the difference
#prevalence covariates are the time and author we can use type of article as another covariate
#all the prevalence covariate determine the order of the documents.
#the hierarchical structure of the documents
#we can include the number of topics too.
table[,.N,c("post_lead_type2","channel")][order(post_lead_type2)]

#content covariates are the topics. here we can use either the channel or the topic matrix. 
#but we can include only one variable, and it has to be a categorical variable
#so we decide to add the channel as content covariate
#again we just do simple, k=10 and inti.type="spectral"

nbTopics <- 10
MASHABLEstm <- stm(out$documents, out$vocab, K = nbTopics,
              max.em.its = 25,verbose=TRUE, reportevery=5,
              prevalence =~first_author+s(nb_days)+post_lead_type2,
              content =~channel,
              LDAbeta = FALSE,
              seed=123456789,data = out$meta, init.type = "Spectral")

fittedData <- cbind(out$meta,MASHABLEstm$theta)
colnames(fittedData)<-c(colnames(fittedData)[1:34],paste0("tm_",str_pad(1:10,2,pad="0")))

# saveRDS(MASHABLEstm,paste0(getwd(),"/output/","STM_tm_spectral_k10.rds"))
# saveRDS(fittedData,paste0(getwd(),"/output/","STM_data_spectral_k10.rds"))

# Try to see the frequency of this new output. 
# Are there correlations of topics? 
# do you think some topics make any sense?
# during the course we are going to analyze the output

#STM has an option to choose the optimal number of topics: 
# mashableSELECT<- searchK(out$documents, out$vocab, K = c(10,15,20),
# prevalence =~first_author+s(nb_days)+post_lead_type2,
# content =~channel,data = out$meta, init.type = "Spectral")
# plot(mashableSELECT)
