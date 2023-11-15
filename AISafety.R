
description <- paste("Speech Analysis at AI Safety Summit 2023 on the", 
                  "1 and 2 November 2023 at Bletchley Park, Buckinghamshire.")

description

#Installing relevant packages
install.packages("pdftools") #To begin we load the pdftools package
install.packages("tm")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("dplyr")


library(pdftools)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(dplyr)

#Create a vector of PDF file names using the list.files() function.
#The pattern argument says to only grab those files ending with "PDF":

files <- list.files(pattern = "pdf$") #The files vector contains the two PDF file names

files

#Automate the process of reading in the text of the PDF files.
words <- lapply(files, pdf_text)

words

#Verify it contains three elements:
length (words)

lapply(words, length)


#Create a corpus, which is basically a database for text using
#the ‘Corpus’ function to extract text from the PDF documents

PDFdatabase <- Corpus (URISource(files), readerControl = 
                         list(reader = readPDF))

PDFdatabase


#Create a term-document matrix, or TDM for short that describes the frequency
#of terms that occur in the documents.

#Clean Up: list of control parameters
words.tdm <- TermDocumentMatrix(PDFdatabase,
              control = list(removePunctuation = TRUE,
              #To remove punctuation marks
              stopwords= TRUE,
              #To eliminate widely used words that carry 
              #very little useful information (such as are “a,” “the,” “is,” “are,”)
              tolower = TRUE,
              #Converts the given character to lowercase
              stemming = FALSE,
              #To prevent the process of reducing a word to its word stem that 
              #affixes to suffixes and prefixes or the roots.
              removeNumbers = TRUE,
              #To remove numbers
              bounds = list(global = c(2, Inf)))) 

words.tdm

#Inspect the TDM
inspect(words.tdm [1:10,]) #First 10 similar words on both docs
inspect(words.tdm [30:40,])
inspect(words.tdm [50:60,])
inspect(words.tdm [70:90,])

#Finding words frequencies

#Display a distribution of words within the documents
findFreqTerms(words.tdm, lowfreq = 5, highfreq = Inf)
#lowest frequency of 5 and highest of infinity

#Create and object
fr <- findFreqTerms(words.tdm, lowfreq = 5, highfreq = Inf)
fr


#Create and display a Matrix 
as.matrix(words.tdm[fr,])
fr.tdm <- as.matrix(words.tdm[fr,])
fr.tdm

#Sorting the frequently appearing words 
sort(apply(fr.tdm, 1, sum), decreasing = TRUE)


#Data processing to prepare to input the document-term matrix to the Word Cloud

# Convert the output to a matrix
matrix <- as.matrix(words.tdm)
head(matrix)

# Counting the frequency of the use of different words
words_count <- sort(rowSums(matrix), decreasing = TRUE)
head(words_count)

# Convert that output to a Dataframe
words_df <- data.frame(word = names(words_count), freq = words_count)
words_df

# Remove words that we don't want to include in the wordcloud
remove.rows <- which(words_df$word %in% c('will','today') )
words_df <- words_df[- remove.rows,]


#Using the ‘wordcloud’ function to create our Wordcloud!

wordcloud(words = words_df$word, freq = words_df$freq, min.freq = 4,           
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(10, "Dark2"))


#Modified Wordcloud with the ‘wordcloud2’ function!
wordcloud2(words_df, size = 0.5, color = 'random-dark', backgroundColor = 'white',
           rotateRatio = 0.35, minRotation = -pi/6, maxRotation = pi/6)


conclusion <- "Thank you for following and welcome again 😊"
cat(conclusion, "\n")










