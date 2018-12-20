## Capstone Project
## Silvério Neves
## December 2018

# Prediction part
# Required libraries

library(tm)
library(stylo)
library(stringr)
library(dplyr)

# load data files
ngram_four <- readRDS("ngram_four.RDS")
ngram_three <- readRDS("ngram_three.RDS")
ngram_two <- readRDS("ngram_two.RDS")


#Predict next word
predictnw <- function(textinput)
{
        
        #Cleaning the input, get the number of words and initialize predictor
        wordin <- clean(textinput)
        nofwords <- length(wordin)
        pred <- c()
        
        if(nofwords>3)
        {
                wordin <- wordin[(nofwords-2):nofwords]
                pred <- ngram_four_get(wordin[1],wordin[2],wordin[3])
        }
        if(nofwords ==3)
        {
                pred <- ngram_four_get(wordin[1],wordin[2],wordin[3])
        }
        if(nofwords ==2)
        {
                pred <- ngram_three_get(wordin[1],wordin[2])
        }
        if(nofwords ==1)
        {
                pred <- ngram_two_get(wordin[1])
        }
        if(nofwords == 0)
        {
                pred <- "Please, enter some words"
        }
        pred <- unique(pred)
        if(length(pred)==0)
        {
                pred <- "Sorry but I was unable to suggest something"
        }
        if(length(pred) < 5)
        {
                pred
        }
        else
        {
                pred[1:5]
        }

}



#Clean input to extract specific words
clean <- function(txt){
        textin <- tolower(txt)
        textin <- removePunctuation(textin)
        textin <- removeNumbers(textin)
        textin <- str_replace_all(textin, "[^[:alnum:]]", " ")
        textin <- stripWhitespace(textin)
        textin <- txt.to.words.ext(textin, language="English.all", preserve.case = TRUE)
        return(textin)
}





# Functions to get word
ngram_four_get <- function (wi1,wi2,wi3)
        
{
        pred <- filter(ngram_four,(w1 == wi1 & w2 == wi2 & w3 == wi3))$w4
        if(length(pred) == 0)
        {
                
                pred <- filter(ngram_four,( w2 == wi2 & w3 == wi3))$w4
                if(length(pred) == 0)
                {
                        pred <- filter(ngram_four,( w1 == wi2 & w2 == wi3))$w3
                        
                        
                        if(length(pred) ==0)
                        {
                                pred <- ngram_three_get(wi2,wi3)
                        }
                        
                }
                
        }
        
        pred
        
}


ngram_three_get <- function(wi1,wi2)
{
        pred <- filter(ngram_three,( w1 == wi1 & w2 == wi2))$w3
        if(length(pred)==0)
        {
                pred <- filter(ngram_three,(w2 == wi2))$w3 
                
                if(length(pred)== 0)
                {
                        pred <- filter(ngram_three,(w1 == wi2))$word2 
                        
                        if(length(pred) ==0 )
                        {
                                pred <- ngram_two_get(wi2)
                        }
                        
                }
        }
        pred
}


ngram_two_get <- function(wi1)
{
        pred <- filter(ngram_two,( w1 == wi1 ))$w2
        
        pred
        
}


