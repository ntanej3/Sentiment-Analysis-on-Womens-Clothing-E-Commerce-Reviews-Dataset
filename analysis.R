
### Dataset: Womens Clothing E-Commerce Reviews
### Author: Neha Taneja

## Read the .txt file 

lines = readLines(file("~/Downloads/Womens Clothing E-Commerce Reviews.txt", open = "r"), encoding = "UTF-8")

## Find the number of reviews.

length(grep("^.{0,}Review Text:\\t{1,}.{1,}$", lines))

## extract information of "Title", "Review text", "Rating" and "Department name", and save them into 4 ## vectors

titles = c()
review_texts = c()
ratings = c()
departments = c()

split_parts = function(line_index) {
  parts = strsplit(trimws(lines[line_index], which="both"), "Title:\\t|\\tReview Text:\\t|\\tRating:\\t|\\tDepartment Name:\\t")[[1]][-1]
  
  titles[line_index] <<- parts[1]
  
  review_texts[line_index] <<- parts[2]
  
  ratings[line_index] <<- parts[3]
  
  departments[line_index] <<- parts[4]
  
}


split_lines = lapply(seq_along(lines), split_parts)


## Find the uncleaned titles and their format


titles[head(grep('^".{0,}"$', titles), 3)]

## Change the format of all the uncleaned titles as: 
##  "\"xxxxxxx\"" ----> "xxxxxxx" 

titles = gsub('^"(.{0,})"$', "\\1", titles, perl = T)

## sentimental analysis for the reviews between Dresses and Tops department.

# save review_top and review_dress in different vectors


move_review_data_frame = data.frame(title=titles, review_text=review_texts, rating=ratings, department = departments)

review_top = as.vector(move_review_data_frame[move_review_data_frame$department %in% c("Tops"), "review_text"])

review_dress = as.vector(move_review_data_frame[move_review_data_frame$department %in% c("Dresses"), "review_text"])

##(7) 
## Clean the data by eliminating apostrophes, numbers, and by changing all characters into lowercase 
## for review_dress and review_top.

## For example:
## "Finally a dress that is not too short! i'm 5'11 and ordered two sizes."
## ----> "finally a dress that is not too short im  and ordered two sizes"

review_dress = tolower(gsub("[^[:alpha:] ]", "", review_dress))
review_top = tolower(gsub("[^[:alpha:] ]", "", review_top))


## Split the reviews by removing blanks and save them to respective lists: token_top and token_dress 

removeEmptyWords = function(list) {
  return(list[list != ""])
}
token_top = removeEmptyWords(unlist(strsplit(review_dress, " ")))

token_dress = removeEmptyWords(unlist(strsplit(review_top, " ")))

## token frequency for each department

token_top_freq = table(token_top)
token_dress_freq = table(token_dress)

## exploratory analysis of the data and term frequencies to classify reviews as positive or negative

### Approach
"
1. Get a set of words associated with positive sentiment related to reviews.
2. Get a set of words associated with negative sentiment related to reviews.
3. Benchmark Tops and Dresses review text based on the frequency of these words in the data set.
"

### Resources
"
1. Negative sentiment words have been fetched from http://ptrckprry.com/course/ssd/data/negative-words.txt
2. Positive sentiment words have been fetched from http://ptrckprry.com/course/ssd/data/positive-words.txt

References

Minqing Hu and Bing Liu. \"Mining and Summarizing Customer Reviews.\" 
Proceedings of the ACM SIGKDD International Conference on Knowledge 
Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
Washington, USA, 
Bing Liu, Minqing Hu and Junsheng Cheng. \"Opinion Observer: Analyzing 
and Comparing Opinions on the Web.\" Proceedings of the 14th 
International World Wide Web conference (WWW-2005), May 10-14, 
2005, Chiba, Japan

"

documentWithPositiveWords = readLines("http://ptrckprry.com/course/ssd/data/positive-words.txt")
wordsWithPositiveSentiment = documentWithPositiveWords[36:length(documentWithPositiveWords)]
documentWithNegativeWords = readLines("http://ptrckprry.com/course/ssd/data/negative-words.txt")
wordsWithNegativeSentiment = documentWithNegativeWords[36:length(documentWithNegativeWords)]

# Frequency of positive words in Dress reviews
positive_freq_dress = token_dress_freq[wordsWithPositiveSentiment]
positive_freq_dress = positive_freq_dress[!is.na(positive_freq_dress)]

negative_freq_dress = (token_dress_freq[wordsWithNegativeSentiment])
negative_freq_dress = negative_freq_dress[!is.na(negative_freq_dress)]

positive_freq_top = (token_top_freq[wordsWithPositiveSentiment])
positive_freq_top = positive_freq_top[!is.na(positive_freq_top)]

negative_freq_top = (token_top_freq[wordsWithNegativeSentiment])
negative_freq_top = negative_freq_top[!is.na(negative_freq_top)]

plot(sort(positive_freq_dress, decreasing = T),
     col = "green", 
     main = "pos sentiment analysis for dresses",
     xlab = "words",
     ylab = "frequency")

plot(sort(negative_freq_dress, decreasing = T),
     col = "red",
     main = "neg sentiment analysis for dresses",
     xlab = "words",
     ylab = "frequency")

plot(sort(positive_freq_top, decreasing = T),
     col = "green",
     main = "pos sentiment analysis for tops",
     xlab = "words",
     ylab = "frequency")

plot(sort(negative_freq_top, decreasing = T),
     col = "red",
     main = "neg sentiment analysis for tops",
     xlab = "words",
     ylab = "frequency")

### Exploratory analysis and conclusion
"

Based on the 4 plots above, it is clear that both the reviews for Tops and Dresses contain positive sentiment in abundance to negative sentiment.

Anecdotal observation from the plot also suggests that reviews for Dresses tend to be more positive than reviews for Tops.

"

