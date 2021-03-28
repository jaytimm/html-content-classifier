## Introduction

> Extract main text content from web page. Remove boilerplate, ads,
> non-relevant content. Utilizing both supervised and unsupervised
> approaches.

``` r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, knitr, magrittr, dplyr, tidyr,
               text2vec)
```

## Extract web text

``` r
terms <- c('Europe', 'Education', 'Business', 
           'Sports', 'COVID-19', 'Science')

ts0 <- lapply(terms, quicknews::ggl_get_newsmeta) %>%
  bind_rows() %>% distinct() %>%
  mutate(id = row_number())

### progress -- also, parallel -- a la PubmedMTK -- 
txts0 <- qnews_extract_article(x = ts0$link, id = ts0$id)
```

``` r
## previous appraoch -- and useful to keep --
# y1 <- RCurl::getURL(q1$link[10], .encoding='UTF-8', ssl.verifypeer = TRUE,
#                     .opts = RCurl::curlOptions(followlocation = TRUE, verbose = T))
# 
# y2 <- boilerpipeR::ArticleExtractor(y1)
```

## Manual annotation

## Supervised node filtering

``` r
og$has_ellipses <- ifelse(grepl('\\.\\.\\.(.)?$', 
                                trimws(og$text)), 1, 0)
og$has_stop <-  ifelse(grepl('(\\.|\\!|\\?)(.)?$', trimws(og$text)), 1, 0)
```

> important to remember here – the `has_latest` filter will kill
> perfectly good sentences – however, they are filtered because they are
> SEE MORE, and not apart of main article –

``` r
og$has_latest <- ifelse(grepl('^latest( .*)? news$|^more( .*)? stories$',
                              trimws(og$text),
                              ignore.case = T), 
                        1, NA)

og$has_latest[og$place == 1] <- 0
og$has_latest <- zoo::na.locf(og$has_latest)
```

``` r
f0 <- subset(og, type == 'p')
table(f0$is_junk)
```

    ## 
    ##    0    1 
    ## 6676 4187

``` r
f1 <- subset(f0, 
             ## type == 'p',
             has_latest == 0 &
             has_ellipses == 0 &
             nchar(trimws(text)) > 0 &
             has_stop == 1)

table(f1$is_junk)
```

    ## 
    ##    0    1 
    ## 5998 1067

### sentence/node representation

``` r
clr_spacify_txt <- function (text) {
  t1 <- tokenizers::tokenize_ptb(text, lowercase = TRUE)
  t2 <- lapply(t1, gsub, pattern = '([a-z0-9])([[:punct:]])$', 
               replacement = '\\1 \\2')
  t3 <- lapply(t2, gsub, pattern = '^([[:punct:]])([a-z0-9])', 
               replacement = '\\1 \\2')
  t4 <- lapply(t3, paste0, collapse = ' ')
  unlist(t4) ####
}

f1$sptxt <- clr_spacify_txt(f1$text)
```

``` r
t2v_corp <-  text2vec::itoken(f1$sptxt, 
                              tokenizer = text2vec::space_tokenizer, 
                              ids = f1$sid)

vocab <- text2vec::create_vocabulary(t2v_corp, stopwords = tm::stopwords())
vocab1 <- text2vec::prune_vocabulary(vocab, doc_proportion_min = .02) 
dtm <- text2vec::create_dtm(t2v_corp, text2vec::vocab_vectorizer(vocab1))
colnames(dtm) <- janitor::make_clean_names(colnames(dtm))

model_tfidf = text2vec::TfIdf$new()
dtm <- model_tfidf$fit_transform(dtm)

fdtm <- data.frame(as.matrix(dtm)) 
fdtm0 <- janitor::clean_names(fdtm)
```

### Using SMOTE

``` r
z0 <- cbind(y = as.factor(f1$is_junk), fdtm0)

ds_rec <- recipes::recipe(y ~ ., data = z0) %>%
  recipes::step_meanimpute(recipes::all_predictors()) %>%
  themis::step_smote(y) %>%
  recipes::prep()

## sort(table(recipes::bake(ds_rec, new_data = NULL)$y, useNA = "always"))

smoted <- recipes::bake(ds_rec, new_data = NULL)
smoted_vals <- smoted[, ncol(smoted)]
smoted <- smoted[, -ncol(smoted)]
```

### Partitioning data

``` r
which_dtm <- smoted
vals <- smoted_vals$y

set.seed(134)
parts <- sample(1:2, 
                size = length(vals),
                prob=c(0.75, 0.25), 
                replace = TRUE) ## why T -- ??

trainx <- which_dtm [parts == 1,]
testx <- which_dtm[parts == 2,]

# create response and feature data
y_train <- as.factor(vals[parts == 1])
y_test <- as.factor(vals[parts == 2])
```

### Naive Bayes classifier

``` r
nb0 <- e1071::naiveBayes(trainx, y_train, laplace = 0.5) 
prediction <- predict(nb0, testx)
caret::confusionMatrix(prediction, y_test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1042  175
    ##          1  455 1296
    ##                                           
    ##                Accuracy : 0.7877          
    ##                  95% CI : (0.7726, 0.8023)
    ##     No Information Rate : 0.5044          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.5761          
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.6961          
    ##             Specificity : 0.8810          
    ##          Pos Pred Value : 0.8562          
    ##          Neg Pred Value : 0.7401          
    ##              Prevalence : 0.5044          
    ##          Detection Rate : 0.3511          
    ##    Detection Prevalence : 0.4100          
    ##       Balanced Accuracy : 0.7885          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

### Support Vector Machines

``` r
setwd(local_dir)
#saveRDS(svm0, 'svm_model.rds')
svm0 <- readRDS('svm_model.rds')
```

``` r
prediction <- predict(svm0, testx)
caret::confusionMatrix(prediction, y_test)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 1289   77
    ##          1  208 1394
    ##                                           
    ##                Accuracy : 0.904           
    ##                  95% CI : (0.8928, 0.9143)
    ##     No Information Rate : 0.5044          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8081          
    ##                                           
    ##  Mcnemar's Test P-Value : 1.355e-14       
    ##                                           
    ##             Sensitivity : 0.8611          
    ##             Specificity : 0.9477          
    ##          Pos Pred Value : 0.9436          
    ##          Neg Pred Value : 0.8702          
    ##              Prevalence : 0.5044          
    ##          Detection Rate : 0.4343          
    ##    Detection Prevalence : 0.4602          
    ##       Balanced Accuracy : 0.9044          
    ##                                           
    ##        'Positive' Class : 0               
    ## 
