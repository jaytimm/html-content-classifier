## Introduction

> Extract main text content from web page. Remove boilerplate, ads,
> non-relevant content. Supervised and unsupervised. No Java dependency.

``` r
remotes::install_github("jaytimm/quicknews")
```

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
txts0 %>% filter(id == 11) %>% knitr::kable()
```

| id  | type | text                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:----|:-----|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 11  | h1   | Migrant arrivals to Europe lower but deaths remain high                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| 11  | h2   |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| 11  | p    | Mar 27, 2021                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 11  | p    | In this Nov. 11 photo, refugees and migrants are rescued by members of the Spanish NGO Proactiva Open Arms, after leaving Libya trying to reach European soil aboard an overcrowded rubber boat in the Mediterranean sea. Though the number of migrants and asylum seekers reaching Europe in 2020 is the lowest it has been in the past decade, deaths and disappearances on sea routes to the continent remained alarmingly high with only a small percentage of bodies being recovered according to a report released Friday by UN’s migration agency. (AP photo) |
| 11  | p    | BARCELONA, Spain — The number of migrants and asylum-seekers who reached Europe in 2020 is the lowest it has been in the past decade, according to a report released Friday by the United Nations migration agency. But deaths and disappearances on sea routes remain alarmingly high with only a small fraction of bodies recovered and victims identified.                                                                                                                                                                                                        |
| 11  | p    | Of the 93,000 people who entered Europe irregularly last year, roughly 92% did so via the Western, Central and Eastern Mediterranean Sea, as well as through the Atlantic Ocean off West Africa to Spain’s Canary Islands, often on unseaworthy boats.                                                                                                                                                                                                                                                                                                               |
| 11  | p    | Arrivals in the Canaries, considered part of the Schengen area, increased by 750% last year. The numbers had already picked up before the pandemic following tougher border controls and interceptions on the Mediterranean by North African countries, but COVID-19 seems to have “acted as a multiplier of existing factors motivating migration on this route,” the report said.                                                                                                                                                                                  |
| 11  | p    | It added that many migrants previously worked in sectors such as fishing and agriculture that have suffered greatly from the economic consequences of the pandemic.                                                                                                                                                                                                                                                                                                                                                                                                  |
| 11  | p    | The sea routes are lethal. The International Organization for Migration’s Missing Migrants Project has confirmed the death or disappearances of nearly 2,300 people last year. This number is higher than in 2019 when 2,095 victims were recorded and slightly lower than in 2018 which had 2,344.                                                                                                                                                                                                                                                                  |
| 11  | p    | The Central Mediterranean north of Libya saw 984 people perish in 2020. Meanwhile, on the Atlantic route to the Canaries at least 849 victims were recorded — more than four times as many as in any previous year, according to the report, “Maritime Migration to Europe: Focus on the Overseas Route to the Canary Islands.”                                                                                                                                                                                                                                      |
| 11  | p    | Another 300 deaths have already been documented so far this year, the report said.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 11  | p    | The organization admits its data is incomplete. So-called “invisible shipwrecks,” when entire boats disappear and leave no survivors, are especially concerning.                                                                                                                                                                                                                                                                                                                                                                                                     |
| 11  | p    | Not included in the report’s death toll for last year are nine cases of invisible shipwrecks reported in the Atlantic and Mediterranean last year with hundreds of additional potential victims, according to IOM data requested by the AP.                                                                                                                                                                                                                                                                                                                          |
| 11  | p    | “Such cases are extremely difficult to detect, let alone verify, and are yet another indication that the true number of deaths on maritime routes to Europe is far higher than indicated by the available data,” the report said.                                                                                                                                                                                                                                                                                                                                    |
| 11  | p    | The Associated Press has most recently come across an example of uncounted deaths after interviewing two survivors who reached the Canary Islands last November. According to them more than 20 people on their boat did not survive the two-week odyssey.                                                                                                                                                                                                                                                                                                           |
| 11  | p    | The group of approximately 180 people had departed the town of Joal-Fadiouth in Senegal but ran out of food, water and fuel after the eighth day.                                                                                                                                                                                                                                                                                                                                                                                                                    |
| 11  | h1   | Newsletter                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 11  | p    | Today’s breaking news and more in your inbox                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 11  | h1   | International News                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 11  | h1   | Ethiopia says Eritrea agrees to withdraw troops from Tigray                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
| 11  | p    | KHARTOUM, Sudan (AP) — Ethiopia’s prime minister said Friday that Eritrea has agreed to withdraw its forces …                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 11  | h1   | Maritime traffic jam grows outside blocked Suez Canal                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 11  | h1   | Brighter outlook for US as vaccinations rise, deaths fall                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| 11  | h1   | Germany’s Merkel: EU needs to increase vaccine production                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| 11  | p    | BERLIN — The European Union’s problems with getting deliveries of coronavirus vaccines have underscored the …                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 11  | h1   | WWII codebreaker Turing honored on UK’s new 50-pound note                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| 11  | h1   | North Korea test-fires ballistic missiles in message to US                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 11  | p    | SEOUL, South Korea — North Korea on Thursday test-fired its first ballistic missiles since President Joe Biden …                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| 11  | h1   | Munising, Superior Central boys earn Skyline Central Conference basketball honors                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| 11  | h1   | Olympic torch relay kicks off 121-day journey around Japan                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 11  | h1   | Michigan Wolverines crash party of familiar faces in NCAA Women’s Tournament Sweet 16                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 11  | h1   | Today in History: More than 500 killed in worst aviation disaster of all time                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| 11  | h1   | Rodney M. Schultz                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| 11  | h1   | Michigan Wolverines, Villanova Wildcats push past key injuries in Sweet 16 runs of NCAA tournament                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 11  | h1   | Newsletter                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| 11  | p    | Today’s breaking news and more in your inbox                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| 11  | p    |                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| 11  | h3   | Starting at $4.75/week.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| 11  | h1   | Subscribe Today                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| 11  | p    | Copyright © The Mining Journal \| <https://www.miningjournal.net> \| 249 W. Washington, Marquette, MI 49855 \| 906-228-2500 \| Ogden Newspapers \| The Nutting Company                                                                                                                                                                                                                                                                                                                                                                                               |

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
    ##          0 1079  172
    ##          1  418 1299
    ##                                           
    ##                Accuracy : 0.8012          
    ##                  95% CI : (0.7864, 0.8154)
    ##     No Information Rate : 0.5044          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.603           
    ##                                           
    ##  Mcnemar's Test P-Value : < 2.2e-16       
    ##                                           
    ##             Sensitivity : 0.7208          
    ##             Specificity : 0.8831          
    ##          Pos Pred Value : 0.8625          
    ##          Neg Pred Value : 0.7566          
    ##              Prevalence : 0.5044          
    ##          Detection Rate : 0.3635          
    ##    Detection Prevalence : 0.4215          
    ##       Balanced Accuracy : 0.8019          
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
    ##          0 1289   79
    ##          1  208 1392
    ##                                           
    ##                Accuracy : 0.9033          
    ##                  95% CI : (0.8921, 0.9137)
    ##     No Information Rate : 0.5044          
    ##     P-Value [Acc > NIR] : < 2.2e-16       
    ##                                           
    ##                   Kappa : 0.8067          
    ##                                           
    ##  Mcnemar's Test P-Value : 4.169e-14       
    ##                                           
    ##             Sensitivity : 0.8611          
    ##             Specificity : 0.9463          
    ##          Pos Pred Value : 0.9423          
    ##          Neg Pred Value : 0.8700          
    ##              Prevalence : 0.5044          
    ##          Detection Rate : 0.4343          
    ##    Detection Prevalence : 0.4609          
    ##       Balanced Accuracy : 0.9037          
    ##                                           
    ##        'Positive' Class : 0               
    ## 
