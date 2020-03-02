## THEATRE CLASSIQUE

## 1ERE ETAPE: TELECHARGER TOUS LES URL -------
library('rvest')
library('stringr')
library('gdata')
library('ggplot2')
library('dplyr')
library('readr')

code_source <- read_html('http://www.theatre-classique.fr/pages/programmes/PageEdition.php')
url <- code_source %>%
  html_nodes("a") %>%
  html_attr("href")


url_1 <- url[str_detect(url, 'xml$')]
url_2 <- url_1[str_detect(url_1, "^../")]
url_3<- str_replace_all(url_2, '^..', '')
url_4 <- paste("http://www.theatre-classique.fr/pages", url_3)
url_5 <- str_replace_all(url_4, ' ', '')
url_5[850] <- 'http://www.theatre-classique.fr/pages/documents/MARIVAUX%20_ACTEURSDEBONNEFOI.xml'
url_5 <- url_5[-928]
url_5[978] <- 'http://www.theatre-classique.fr/pages/documents/MUSSET_CAPRICES.xml'

## 2EME ETAPE: TELECHARGER TOUTES LES INFOS------ 

Title = vector("character")
Author = vector("character")
Period = vector("character")
Pers = vector("character")
Url = vector("character")
Ratio_pers = vector("numeric")
Moy = vector("character")


for(url in url_5[1:300]){
  test <- read_html(url)
  title <- test %>% 
    html_nodes("title")%>%
    html_text()
  if(is.na(title)){title = ""}
  Title = append(Title,title)
  
  Url = append(Url, url)
  
  author <- test %>%
    html_node('author') %>%
    html_text()
  if(is.na(author)){author = ""}
   Author = append(Author,author)
   
  period <- test %>%
    html_nodes('periode') %>%
    html_text()
  if(is.na(period)){period = ""}
  Period = append(Period,period)
  
  pers <- test %>%
    html_nodes('role') %>%
    html_attr('id')
  if(is.na(pers)){pers = ""}
  
  sex <- test %>%
    html_nodes('role') %>%
    html_attr('sex') 
  if(is.na(sex)){sex = ""}
  
  A = ""
  for(i in 1:length(pers)){
    z = paste(pers[i],as.character(sex[i]),sep = ",")
    A = paste(A,z,sep=";")
    if(is.na(A)){A= ""}
  }
  Pers = append(Pers,A)
  
}

df = data.frame(Title,Author,Period,Pers,Url)

##COMPTER PERS FEMININS -------
###ratio pers feminins/ pers masculins 

for(i in df$Pers[1: length(df$Pers)]){
  fem <- str_count(i, '2') / (str_count(i, '1') + str_count(i, '2'))
  if(is.na(fem)){fem= ""}
  Ratio_pers = append(Ratio_pers, fem)
}

df = data.frame(Title,Author,Period,Pers,Url, Ratio_pers)

### TELECHARGER REPLIQUES, PERS ET SEX POUR CHAQUE PIECE  ----

  test_2 <- read_html(url) 
  
  unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
                            'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
                            'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  repliques = c()
  
  repliques <- test_2 %>%
    html_nodes('sp') %>%
    html_attr('who') %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("L ", "") %>%
    str_replace_all("LE ", "") %>%
    str_replace_all("LA ", "") %>%
    str_replace_all("MONSIEUR ", "") %>%
    str_replace_all("MAÎTRE ", "") %>%
    str_replace_all("DON ", "") %>%
    str_replace_all("DONA ", "") %>%
    str_replace("VIRGILIE", "VIRGILE")%>%
    str_replace("MARTHON", "MARTON") %>%
    str_replace_all("GULLAUME", "GUILLAUME") %>%
    str_replace_all("PARDONNEUR", "PARDONNER")%>%
    str_replace_all("UNE ", "") %>%
    str_replace_all("UN ", "") %>%
    str_replace_all("SECOND ", "DEUXIEME") %>%
    str_replace_all("LES  ", "") %>%
    str_replace_all("CHOEUR", "") %>%
    str_replace_all(" ","")
  
  
  
  
  repliques <- chartr(paste(names(unwanted_array), collapse=''),paste(unwanted_array, collapse=''), repliques)
  
  
  repliques <- subset(repliques, repliques != "")
  
  
  pers_2= c()
  
  pers_2 <- test_2 %>%
    html_nodes('role') %>%
    html_attr('id') %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("L ", "") %>%
    str_replace_all("LE ", "") %>%
    str_replace_all("LA ", "") %>%
    str_replace_all("MONSIEUR ", "") %>%
    str_replace_all("MAÎTRE ", "") %>%
    str_replace_all("DON ", "") %>%
    str_replace_all("DONA ", "") %>%
    str_replace("VIRGILIE", "VIRGILE") %>%
    str_replace("MARTHON", "MARTON") %>%
    str_replace_all("GULLAUME", "GUILLAUME") %>%
    str_replace_all("UNE ","") %>%
    str_replace_all("UN ", "") %>%
    str_replace_all("SECOND ", "DEUXIEME") %>%
    str_replace_all("LES  ", "") %>%
    str_replace_all("CHOEUR", "") %>%
    str_replace_all(" ","") 
  
  
  
  pers_2 <- chartr(paste(names(unwanted_array), collapse=''),paste(unwanted_array, collapse=''), pers_2)

  sex <- test_2 %>%
    html_nodes('role') %>%
    html_attr('sex') 
  if(is.na(sex)){sex = ""}
    
  df <- data.frame(pers_2, sex)
  
  name_url <- paste(url) %>%
    str_replace_all("/", " ")
  
  write.csv(repliques, paste(name_url, "repliques"), row.names = FALSE, col.names = FALSE)
  write.csv(df, paste(name_url, "pers"), row.names = FALSE, col.names = FALSE)
  


### COMPTER REPLIQUES -----


files_repliques <- list.files(path="/Users/loialamarque/Desktop/Cogmaster/M2/Stage/Patriarcat/theatre_classique", pattern = "repliques")
files_pers <- list.files(path="/Users/loialamarque/Desktop/Cogmaster/M2/Stage/Patriarcat/theatre_classique", pattern = "pers")

Ratio_repliques_sex = vector("numeric")
Url = vector("character")

for(x in files_repliques[1:300])
{
  
  repliques_2 <- read.csv(x) 
  
  repliques <- as.vector(repliques_2 [, 1])
  repliques[is.na(repliques)] <- ""
  
  
  url_repliques <- paste(x) %>%
    str_replace_all("repliques", "")
  
  url <- url_repliques %>%
    str_replace_all(" ", "/") %>%
    str_remove("/$")
  
  Url= append(Url, url)
  
  url_pers <- files_pers[str_detect(files_pers, url_repliques)]
  
  pers_2_2 <- read.csv(url_pers)
  pers_2_2 <- pers_2_2 %>%
    distinct()
  pers_2 <- as.vector(pers_2_2[, 1])
  sex <- as.vector(pers_2_2[, 2])
  pers_2 <- str_replace_all(pers_2, "MADME", "MADAME")
  
  repliques <- str_replace_all(repliques, "ENSEMBLE", "")
  repliques <- str_replace_all(repliques, "MADME", "MADAME")
  repliques <- subset(repliques, repliques != "")
  
  ratio_repliques_sex = c()
  
  if(length(repliques) == 0)
  {
    ratio_repliques_sex = paste("Probleme avec : ", url)
    Ratio_repliques_sex = append(Ratio_repliques_sex, ratio_repliques_sex)
    next
  } 
  
  M= matrix(0, length(repliques), length(pers_2))
  for (i in 1:length(repliques)) {
    for (j in 1:length(pers_2)) {
      if(length(grep(repliques[i], pers_2)) > 1)
      {
        if(repliques[i] == pers_2[j])
        {
          M[i, j] = 1
        }
      }
      else
      {
        M[i, j]= length(grep(repliques[i], pers_2[j]))
      }
      
    }
  }
  
  
  repliques_test = c()
  
  for( i in 1:length(repliques))
  {
    repliques_test = c(repliques_test, sum(M[i,]))
  }
  
  x <- which(repliques_test == 0)
  tri <- unique(repliques[x])
  
  count = c()
  
  for(i in 1:length(M[1,]))
  {
    count = c(count, sum(M[,i]))
  }
  
  
  df_3 <- data.frame(pers_2,count, sex)
  
  
  if ((sum(repliques_test) - length(repliques))^2 < ((5/100)*length(repliques))^2)
  {
    ratio_repliques_sex <- sum(df_3$count[which(df_3[,3] == 2)]) / (sum(df_3$count[which(df_3[,3] == 1)]) + sum(df_3$count[which(df_3[,3] == 2)]))
    
  }
  
  else {
    ratio_repliques_sex = ""
  }
  
  print(ratio_repliques_sex)
  
  Ratio_repliques_sex = append(Ratio_repliques_sex, ratio_repliques_sex)
  
}


Df_5 = data.frame(Url, Ratio_repliques_sex)
df_6 <- left_join(df, Df_5, by= "Url")



### ANALYSES STAT ------

## simplification periode 

for (i in df_6$Period[1 : length(df_6$Period)]){
  moy <- str_extract(df_6$Period, "....")
  if(is.na(moy)){moy = ""}
}

df_7 = data.frame(Title,Author,Period,Pers,Url, Ratio_pers, moy, Ratio_repliques_sex)
df_8 <- subset(df_7, moy < 1800)

##ANALYSE

df_1$Ratio_repliques_sex <- as.numeric(as.character(df_1$Ratio_repliques_sex))
df_1$moy <- as.numeric(as.character(df_1$moy)) 
ggplot(df_1, aes(x= moy, y= Ratio_repliques_sex)) + geom_point() + geom_smooth(method= 'lm')


summary(lm(Ratio_repliques_sex + Ratio_pers ~ moy, df_1))

summary(lmer(Ratio_repliques_sex ~ moy + (1| Author), df))

