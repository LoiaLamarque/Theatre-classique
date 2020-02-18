## THEATRE CLASSIQUE

## 1ERE ETAPE: TELECHARGER TOUS LES URL -------
library('rvest')
library('stringr')
library('gdata')
library('ggplot2')
library('dplyr')

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

for(url in url_5[1:length(url_5)]){
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

for(i in df$Pers[1: length(df$Pers)]){
  fem <- str_count(i, '2') / (str_count(i, '1') + str_count(i, '2'))
  if(is.na(fem)){fem= ""}
  Ratio_pers = append(Ratio_pers, fem)
}

df = data.frame(Title,Author,Period,Pers,Url, Ratio_pers)


for (i in df$Period[1 : length(df$Period)]){
  moy <- str_extract(df$Period, "....")
  if(is.na(moy)){moy = ""}
}

df = data.frame(Title,Author,Period,Pers,Url, Ratio_pers, moy)

### COMPTER REPLIQUES -----


test_2 <- read_html("http://www.theatre-classique.fr/pages/documents/ROTROU_BELISAIRE.xml") 

repliques <- test_2 %>%
  html_nodes('sp') %>%
  html_attr('who') %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("L ", "") %>%
  str_replace_all("LE ", "") %>%
  str_replace_all("LA ", "") %>%
  str_replace_all("MONSIEUR", "") %>%
  str_replace_all("MAÎTRE", "")



pers_2 <- test_2 %>%
  html_nodes('role') %>%
  html_attr('id') %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("L ", "") %>%
  str_replace_all("LE ", "") %>%
  str_replace_all("LA ", "") %>%
  str_replace_all("MONSIEUR", "") %>%
  str_replace_all("MAÎTRE", "")


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

sum(repliques_test) == length(repliques)

count = c()

for( i in 1:length(M[1,]))
{
  count = c(count, sum(M[,i]))
}


sex <- test_2 %>%
  html_nodes('role') %>%
  html_attr('sex') 
if(is.na(sex)){sex = ""}


df_2 <- data.frame(pers_2, sex, count)
ratio_repliques_sex <- sum(df_2$count, sex == '2')/ sum(df_2$count, sex == '1')



### ANALYSES STAT ------

df$Ratio_pers <- as.numeric(as.character(df$Ratio_pers))
df$moy <- as.numeric(as.character(df$moy)) 
ggplot(df, aes(x= moy, y= Ratio_pers)) + geom_point() + geom_smooth(linetype= "dashed", color = "red")

summary(lm(Ratio_pers~moy, df))

###brouillon
for(i in pers_2[1:length(pers_2)]){
  x <- str_count(string = repliques, pattern= i)
  if(is.na(x)){x = ""}
  sum <- sum(x)
  if(is.na(sum)){sum = ""}
  Pers_2 <- append(Pers_2, sum)
}

