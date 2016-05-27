
library(timeDate)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(lubridate)
library(tm)
library(wordcloud)
library(stringi)

#Limpieza
base<-read.csv("_chat.txt",header=FALSE,sep="|",quote="")

#Separamos poco a poco
base$fecha<-gsub("\\.","",substr(base$V1, 1, 21))
base<-base%>%
  mutate(V2=gsub("\\.:",":",substring(V1, 22)))%>%
  select(-V1)

#Split Sobre el mensaje para separar la persona y el mensaje
base<-base%>%
     mutate(persona=substring(V2,1,
        lapply(base$V2,function(x) gregexpr(":",x)[[1]][2])))%>%
     mutate(mensaje=substring(V2,
        as.numeric(lapply(base$V2,function(x) gregexpr(":",x)[[1]][2]))+1))%>%
     mutate(persona=as.factor(gsub("\\:","",persona)))%>%
     select(-V2)%>%
     filter(!grepl("omitted",mensaje))%>%
     filter(!is.na(persona))

#Ahora nos falta limpiar la fecha
base<-base%>%
  mutate(fecha=ifelse(nchar(fecha)==19&grepl("pm",fecha),
                       paste(substr(fecha,1,9),paste0(as.numeric(substr(fecha,10,10))+12,substr(fecha,11,16))),
                       ifelse(nchar(fecha)==20&grepl("pm",fecha)&(substr(fecha,10,11))!=12
                      ,paste(substr(fecha,1,9),paste0(as.numeric(substr(fecha,10,11))+12,substr(fecha,12,17))),
                       ifelse(grepl("am",fecha)&substr(fecha,10,11)==12,paste(substr(fecha,1,9),paste0("00",substr(fecha,12,17))),
                       fecha))))%>%
  mutate(fecha=gsub("am","",fecha))


base$fecha<-dmy_hms(base$fecha)
levels(base$persona)<-c("Persona_1","Persona_2")
base$date <- as.Date(base$fecha, format = "%d/%m/%y")

#Ahora si empezamos el análisis
#Histórico
ggplot(base,aes(x=date,fill=persona))+
  geom_histogram(binwidth=7,position='dodge')+
  xlab("Fecha")+ylab("# Mensajes por 7 días")+
  scale_fill_manual(values=c("mediumseagreen","dodgerblue4"))+
  labs(fill="")

#Por hora
ggplot(base,aes(x=hour(base$fecha),fill=persona))+
  geom_histogram(binwidth=1,position='dodge')+
  xlab("Hora México")+ylab("# Mensajes por hora")+
  scale_fill_manual(values=c("mediumseagreen","dodgerblue4"))+
  labs(fill="")+
  xlim(0,23)+
  ggtitle("Whatsapp's por Hora")

#Función de texto por persona
grafic<-function(nombre)
{
    base_c <- base%>%
    filter(persona==nombre)
    corpus_c<-Corpus(DataframeSource(data.frame(base_c$mensaje)))
    corpus_c <- tm_map(corpus_c, removePunctuation)
    corpus_c <- tm_map(corpus_c, removeWords, stopwords('spanish'))
    corpus_c <- tm_map(corpus_c, function(x) removeWords(x, stopwords("spanish")))
    tdm <- TermDocumentMatrix(corpus_c)
    tdm <- TermDocumentMatrix(corpus_c)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    pal <- brewer.pal(9, "BuGn")
    pal <- pal[-(1:2)]
    
    d$word<-stri_trans_general(d$word,"Latin-ASCII")
    
    wordcloud(d$word,d$freq, scale=c(2,1),min.freq=5,max.words=170,
    random.order=FALSE, rot.per=.15,colors=brewer.pal(3, "Dark2"),
    vfont=c("sans serif","plain"))
    palabras_nombre<-nrow(d)
    return(palabras_nombre)
}

#¿Qué dice cada persona?
p1<-grafic("Persona_1")
p2<-grafic("Persona_2")

#Mayor vocabulario
persona<-c('Persona_1','Persona_2')
total<-c(p1,p2)

palabras<-data.frame(factor(persona,level=persona),total)
names(palabras)[1]<-'persona'


ggplot(palabras, aes(x = persona, y=total)) + geom_bar(stat="identity", fill="dodgerblue4", color="dimgrey") +
  ggtitle('') +ylab('# de palabras utilizadas') + xlab('') +
  geom_text(aes(label=round(total,1)), vjust=-0.5) 