## Fonction permettant de cr?er puis am?liorer un plot (texte, points, lignes...) avant de l'enregistrer en fichier (pdf ou autre)

## Entr?es de la fonction :
##   - type       : format de sortie souhait?e (pdf, png, jpg, wmf)
##   - filewidth  : largeur de la fen?tre graphique lors de l'enregistrement (pour am?liorer la r?solution)
##   - fileheight : hauteur de la fen?tre graphique lors de l'enregistrement
##   - fileName   : le nom ? donner au fichier lors de l'enregistrement (SANS l'extension ".pdf", ".jpg", ...)
##   - CALLFUN    : objet ou vecteur d'objets issus de la fonction call(), qui traduit les diff?rentes fonctions pour cr?er et modifier les graphiques avant enregistrement
##                    --> par exemple, CALLFUN=call("plot",1:3,2:4) ?quivaudrait ? taper manuellement plot(1:3,2:4), mais CALLFUN est ici un "objet" et pas un r?sultat de fonction!
##                    --> s'il y a plusieurs fonctions ? ex?cuter successivement, il faut les concat?ner : CALLFUN=c( call("plot",1:3,2:4), call("text",x=1.5,y=2.5) )

## Am?liorations :
## on peut utiliser "title("Titre", outer=T, line=-1)" pour cr?er un titre g?n?ral si la fen?tre graphique est divis?e
## pour la boucle contenant "eval(CALLFUN[[i]])", on pourrait la remplacer par "lapply(CALLFUN, FUN = eval)" (gain de temps notable ? pas s?r...)
## la division de la fen?tre graphique et tous les ajouts de titre/l?gende... peuvent aussi ?tre int?gr?s sous forme de "call()", donc on les ajoute dans CALLFUN

 GenericPlot=function(type="R", filewidth=7, fileheight=7, fileName="output", CALLFUN=NULL) 
 {
   CALLFUN=c(CALLFUN)    # cela s'av?re n?cessaire lorsqu'il n'y a qu'une seule fonction : la concat?nation de "call()" renvoie une liste

   if(!is.character(type)) stop("The output format (type) must be a string!")
   if(!is.character(fileName)) stop("The output name (fileName) must be a string!")
	if(nchar(fileName)>175){fileName=substring(fileName,first=1,last=190)}
   # if(type=="pdf") { pdf(file=paste(fileName,".pdf",sep=""), width=filewidth,height=fileheight)} else
   # if(type=="png") { png(file=paste(fileName,".png",sep=""), width=100*filewidth,height=100*fileheight)} else
   # if(type=="jpg") { jpeg(file=paste(fileName,".jpg",sep=""), width=100*filewidth,height=100*fileheight)} else
   # if(type=="wmf") { win.metafile(file = paste(fileName,".wmf",sep=""), width=filewidth, height=fileheight) } else
   # { x11()}
	res=list()
   if(!is.null(CALLFUN))
   {
		for(i in 1:length(CALLFUN))
		{
			res[[i]]= eval(CALLFUN[[i]])   
		}   
   }
 
#    if (type!="R")
#    {
# 	   dev.off()   # il ne faut fermer que le device actuel, alors que graphics.off() les ferme tous !! 
#    }
   invisible(res)

 }

INTERNAL_GetCex=function(nbPoints)
{
	cex=1
	if (nbPoints<5) {cex=1.3}
	else if (nbPoints<10) {cex=1.2}
	else if (nbPoints<15) {cex=1.1}
	else if (nbPoints>100) {cex=0.5}
	else if (nbPoints>60) {cex=0.6}
	else if (nbPoints>50) {cex=0.7}
	else if (nbPoints>40) {cex=0.8}
	else if (nbPoints>20) {cex=0.9}    
	else {cex=1}
	return (cex)
}

### Exemple d'utilisation : on veut cr?er un plot, puis ajouter du texte

#  GenericPlot("pdf",7,7,"test3",
#              CALLFUN=c(call("plot",1:3,2:4),       # ?quivaut ? plot(1:3,2:4)
#                        call("text",x=1.5,y=2.5),   # ?quivaut ? text(x=1.5,y=2.5)
#                        34,                         # 34 n'est pas un objet issu de la fonction call(), donc sera ignor? par la fonction
#                        call("text",x=3,y=4)))      # ?quivaut ? text(x=3,y=4)
#
#       --> Cela ?quivaut ? taper successivement : plot(1:3,2:4) ; text(x=1.5,y=2.5) ; text(x=3,y=4)
#           puis ? enregistrer le graphique final sous le fichier "test3.pdf".