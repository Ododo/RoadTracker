import com.tncy.top.image.ImageWrapper
import duda.Duda
import colors.values._

object RoadTracker extends App {
      
  val imgPath = "imgs/"
  val outputPath = "outputs/"
  val sobelThreshold = 40000000
  val simpleThreshold = 3500000
      
  process("ImagesTests/6.jpg", "sobel") //args(0)
  
  /**
   * Applique toute la chaine de transformation à l'image
   * spécifiée, filtres: sobel | simple
   */
  def process(name:String, filter:String){
      var iw:ImageWrapper = loadImage(name)
      var rw:ImageWrapper = loadImage(name)
      greyLevels(iw)
      saveImage(iw, name, "_grey")
      if (filter == "sobel"){
          sobelEdgeDetection(iw)
          saveImage(iw, name, "_sobel")
      }else if (filter == "simple"){
          simpleEdgeDetection(iw)
          saveImage(iw, name, "_simple")
      }else{
          println("filter not handled")
          return
      }
      roadTracker(iw)
      saveImage(iw, name, "_roads")
      merge(rw, iw)
      saveImage(rw, name, "_merged")
  }
  
  /**
   * Charge une image à partir du chemin de base
   * (imgPath)
   */
  def loadImage(source:String):ImageWrapper = {
      return new ImageWrapper(imgPath + source)
  }
  
  /**
   * Enregistre l'image dans outputPath, possibilité
   * de spécifier une chaine à ajouter à la suite du nom
   */
  def saveImage(iw:ImageWrapper, name:String, ind:String){
      iw.saveImage(outputPath + name.dropRight(4) + 
                   ind + name.takeRight(4))
  }
  
  /**
   * Copie une image src vers une image dest
   */
  def copy(src:Array[Array[Int]], dest:Array[Array[Int]]){
      for(i<-0 to dest.length-1){
        for(j<-0 to dest(i).length-1){
            dest(i)(j)=src(i)(j)
      }
    }
  }
  
    /* Toutes les fonctions qui suivent modifient directement
     * l'image passée en paramètre
     */
  
  /**
   * Convertit une image en niveau de gris
   */
  def greyLevels(iw:ImageWrapper){
      //fonction permettant de convertir une image couleur en niveau de gris
  
      var source = iw.getImage()
      
      for (i<-0 to iw.height-1){	    	
        for (j<-0 to iw.width-1){	
        
          val cValue:Int=source(i)(j)
          var gValue:Double = 0
          
          //On retrouve les valeurs décimales de chaque canal par un decalage à droite.
          val b = (cValue & 0xff)/255f	//b est la valeur en bleu de l'image
          val g = ((cValue >> 8) & 0xff)/255f	//g la valeur en vert
          val r = ((cValue >> 16) & 0xff)/255f  //r la valeur en rouge
          
          //calcul de la luminosité selon la recommandation 709
          val lumi = (0.2126*r +
                      0.7152*g +	
                      0.0722*b)	 	
          
          //passage en niveau de gris selon les valeurs de la luminosité
          if (lumi <= 0.0031308)				
              gValue = (12.92*lumi).toInt	
          else	
              gValue = (1.055*math.pow(lumi, 0.41666) - 0.055) // (1/2.4)=0.41666..
           
          gValue = gValue*255f
          
          //On réintègre les valeurs calculées dans l'image
          source(i)(j) = (gValue).toInt + 
                         ((gValue).toInt << 8) + 
                         ((gValue).toInt << 16) 
      }	
    }	
  }
    
  /**
   * Applique un filtre de sobel à l'image
   */
  def sobelEdgeDetection(iw:ImageWrapper){   
      var img = iw.getImage()
      var output = Array.ofDim[Int](iw.height, iw.width)

      for (i <- 1 to iw.height-2){
        for (j <- 1 to iw.width-2){
          
            var gx = img(i+1)(j-1) + 2*img(i+1)(j) + img(i+1)(j+1) -
                     img(i-1)(j-1) - 2*img(i-1)(j) - img(i-1)(j+1)
                 
            var gy = img(i-1)(j+1) + 2*img(i)(j+1) + img(i+1)(j+1) - 
                     img(i-1)(j-1) + 2*img(i)(j-1) - img(i+1)(j-1)
                 
            var g = math.sqrt(math.pow(gx,2) + math.pow(gy,2)).toInt
            
            if (g > sobelThreshold){
                output(i)(j) = BLACK
            }else{
                output(i)(j) = WHITE
            } 
        }
      }     
      copy(output, img)
  }  
  
  /**
   * Applique un filtre basé sur un seuil appliqué
   * au calcul du gradient pour chaque pixel
   */
  def simpleEdgeDetection(iw:ImageWrapper){
      var tab = Array.ofDim[Int](iw.height, iw.width)
      var source = iw.getImage()
      
      for(i<-2 to iw.height-2){
        for(j<-2 to iw.width-2){
            //Calcul du gradient pour chaque pixel
            var x=math.abs(source(i)(j+1)-source(i)(j-1)) +
                  math.abs(source(i+1)(j)-source(i-1)(j))
          
            if (x>simpleThreshold) { //Si cette valeur dépasse un seuil alors, c'est un contour
                tab(i)(j)=BLACK   //Les contours en noir
            }
            else {
                tab(i)(j)=WHITE //Le reste en blanc
            }
        }
      }
      copy(tab, source)
  }
  
  /**
   * Applique le Duda Road Operator (DRO) sur l'image.
   */
  def roadTracker(iw:ImageWrapper){
      copy(Duda.process(iw), iw.getImage())
  }
  
  /**
   * Superpose une image d'origine (iw1) et une image
   * résultat (iw2), écrit dans iw1
   */
  def merge(iw1:ImageWrapper, iw2:ImageWrapper){
      var imageBase=iw1.getImage()
      var imageRes=iw2.getImage()

      if(iw1.height != iw2.height || iw1.width != iw2.width){
        println("Erreur: dim(iw1)!=dim(iw2)")
        return
      }
        
      var result=Array.ofDim[Int](iw1.height,iw1.width)
      
      for(i<-0 to iw1.height-1){
      for(j<-0 to iw1.width-1){
        if (imageRes(i)(j)==WHITE)
            result(i)(j)=RED
        else
            result(i)(j)=imageBase(i)(j)
      }}
      copy(result,imageBase)
  }
  
}