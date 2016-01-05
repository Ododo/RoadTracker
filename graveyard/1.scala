
/**
 * Tentative de détection de route:
 
Cette fonction regarde pour chaque pixel, une série de voisins proches et une
série de voisins plus éloignés(=variable p ~ largeur de la route), établi un ratio 
nbrPixelsNoires/nbrPixelsBlancs et choisi la couleur du pixel courant
selon un seuil appliqué à ce ratio (ex: seuil=2).
Malheureusement elle ne fait qu'unifier partiellement les zones disparates,
ce qui renforce néenmoins le contraste.
Faire plusieurs itérations de cet algo ne semble pas donner un meilleur résutat.


Idée d'amélioration:
    Faire le ratio sur plusieurs tranches de voisins en affectant un poids 
    différent à chaque tranche.
 */

def roadTracker2(iw:ImageWrapper){
      var output = Array.ofDim[Int](iw.height, iw.width)
      var img = iw.getImage()

      for(i<-1 to iw.height-1){
        for(j<-1 to iw.width-1){
            var nb = 0f
            var nw = 0f
            
            if (img(i)(j) == BLACK){
              nb += 1f
            }else{
              nw += 1f
            }
           
            var p = 5
            val neighbors = Array(Array(i,j-1),Array(i,j+1),
                                  Array(i+1,j-1),Array(i+1,j),Array(i+1,j+1),
                                  Array(i-1,j-1),Array(i-1,j),Array(i-1,j+1),
                                  Array(i-1,j-p),Array(i-1,j+p),
                                  Array(i,j-p),Array(i,j+p),
                                  Array(i+1,j-p),Array(i+1,j+p),
                                  
                                  Array(i-p,j-1),Array(i-p,j),Array(i-p,j+1),
                                  Array(i+p,j-1),Array(i+p,j),Array(i+p,j+1))
                                                   
            for (k <- 0 until neighbors.length){
                var ii = neighbors(k)(0)
                var jj = neighbors(k)(1)
                
                if (ii < iw.height && ii > 0 && jj < iw.width && jj>0){
                  if (img(ii)(jj) == BLACK)
                    nb+=1
                  else
                    nw+= 1
                }
            }
            
            if (nb > 0f){
              var ratio = nw/nb
              if (ratio > 2)
                output(i)(j) = WHITE
              else
                output(i)(j) = BLACK
            }else{
              output(i)(j) = WHITE
            }
        }
      }
      copy(output, img)
  }
