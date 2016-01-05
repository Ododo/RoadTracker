
//Methode de détection "Snake"
def snake(iw:ImageWrapper,alpha:Double,beta:Double,gamma:Double,output:String){
   //var tab = Array.ofDim[Int](iw.height, iw.width)
   var source = iw.getImage()
   //var s:Array[Int]= new Array[Int](iw.height)
   val a=(gamma*(-2*alpha-6*beta)+1).toInt
   val b=(gamma*(alpha+4*beta)).toInt
   val c=(gamma*(-beta)).toInt
   var gradient=0
   var matrix= Array ofDim[Int](iw.height,iw.width)
   for (i<-2 to iw.height-3){
       matrix(i)(i)=a
       matrix(i)(i+1)=b
       matrix(i)(i-1)=b
       matrix (i)(i-2)=c
       matrix(i)(i+2)=c      
   }
   matrix(0)(iw.height-2)=a
   matrix(iw.height-1)(0)=a
   matrix(iw.height-2)(0)=c
   matrix(iw.height-1)(1)=c
   matrix(0)(iw.height-2)=c
   matrix(1)(iw.height-1)=c
   for(i<-1 to iw.height-2){
     for(j<-1 to iw.width-2){
         //Calcul du gradient pour chaque pixel
         gradient=math.abs(source(i)(j+1)-source(i)(j-1)) +
                      math.abs(source(i+1)(j)-source(i-1)(j))
     }
   }
   copy(matrix,source)
   
}
