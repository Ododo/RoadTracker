package duda

import com.tncy.top.image.ImageWrapper
import colors.values._

object Duda {
   
    type pixseq = Tuple2[Tuple2[Int,Int],
                         Tuple2[Int,Int]]
    
    val dudaThreshold = 0.167f
    
    /**
     * Détermine la valeur du contraste à appliquer
     * en fonction de la différence de valeur 
     * entre deux pixels
     */
    def contrast(diff:Int):Float={
        if (diff==0)
          return 1 
        else 
          return 0.166f
    }
    
    /**
     * Calcul la somme des contrastes pour les paires
     * de pixels données
     */
    def doSum(src:Array[Array[Int]], l:List[pixseq]):Float = {
        return 1/l.map{x:pixseq => 
               contrast(src(x._1._1)(x._1._2)-src(x._2._1)(x._2._2))}.sum
    }
    
    /**
     * Applique le DRO sur l'image
     */
    def process(iw:ImageWrapper):Array[Array[Int]] = {
        var sch = 0f
        var scv = 0f
        var scd1 = 0f
        var scd2 = 0f
        
        var src = iw.getImage()
        var output = Array.ofDim[Int](iw.height, iw.width)
        var scores = Array.ofDim[Float](iw.height,iw.width)
        
        for (i<-2 to iw.height-3){
        for (j<-2 to iw.width-3){
          
        sch=doSum(src, List(((i,j-1),(i-2,j-1)),((i,j),(i-2,j)),((i,j+1),(i-2,j+1)),
             ((i,j-1),(i+2,j-1)), ((i,j),(i+2,j)),((i,j+1),(i+2,j+1))))
               
        scv=doSum(src,List(((i+1,j),(i+1,j-2)),((i,j),(i,j-2)),((i-1,j),(i-1,j-2)),
             ((i+1,j),(i+1,j+2)), ((i,j),(i,j+2)),((i-1,j),(i-1,j+2))))
         
        scd1=doSum(src, List(((i+1,j-1),(i-1,j-2)),((i,j),(i-2,j-2)),((i-1,j+1),(i-2,j-1)),
             ((i+1,j-1),(i+2,j+1)), ((i,j),(i+2,j+2)),((i-1,j+1),(i+1,j+2))))
                     
        scd2=doSum(src, List(((i+1,j+1),(i+2,j-1)),((i,j),(i+2,j-2)),((i-1,j-1),(i+1,j-2)),
             ((i+1,j+1),(i-1,j+2)), ((i,j),(i-2,j+2)),((i-1,j-1),(i-2,j+1))))
             
           
        scores(i)(j)=(sch+scv+scd1+scd2)/4    
        }}
         
      for (i<-0 to iw.height-1){
      for (j<-0 to iw.width-1){
        
        if (scores(i)(j) > dudaThreshold)
          output(i)(j)=WHITE
        else 
          output(i)(j)=BLACK
      }}      
      
      return output
    }
}