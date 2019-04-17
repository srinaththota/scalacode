

object Main {
case class Key(freq:Int,ch:Char) extends Ordered[Key]{
  
  def compare(that:Key)=freq.compare(that.freq)
}

  def main(args:Array[String]){
    
    val str="AAAAAAAAKKKKMMMT".toLowerCase();
		var value:Int=0;
			reArrangeChars(str,1);
		while(value<str.length()){
		reArrangeChars(str,value);
		value +=1;
		}
  }
  
 def reArrangeChars(str:String, value:Int){
   
   
   val len:Int=str.length();
   
   
   val count:Array[Int]=new Array[Int](26);
   
   
		for(i<-0 to len-1){
		  
		  
			count(str.charAt(i)-'a')+=1;
		}
		 
		
val pq=new scala.collection.mutable.PriorityQueue[Key];

    
	for(c<-'a' until 'z'){
			 val chars=c-'a';

			if(count(chars)>0){
			
			  pq.enqueue(new Key(count(chars),c))
				
			}
		}
		
		
		
		val resArr:Array[Char]=new Array[Char](str.length);
		val pqSize=pq.size
		

		var emptyPositions=new java.util.ArrayList[Int]();

		var filledPositions=new java.util.ArrayList[Int]();

		var notFilledPositions=new java.util.ArrayList[Int]();
		var emptyPos:Int=0;
		
		while(emptyPos<str.length()){
		  
		  emptyPositions.add(emptyPos)
			emptyPos+=1;
		}

		 var listIndex:Int=0;
		  for(i<-0 until pqSize){
		    val key=pq.dequeue();  
		  
		    emptyPositions.removeAll(filledPositions);
			if(emptyPositions.size() < str.length()){
				notFilledPositions.removeAll(filledPositions);
				emptyPositions=notFilledPositions;
				notFilledPositions=new java.util.ArrayList[Int]();		
				listIndex=0;
			}else{
				listIndex=value;
			}
			
			filledPositions=new java.util.ArrayList[Int]();
			
				for( k<-0 until key.freq){

				if(listIndex>=emptyPositions.size()){
					listIndex=listIndex-emptyPositions.size();
				}
				/*if(listIndex>emptyPositions.size()){
					listIndex=listIndex-emptyPositions.size()+1;
				}*/
				var pos=emptyPositions.get(listIndex);
				while(filledPositions.contains(pos)){
					listIndex+=1;
					pos=emptyPositions.get(listIndex);
				}

				resArr(pos)=key.ch;
				filledPositions.add(pos);
				listIndex+=1;
				if(listIndex >=emptyPositions.size()){
					notFilledPositions.add(listIndex-emptyPositions.size());
				}else{
					notFilledPositions.add(emptyPositions.get(listIndex));
				}
				listIndex+=1;

			}
		}
		  println(String.valueOf(resArr))

 }
 
}

