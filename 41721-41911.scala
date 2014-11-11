object t2 {
  
  def probA(input: String, output: String): Unit = {
    
    val x = scala.xml.XML.loadFile(input + ".xml");
    println(x);
  }
  
  
  
    
    def main(args: Array[String]) {
      
      probA(args(0), args(1));
  }
}