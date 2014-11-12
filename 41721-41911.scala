object Proj02 {
  
  def probA(input: String, output: String): Unit = {
    
    val x = scala.xml.XML.loadFile(input);

    val filesN = (x \ "java-class-file").size;



    val html = <html><h2>{filesN} ficheiro(s)</h2></html>;
    scala.xml.XML.save(output, html);
  }

  def probB(input: String, output: String): Unit = {

  }
  
  
  
    
    def main(args: Array[String]) {
      
      probA(args(0), args(1));
  }
}