object t2 {
  
  def probA(input: String, output: String): Unit = {
    
    val x = scala.xml.XML.loadFile(input);
    println(x.text);
  }
  
  
  
    
    def main(args: Array[String]) {
      
      //println(args(0) + " ; " + args(1));
      probA(args(0), args(1));
      //val foo = <foo><bar type="greet">hi</bar><bar type="count">1</bar><bar type="color">yellow</bar></foo>
      //println(foo.text);
  }
}