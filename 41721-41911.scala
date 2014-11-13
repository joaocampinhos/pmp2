object Proj02 {
  
  def probA(input: String, output: String): Unit = {
    
    var x = scala.xml.XML.loadFile(input);

    var files = 0;
    var interfaces = 0;
    var classes = 0;

    var program = x \\ "java-source-program";
    for(file <- program \\ "java-class-file"){
    	files = files + 1;
    	for(interface <- file \\ "interface"){
    		interfaces = interfaces + 1;
    	}
    	for(clace <- file \\ "class"){
    		classes = classes + 1;
    	}
    }



    var html = <html>
    	<h3>{files} ficheiro(s)</h3>
    	<h3>{interfaces} interface(s)</h3>
    	<h3>{classes} classe(s)</h3>
    </html>;
    scala.xml.XML.save(output, html);
  }

  def probB(input: String, output: String): Unit = {

  }
  
  
  
    
    def main(args: Array[String]) {
      
      probA(args(0), args(1));
  }
}