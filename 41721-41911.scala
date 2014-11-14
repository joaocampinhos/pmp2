object Proj02 {
  
  def probA(input: String, output: String): Unit = {
    
    var x = scala.xml.XML.loadFile(input);

    var files = 0;
    var interfaces = 0;
    var classes = 0;


    class Interface(name: String){
    	var nome = name;
    	var superIF = List[String]();

    	def addSuperIF(name: String): Unit ={
    		superIF = name :: superIF;
    	}

    	def getName(): String ={
    		return nome;
    	}

    	def getSuperIFs(): List[String] ={
    		return superIF;
    	}
    }


    class File(name: String){
    	var nome = name;
    	var classList = List[String]();
    	var interfaceList = List[Interface]();

    	def addClass(name: String): Unit = {
    		classList = name :: classList;
    	}

    	def addInterface(name: String): Interface = {
    		var tempIF = new Interface(name);
    		interfaceList = tempIF :: interfaceList;
    		return tempIF;
    	}

    	def getName(): String = {
    		return nome;
    	}

    	def getClasses(): List[String] = {
    		return classList;
    	}

    	def getInterfaces(): List[Interface] = {
    		return interfaceList;
    	}
    }

    var fileList = List[File]();


    //isto conta o numero de ficheiros, classes e interfasses e guarda na lista
    var program = x \\ "java-source-program";
    for(file <- program \ "java-class-file"){
    	var tempFile = new File((file \ "@name").text);
    	files = files + 1;
    	fileList = tempFile :: fileList;
    	for(interface <- file \ "interface"){
    		interfaces = interfaces + 1;
    		var tempIF = tempFile.addInterface((interface \ "@name").text);
    		for(sif <- interface \ "extend"){
    			tempIF.addSuperIF((sif \ "@interface").text);
    		}

    	}
    	for(clace <- file \ "class"){
    		classes = classes + 1;
    		tempFile.addClass((clace \ "@name").text);
    	}
    }

    //println(fileList(1).getName());
    //println(fileList(1).getInterfaces()(1).getSuperIFs()(0));

	var htmlFiles = new xml.NodeBuffer;
	for(file <- fileList){
    	htmlFiles &+ <h3>ficheiro {file.getName()}</h3>;
    	for(interface <- file.getInterfaces()){
    		htmlFiles &+ <p>interface {interface.getName()}</p>;
    		for(sif <- interface.getSuperIFs()){
    			htmlFiles &+ <p>super: {sif}</p>; 
    		}
    	}
    	for(clace <- file.getClasses()){
    		htmlFiles &+ <p>class {clace}</p>;
    	}
    }


    var html = <html>
    		<h3>{files} ficheiro(s)</h3>
    		<h3>{interfaces} interface(s)</h3>
    		<h3>{classes} classe(s)</h3>
    		{htmlFiles}
    	</html>;


    scala.xml.XML.save(output, html);
  }

  def probB(input: String, output: String): Unit = {

  }
  
  
  
    
    def main(args: Array[String]) {
      
      probA(args(0), args(0).dropRight(9) + ".stats.html");
  }
}