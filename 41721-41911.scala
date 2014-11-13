object Proj02 {
  
  def probA(input: String, output: String): Unit = {
    
    var x = scala.xml.XML.loadFile(input);

    var files = 0;
    var interfaces = 0;
    var classes = 0;


    class File(name: String){
    	var nome = name;
    	var classList = List[String]();
    	var interfaceList = List[String]();

    	def addClass(name: String): Unit = {
    		classList = name :: classList;
    	}

    	def addInterface(name: String): Unit = {
    		interfaceList = name :: interfaceList;
    	}

    	def getName(): String = {
    		return nome;
    	}

    	def getClasses(): List[String] = {
    		return classList;
    	}

    	def getInterfaces(): List[String] = {
    		return interfaceList;
    	}
    }

    var fileList = List[File]();


    //isto conta o numero de ficheiros, classes e interfasses e guarda na lista
    var program = x \\ "java-source-program";
    for(file <- program \ "java-class-file"){
    	files = files + 1;
    	fileList = (new File((file \ "@name").text)) :: fileList;
    	for(interface <- file \ "interface"){
    		interfaces = interfaces + 1;
    		fileList(files-1).addInterface((interface \ "@name").text);
    	}
    	for(clace <- file \ "class"){
    		classes = classes + 1;
    		fileList(files-1).addClass((clace \ "@name").text);
    	}
    }


    //testar se ta a guardar bem
    /*for(file <- fileList){
    	println(file.getName() + "::");
    	for(interface <- file.getInterfaces()){
    		println(interface);
    	}
    	for(clace <- file.getClasses()){
    		println(clace);
    	}
    }*/


    

    //tentativa de fazer update ao html com o que está guardado nas listas
    /*def addFile(files: scala.xml.NodeSeq, name: String) = {
    files match {
        case <files>{ l @ _* }</files> =>
            <files>
                 { l }
                 <h3>{name}</h3>
            </files>
   }
}*/



    var html = <html>
    	<h3>{files} ficheiro(s)</h3>
    	<h3>{interfaces} interface(s)</h3>
    	<h3>{classes} classe(s)</h3>
    	<files>
    	</files>
    </html>;

    scala.xml.XML.save(output, html);
  }

  def probB(input: String, output: String): Unit = {

  }
  
  
  
    
    def main(args: Array[String]) {
      
      probA(args(0), args(1));
  }
}