object Proj02 {

  def probA(input: String, output: String): Unit = {

    var x = scala.xml.XML.loadFile(input);

    var files = 0;
    var interfaces = 0;
    var classes = 0;


    class Interface(name: String){
      var nome = name;
      var superIF = List[String]();
      var mHeaders = List[String]();
      var uses = List[String]();

      def addSuperIF(name: String): Unit ={
        superIF = name :: superIF;
      }

      def addMHeader(header: String): Unit ={
        mHeaders = header :: mHeaders;
      }
      def addUse(name: String): Unit ={
        uses = name :: uses;
      }

      def getName(): String ={
        return nome;
      }

      def getSuperIFs(): List[String] ={
        return superIF;
      }

      def getMHeaders(): List[String] ={
        return mHeaders;
      }

      def getUses(): List[String] ={
        return uses;
      }
    }

    class Clace(name: String){
      var nome = name;
      var superC = List[String]();
      var implements = List[String]();
      var mHeaders = List[String]();
      var uses = List[String]();

      def addSuperC(name: String): Unit ={
        superC = name :: superC;
      }

      def addImplements(name: String): Unit ={
        implements = name :: implements;
      }

      def addMHeader(header: String): Unit ={
        mHeaders = header :: mHeaders;
      }

      def addUse(name: String): Unit ={
        uses = name :: uses;
      }

      def getName(): String ={
        return nome;
      }

      def getSuperC(): List[String] ={
        return superC;
      }

      def getImplements(): List[String] ={
        return implements;
      }

      def getMHeaders(): List[String] ={
        return mHeaders;
      }

      def getUses(): List[String] ={
        return uses;
      }
    }


    class File(name: String){
      var nome = name;
      var classList = List[Clace]();
      var interfaceList = List[Interface]();

      def addClass(name: String): Clace = {
        var tempC = new Clace(name);
        classList = tempC :: classList;
        return tempC;
      }

      def addInterface(name: String): Interface = {
        var tempIF = new Interface(name);
        interfaceList = tempIF :: interfaceList;
        return tempIF;
      }

      def getName(): String ={
        return nome;
      }

      def getClasses(): List[Clace] = {
        return classList;
      }

      def getInterfaces(): List[Interface] = {
        return interfaceList;
      }
    }

    var fileList = List[File]();


    //bloco que extrai a informação do ficheiro xml e guarda
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
        for(tipe <- interface \\ "type"){
          if((tipe \ "@primitive").text != "true" &&
            ((tipe \ "@name").text).startsWith("java")==false)
              if(tempIF.getUses().contains((tipe \ "@name").text) == false)
                tempIF.addUse((tipe \ "@name").text);
        }
        for(method <- interface \ "method"){
          var visibility = method \ "@visibility";
          var static = method \ "@static";
          var finale = method \ "@final";
          var sync = method \ "@synchronized"
          var tipe = (method \ "type") \ "@name";
          var name = method \ "@name";
          var arguments = "(";
          for(argument <- (method \ "formal-arguments") \ "formal-argument"){
            arguments = arguments + ((argument \ "type") \ "@name") +
            " "+ (argument \ "@name") +"  ";
          }
          arguments = arguments + ")";
          var throws= "";
          for(t <- method \ "throws"){
            throws = throws + " " + t \ "@exception";
          }
          var header = visibility.text;
          if(static.text == "true")
            header = header +" static";
          if(finale.text == "true")
            header = header + " final";
          if(sync.text == "true")
            header = header + " synchronized";
          header = header +" "+ tipe.text +" "+ name.text + arguments;
        if(throws != "")
          header = header +" throws"+ throws;
        tempIF.addMHeader(header);
        }

      }
      for(clace <- file \ "class"){
        classes = classes + 1;
        var tempC = tempFile.addClass((clace \ "@name").text);
        for(sc <- clace \ "superclass"){
          tempC.addSuperC((sc \ "@name").text);
        }
        for(ip <- clace \ "implement"){
          tempC.addImplements((ip \ "@interface").text);
        }
        for(tipe <- clace \\ "type"){
          if((tipe \ "@primitive").text != "true" &&
            ((tipe \ "@name").text).startsWith("java")==false)
              if(tempC.getUses().contains((tipe \ "@name").text) == false)
                tempC.addUse((tipe \ "@name").text);
        }
        for(method <- clace \ "method"){
          var visibility = method \ "@visibility";
          var static = method \ "@static";
          var tipe = (method \ "type") \ "@name";
          var name = method \ "@name";
          var arguments = "(";
          for(argument <- (method \ "formal-arguments") \ "formal-argument"){
            arguments = arguments + ((argument \ "type") \ "@name") +
            " "+ (argument \ "@name") +"  ";
          }
          arguments = arguments + ")";
          var throws= "";
          for(t <- method \ "throws"){
            throws = throws + " " + t \ "@exception";
          }
          var header = visibility.text;
          if(static.text == "true")
            header = header +" static";
          header = header +" "+ tipe.text +" "+ name.text + arguments;
        if(throws != "")
          header = header +" throws"+ throws;
        tempC.addMHeader(header);
        }
      }
    }

    //bloco para construir o ficheiro html
    var htmlFiles = new xml.NodeBuffer;
    for(file <- fileList){
      htmlFiles &+ <h2>File {file.getName()}</h2>;
      for(interface <- file.getInterfaces()){
        htmlFiles &+ <h3>Interface {interface.getName()}</h3>;
        for(sif <- interface.getSuperIFs()){
          htmlFiles &+ <p>Super: {sif}</p>; 
        }
        htmlFiles &+ <span>Uses: </span>
        for(use <- interface.getUses()){
          htmlFiles &+ <span>{use} </span>;
        }
        htmlFiles &+ <h4>Methods:</h4>;
        for(header <- interface.getMHeaders()){
          htmlFiles &+ <p>{header}</p>;
        }
        htmlFiles &+ <hr/>;
      }
      for(clace <- file.getClasses()){
        htmlFiles &+ <h3>Class {clace.getName()}</h3>;
        if(clace.getSuperC().size == 0)
          htmlFiles &+ <p>Super: java.lang.Object</p>;
        else
          for(sc <- clace.getSuperC()){
            htmlFiles &+ <p>Super: {sc}</p>;
          }
          htmlFiles &+ <span>Implements: </span>;
          for(ip <- clace.getImplements()){
            htmlFiles &+ <span>{ip} </span>;
          }

          htmlFiles &+ <p> </p><span>Uses: </span>;
          for(use <- clace.getUses()){
            htmlFiles &+ <span>{use} </span>;
          }
          htmlFiles &+ <h4>Methods:</h4>;
          for(header <- clace.getMHeaders()){
            htmlFiles &+ <p>{header}</p>;
          }
          htmlFiles &+ <hr/>;
      }
      htmlFiles &+ <hr/>;
    }


    var html = <html>
    <h1>Stats for {input}</h1>
    <p>{files} ficheiro(s)</p>
    <p>{interfaces} interface(s)</p>
    <p>{classes} classe(s)</p>
    {htmlFiles}
  </html>;


  scala.xml.XML.save(output, html);
  }

  def probB(input: String, output: String): Unit = {

    var cName = "";
    var mName = "";
    var main = false;

    def profile( node: xml.Node) : xml.Node = {

      def sub( seq : Seq[xml.Node]) : Seq[xml.Node] =
        for( subNode <- seq ) yield profile( subNode )

      node match {
        case <java-source-program>{ el @ _* }</java-source-program> =>
          <java-source-program>{ sub(el) }</java-source-program>
        case a @ <java-class-file>{ el @ _* }</java-class-file> =>
          <java-class-file >{ sub(el) }</java-class-file>
          /*val _el = <java-class-file >{ sub(el) }</java-class-file>
            addAttrs(_el, a.attributes.asAttrMap)*/
        case a @ <class>{ el @ _* }</class> =>
          cName = (a \ "@name").text
          <class>{ sub(el) }</class>
          /*val _el = <class>{ sub(el) }</class>
            addAttrs(_el, a.attributes.asAttrMap)*/
        case a @ <method>{ el @ _* }</method> if (a \ "@name").text == "main" =>
          main = true
          <method>{ sub(el) }</method>
          /*val _el = <method>{ sub(el) }</method>
            addAttrs(_el, a.attributes.asAttrMap)*/
        case a @ <method>{ el @ _* }</method> =>
          mName = (a \ "@name").text
          <method>{ sub(el) }</method>
          /*val _el = <method>{ sub(el) }</method>
            addAttrs(_el, a.attributes.asAttrMap)*/
        case <block>{ el @ _* }</block> =>
          <block>{ addProf()}{ sub(el) }</block>
        case other @ _ => other
      }
    }

    /*
    Não está terminado
    def addAttrs(node: xml.Node, attrs: Map[String,String]) : xml.Node = {
      attrs match {
        case x if x.isEmpty => node
        case x =>
          val (key, value) = x.head
          addAttrs(node % Attribute(None, key, value, Null), attrs.tail)
      }
    }
    */

    def addProf() : xml.Node = {
      var fullName = cName+"."+mName;
      if (main)
        <send message="list">
          <target>
            <var-ref name="CentralCounting"/>
          </target>
          <arguments/>
        </send>
      else
        <send message="count">
          <target>
            <var-ref name="CentralCounting"/>
          </target>
          <arguments>
            <literal-string value={fullName}/>
          </arguments>
        </send>
    }

    var x = scala.xml.XML.loadFile(input);
    scala.xml.XML.save(output, profile(x));
  }


  def main(args: Array[String]) {

    probA(args(0), args(0).dropRight(9) + ".stats.html");
    probB(args(0), args(0).dropRight(9) + ".prof.xml");

  }
}
