object Proj02 {

  def probB(input: String, output: String): Unit = {

    var fullName = ""; //Uma solução um bocado feia
    var main = false;

    def profile( node: xml.Node) : xml.Node = {

      def sub( seq : Seq[xml.Node]) : Seq[xml.Node] =
        for( subNode <- seq ) yield profile( subNode )

      node match {
        case <java-source-program>{ el @ _* }</java-source-program> =>
          <java-source-program>{ sub(el) }</java-source-program>
        case a @ <java-class-file>{ el @ _* }</java-class-file> =>
          <java-class-file>{ sub(el) }</java-class-file>
          //Continuar com o nome do ficheiro. ficou perdido pelo caminho
        case a @ <class>{ el @ _* }</class> =>
          fullName = (a \ "@name").text
          <class>{ sub(el) }</class> //Tamos a perder cenas importantes!
        case a @ <method>{ el @ _* }</method> if (a \ "@name").text == "main" =>
          main = true
          <method>{ sub(el) }</method> //Mais cenas perdidas :(
        case a @ <method>{ el @ _* }</method> =>
          fullName += "." + (a \ "@name").text
          <method>{ sub(el) }</method>
        case <block>{ el @ _* }</block> =>
          <block>{ addProf()}{ sub(el) }</block>
        case other @ _ => other
      }
    }

    def addProf() : xml.Node = {
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
    probB(args(0), args(0).dropRight(9) + ".prof.xml");
  }
}
