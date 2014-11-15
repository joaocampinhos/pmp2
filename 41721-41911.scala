object Proj02 {

  def probB(input: String, output: String): Unit = {

    var cName = ""; //Esta solução é um bocado feia não?
    var mName = "";

    def profile( node: xml.Node) : xml.Node = {

      def sub( seq : Seq[xml.Node]) : Seq[xml.Node] =
        for( subNode <- seq ) yield profile( subNode )

      node match {
        case <java-source-program>{ el @ _* }</java-source-program> =>
          <java-source-program>{ sub(el) }</java-source-program>
        case a @ <java-class-file>{ el @ _* }</java-class-file> =>
          println(a \ "_")
          <java-class-file>{ sub(el) }</java-class-file> //Continuar com o nome do ficheiro. ficou perdido pelo caminho
        case a @ <class>{ el @ _* }</class> =>
          cName = (a \ "@name").text
          <class>{ sub(el) }</class> //Tamos a perder cenas importantes!
        case a @ <method>{ el @ _* }</method> if (a \ "@name").text == "main" =>
          <method>{ sub(el) }</method>
        case a @ <method>{ el @ _* }</method> =>
          mName = (a \ "@name").text
          <method>{ sub(el) }</method>
        case <block>{ el @ _* }</block> =>
          <block>{ addProf()}{ sub(el) }</block>
        case other @ _ => other
      }
    }

    def addProf() : xml.Node = {
      if (mName == "")
        <send message="list">
          <target>
            CentralCounting
          </target>
          <arguments/>
        </send>
      else
        <send message="count">
          <target>
            CentralCounting
          </target>
          <arguments>
            String -> {cName+"."+mName}
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
