object Proj02 {

  def probB(input: String, output: String): Unit = {

    var cName = ""; //Esta solução é um bocado feia não?

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
          <method>{ addList(a) }</method>
        case a @ <method>{ el @ _* }</method> =>
          var fullname = cName+"."+(a \ "@name").text
          addCount(a, fullname);
        case other @ _ => other
      }
    }

    def addCount( seq: Seq[xml.Node], name: String) : xml.Node = {
      seq match {
        case <block>{ el @ _* }</block> => <block>
                                             {el}
                                             <send message="count">
                                               <target>
                                                 CentralCounting
                                               </target>
                                               <arguments>
                                                 String -> {name}
                                               </arguments>
                                             </send>
                                           </block>
        case other @ _ => addCount(other, name)
      }
    }

    def addList( seq: Seq[xml.Node]) : xml.Node = {
      <send message="list">
        <target>
          CentralCounting
        </target>
        <arguments/>
      </send>
    }

    var x = scala.xml.XML.loadFile(input);
    scala.xml.XML.save(output, profile(x));
  }

  def main(args: Array[String]) {
    probB(args(0), args(0).dropRight(9) + ".prof.xml");
  }
}
