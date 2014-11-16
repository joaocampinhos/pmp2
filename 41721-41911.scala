object Proj02 {

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

    /* Não está terminado
    def addAttrs(node: xml.Node, attrs: Map[String,String]) : xml.Node = {
     attrs match {
       case x if x.isEmpty => node
       case x =>
         val (key, value) = x.head
         addAttrs(node % Attribute(None, key, value, Null), attrs.tail)
     }
    }*/

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
    probB(args(0), args(0).dropRight(9) + ".prof.xml");
  }
}
