import java.io.{FileNotFoundException, PrintWriter, File}



import scala.io.Source
import scala.util.parsing.json.JSON

/**
 *
 * User: molikto
 * Date: 12/24/14
 * Time: 18:36
 */

// TODO(molikto) merge imports from extra code

object Utils {

  def typeRefName(t: JsonType) = t match {
    case ObjectType(name, _, _) => name
    case a => a.toString
  }

  var _counter = 0
  def newName = {
    _counter += 1
    "new_var_" + _counter
  }
}

object MagicNumbers {
  val INT_NULL = Integer.MAX_VALUE - 4223;
}

abstract class JsonType {
  def typeName: String

}
case object IntType extends JsonType {
  override def typeName = "int"
}
case object LongType extends JsonType {
  override def typeName = "long"
}


case object FloatType extends JsonType {
  override def typeName = "float"
}

case object DoubleType extends JsonType {
  override def typeName = "double"
}
case object StringType extends JsonType {
  override def typeName = "String"
}

case class EnumType(clzName: String, vals: Seq[String]) extends JsonType {
  override def typeName = clzName
}

case object UnknownType extends JsonType {
  override def typeName = "Object"
}
case object BooleanType extends JsonType {
  override def typeName = "boolean"
}


case object BoxedBooleanType extends JsonType {
  override def typeName = "Boolean"
}

case object BoxedIntType extends JsonType {
  override def typeName = "Integer"
}

case class Field(jsonFieldName: String, fieldName: String, var fieldType: JsonType, nullable: Boolean = true, serializable: Boolean = true) {

  var _noDiff = false
  def noDiff(): Field = {
    _noDiff = true
    this
  }

  def convertedFieldType = fieldType match {
    case ConvertedType(_, c, _, _) => c
    case a => a
  }

  def originalFieldType = fieldType match {
    case ConvertedType(o, _, _, _) => o
    case a => a
  }

  override def toString: String = {
    s"""Field("${jsonFieldName}", "${fieldName}", ${Utils.typeRefName(fieldType)}${if (nullable) "" else ", false"})"""
  }
}
case class ObjectType(name: String, fields: Seq[Field], genereateDiff: Boolean = false) extends JsonType {
  var _extends:String = null
  def extends_(s: String): ObjectType = {
    _extends = s
    this
  }

  override def typeName = name
  override def toString: String = s"""ObjectType("${name}", Seq(\n    ${fields.mkString(",\n    ")}))"""
}
case class ArrayType(itemType: JsonType, fixLength: Int = -1) extends JsonType {
  override def typeName = if (fixLength != -1) "/* fixed length " + fixLength + " */" + itemType.typeName + "[]" else "List<" + itemType.typeName + ">"
  override def toString: String = s"""ArrayType(${Utils.typeRefName(itemType)}${if (fixLength != -1) ", true" else ""})"""
}


case class MapType(itemType: JsonType) extends JsonType {
  override def typeName = "java.util.LinkedHashMap<String," + itemType.typeName + ">"
  override def toString: String =  s"""MapType(${Utils.typeRefName(itemType)})"""
}

case class JavaObjectType(name: String) extends JsonType {
  override def typeName: String = name
}

case class ConvertedType(originalType: JsonType, convertedType: JsonType, convertFunctionName: String, revertFunctionName: String = null) extends JsonType {
  override def typeName: String = convertedType.typeName
}

val EXTRA_CODE_START = "/* EXTRA CODE START */"
val EXTRA_CODE_MARK = "/* EXTRA CODE MARK */"
val EXTRA_CODE_END = "/* EXTRA CODE END */"

object ClassFile {
}
case class ClassFile(packageName: String, className: String, var content: String) {
  def writeTo(dir: File) = {
    val des = new File(dir, className + ".java")
    des.mkdirs()
    if (des.isFile) {
      var extra = ""
      var started = false
      for (line <- io.Source.fromFile(des).getLines()) {
        if (line.contains(EXTRA_CODE_START)) {
          started = true
        } else if (line.contains(EXTRA_CODE_END)) {
          started = false
        } else if (started) {
          extra = extra + line + "\n"
        }
      }
      if (!started) {
        extra =  EXTRA_CODE_START + "\n" + extra + EXTRA_CODE_END
        content = content.replace(EXTRA_CODE_MARK, extra)
      }
    }
    des.delete()
    des.createNewFile()
    val writer = new PrintWriter(des)
    writer.write(toString)
    writer.close()
  }

  override def toString: String =
    s"""
       |package ${packageName};
       |
       | import com.fasterxml.jackson.core.JsonGenerator;
       | import com.fasterxml.jackson.core.JsonParser;
       | import com.fasterxml.jackson.core.JsonToken;
       | import com.fasterxml.jackson.core.JsonParseException;
       |
       | import android.support.annotation.NonNull;
       |
       | import java.io.IOException;
       | import java.io.InputStream;
       | import java.io.Serializable;
       | import java.io.StringWriter;
       | import java.util.ArrayList;
       | import java.util.Collections;
       | import java.util.List;
       | import java.util.concurrent.atomic.AtomicLong;
       | import java.util.regex.Pattern;
       |
       | ${content}
     """.stripMargin
}

object GenerateContent extends (Spec => Seq[ClassFile]) {

  def JsonFactory(s: Spec) = ClassFile(s.packageName, "JsonFactoryHolder",
    s"""
       | import com.fasterxml.jackson.core.JsonFactory;
       |
       | public class JsonFactoryHolder {
       |    public static JsonFactory APP_FACTORY = new JsonFactory();
       | }
     """.stripMargin)


  def StreamParser(s: Spec) = ClassFile(s.packageName, "StreamParser",
    s"""
       | import com.fasterxml.jackson.core.JsonParseException;
       | import java.io.InputStream;
       | public interface StreamParser<T> {
       |   public T parse(InputStream in) throws IOException, JsonParseException;
       | }
     """.stripMargin)

  override def apply(v1: Spec): Seq[ClassFile] = v1.classSpecs.map(c => {
    Utils._counter = 0 // reset it so it is more stable, we do not got too many merge conflicts
    singleClass(v1, c)
  }) ++ Seq(JsonFactory(v1), StreamParser(v1)) ++ v1.enumSpecs.map(c => singleEnum(v1, c))


  def singleEnum(s: Spec, t: EnumType): ClassFile = ClassFile(s.packageName, t.clzName, {

    val JavaKeywords = Seq("default")
    def declear_item(s: String): String = {
      if (JavaKeywords.contains(s))
        s"""${s}_ { @Override public String toString(){ return "${s}";}}"""
      else if (s.contains('-'))  s"""${s.replace('-', '_')} { @Override public String toString(){ return "${s}";}}"""
      else s
    }
    val _override_value_if_necessary = {
      val keywords = JavaKeywords.toSet.intersect(t.vals.toSet)
      keywords.map(k =>
        s"""        if (${k}_.toString().equals(value)) return ${k}_;
         """.stripMargin).mkString ++  {
        t.vals.filter(_.contains('-')).map(k =>
          s"""        if (${k.replace('-', '_')}.toString().equals(value)) return ${k.replace('-', '_')};
         """.stripMargin).mkString
      }
    }
    s"""
     |public enum ${t.clzName} {
     |   ${t.vals.map(declear_item).mkString(", ")};
     |   public static ${t.clzName} fromString(String value) {
     |     ${_override_value_if_necessary}
     |     return valueOf(value);
     |   }
     |}
   """.stripMargin
  }
  )

  def extractValue(spec: Spec, t: JsonType): (String, String) = {
    val pre_extract_value = t match {
      case ConvertedType(o, c, converter, reverter) =>
        val item_extract_value = extractValue(spec, o)
        item_extract_value._1
      case MapType(it) =>
        val item_extract_value = extractValue(spec, it)
        s"""
      java.util.LinkedHashMap<String, ${it.typeName}> mapValue = new java.util.LinkedHashMap<String, ${it.typeName}>();
    if (jp.getCurrentToken() != JsonToken.START_OBJECT) {
      jp.skipChildren();
    } else {
    while (jp.nextToken() != JsonToken.END_OBJECT) {
      String fieldName1 = jp.getCurrentName();
      jp.nextToken();
        ${item_extract_value._1}
        mapValue.put(fieldName1, ${item_extract_value._2});
      jp.skipChildren();
    }
    }

         """.stripMargin
      case ArrayType(it, length) =>
        val item_extract_value = extractValue(spec, it)
        val assgin_value_not_null = if (length >= 0) {
          "arrayValue[i] = parsed;"
        } else {
          "arrayValue.add(parsed);"
        }
        val assgin_value = it match {
          case IntType | BooleanType | LongType | FloatType =>
            assgin_value_not_null
          case _ =>
            s"""
              |           if (parsed != null) {
              |             ${assgin_value_not_null}
              |           }
            """.stripMargin
        }
        if (length >= 0) {
          s"""
                 |       ${it.typeName}[] arrayValue = null;
                 |       int i = 0;
                 |       if (jp.getCurrentToken() == JsonToken.START_ARRAY) {
                 |         arrayValue = new ${it.typeName}[${length}];
                 |         while (jp.nextToken() != JsonToken.END_ARRAY) {
                 |           ${item_extract_value._1}
                 |           ${it.typeName} parsed = ${item_extract_value._2};
                 |           ${assgin_value}
                 |           i++;
                 |         }
                 |       }
           """.stripMargin
        } else {
          s"""
                 |       List<${it.typeName}> arrayValue = null;
                 |       if (jp.getCurrentToken() == JsonToken.START_ARRAY) {
                 |         arrayValue = new ArrayList<${it.typeName}>();
                 |         while (jp.nextToken() != JsonToken.END_ARRAY) {
                 |           ${item_extract_value._1}
                 |           ${it.typeName} parsed = ${item_extract_value._2};
                 |           ${assgin_value}
                 |         }
                 |
                 |       if (arrayValue.size() == 0) {
                 |          arrayValue = Collections.emptyList();
                 |       }
                 |       }
               """.stripMargin
        }
      case _ => ""
    }
    val extract_value = t match {
      case StringType => "(jp.getCurrentToken() == JsonToken.VALUE_NULL ? null : jp.getText())"
      case EnumType(c, vals) => s"(jp.getCurrentToken() == JsonToken.VALUE_NULL ? null : ${c}.fromString((jp.getText())))"
      case BooleanType => "jp.getValueAsBoolean()"
      case BoxedBooleanType => "jp.getValueAsBoolean()"
      case IntType => "jp.getValueAsInt()"
      case FloatType => "((float) jp.getValueAsDouble())"
      case BoxedIntType => "jp.getValueAsInt()"
      case ObjectType(a, _, _) => a + ".parse(jp)"
      case ArrayType(t, length) => "arrayValue"
      case MapType(t) => "mapValue"
      case ConvertedType(o, c, converter, _) =>
        val item_extract_value = extractValue(spec, o)
        s"${spec.converterClassName}.${converter}(${item_extract_value._2});"
      case a  =>
        throw new Exception(a.toString + " cannot be extracted")
    }
    (pre_extract_value, extract_value)
  }



  def singleClass(spec: Spec, t: ObjectType): ClassFile = t match {
    case ObjectType(name, fields, genereateDiff) =>
      val dataFields = fields.map(field => {
        s"${if (!field.nullable) "@NonNull" else ""} public  ${field.fieldType.typeName} ${field.fieldName};"
      }).mkString("\n")

//      def copy_field_val(fieldType: JsonType, fromVal: String): (String, String) = fieldType match {
//        case IntType | StringType | DoubleType | LongType | BooleanType => ("", s"${fromVal}")
//        case MapType(it) =>
//          it match {
//            case ArrayType(ait, n) =>
//              if (n < 0) {
//                ait match {
//                  case StringType =>
//
//                }
//              } else {
//                throw new Exception("fdsa")
//              }
//          }
//        case ConvertedType(o, c, fc, fr) =>
//          val name1 = Utils.newName
//          s"""
//             | ${o.typeName}
//           """.stripMargin
//          val c = copy_field_val(o, )
//        case ArrayType(it, n) =>
//          val nname = Utils.newName
//          (if (n >= 0) {
//          s"""
//             |${it.typeName}[] ${nname} =null;
//             |if (${fromVal} != null) {
//             | ${nname} = new ${it.typeName}[${n}];
//             | for (int i = 0; i < ${n}; i++) {
//             |   ${copy_field_val(it, fromVal + "[i]")}
//             |   ${nname}[i] = ${copy_field_val(it, fromVal + "[i]")._2};
//             | }
//             | }
//           """.stripMargin
//        } else {
//            s"""
//             |ArrayList<${it.typeName}> ${nname} = null;
//             |if (${fromVal} != null) {
//             | ${nname} = new ArrayList<${it.typeName}>(${n});
//             | for (int i = 0; i < ${n}; i++) {
//             |   ${copy_field_val(it, fromVal + ".get(i)")}
//             |   ${nname}.add(${copy_field_val(it, fromVal + ".get(i)")._2});
//             | }
//             | }
//           """.stripMargin
//        }, nname)
//      }
      def if_jsonField_equals_fieldName_then_parse_field(field: Field): String = {
        val oType = field.originalFieldType
        val (pre_extract_value, extract_value) = extractValue(spec, field.originalFieldType)
        val assgin_value = field.fieldType match {
          case ConvertedType(o, c, converter, _) =>
            s"""
               |     ${oType.typeName} originalValue = ${extract_value};
               |     instance.${field.fieldName} = ${spec.converterClassName}.${converter}(originalValue);
             """.stripMargin
          case _ => s"     instance.${field.fieldName} = " + extract_value + ";"
        }
        s"""
           | if ("${field.jsonFieldName}".equals(fieldName)) {
           |  ${pre_extract_value}
           |  ${assgin_value}
           |  return true;
           | }
         """stripMargin
      }


      def write_if_not_null(t: JsonType, value: String, newVar: Boolean = false): String = {

        val newName = Utils.newName
        val varName = if (newVar) newName else value
        val newValString = if (newVar) t.typeName + " " + newName + " = " + value + ";\n" else ""
        t match {
          case StringType =>
            s"""
               | ${newValString}
               | if (${varName} != null) {
               |   generator.writeString(${varName});
               | }
             """.stripMargin
          case EnumType(c, vals) =>
            s"""
               | ${newValString}
               | if (${varName} != null) {
               |   generator.writeString(${varName}.toString());
               | }
             """.stripMargin

          case ObjectType(_, _, _) =>
            s"""
               |
               | ${newValString}
               | if (${varName} != null) {
               |   ${varName}.serialize(generator, true);
               | }
             """.stripMargin
          case FloatType =>
            s"generator.writeNumber(${value});"
          case IntType | BoxedIntType=>
            s"generator.writeNumber(${value});"
          case BooleanType |BoxedBooleanType =>
            s"generator.writeBoolean(${value});"
          case ConvertedType(o, c, _, reverter) => Option(reverter) match {

            case None => "/* reverter not specificed, value cannot be written */"
            case Some(a) => write_if_not_null(o, s"${spec.converterClassName}.${reverter}(${value})", true)
          }
        }
      }


      def write_field_if_not_null(fieldName: String, value: String, t: JsonType, newVar: Boolean = false): String = {

        val newName = Utils.newName
        val varName = if (newVar) newName else value
        val newValString = if (newVar) t.typeName + " " + newName + " = " + value + ";\n" else ""
        t match {
          case StringType =>
            s"""
               | ${newValString}
               | if (${varName} != null) {
               |   generator.writeStringField("${fieldName}", ${varName});
               | }
             """.stripMargin

          case EnumType(c, vals)=>
            s"""
               | ${newValString}
               | if (${varName} != null) {
               |   generator.writeStringField("${fieldName}", ${varName}.toString());
               | }
             """.stripMargin
          case FloatType =>
            s""" generator.writeNumberField("${fieldName}", ${value});"""
          case IntType =>
            s""" generator.writeNumberField("${fieldName}", ${value});"""
          case BoxedIntType =>
            s"""
               | if (${varName} != null) {
               |    generator.writeNumberField("${fieldName}", ${value});
               | }
             """.stripMargin
          case BooleanType =>
            s"""    generator.writeBooleanField("${fieldName}", ${value});"""
          case BoxedBooleanType =>
            s"""
               | if (${varName} != null) {
               |    generator.writeBooleanField("${fieldName}", ${value});
               | }
             """.stripMargin
          case t: ObjectType =>
            s"""
               | ${newValString}
               | if (${varName} != null) {
               |    generator.writeFieldName("${fieldName}");
               |    ${varName}.serialize(generator, true);
               | }
             """.stripMargin
          case MapType(it) =>
            s"""
                // TODO not implemented yet
             """.stripMargin
          case ArrayType(it, length) =>
            s"""
                 |
                 | ${newValString}
                 | if (${varName} != null) {
                 |   generator.writeFieldName("${fieldName}");
                 |   generator.writeStartArray();
                 |   for (${it.typeName} element : ${varName}) {
                 |     ${write_if_not_null(it, "element")}
                 |   }
                 |   generator.writeEndArray();
                 | }
               """.stripMargin
          case ConvertedType(o, c, _, reverter) => Option(reverter) match {
            case None => "/* not writing because reverter is not specified */"
            case Some(a) => write_field_if_not_null(fieldName, s"${spec.converterClassName}.${reverter}(${value})", o, true)
          }
        }
      }


      def __field_equals(f: Field): String = {
        f.convertedFieldType match {
          case IntType | BooleanType | LongType | FloatType =>
            s"${f.fieldName} == o.${f.fieldName}"
          case _ =>
            s"(${f.fieldName} == null && o.${f.fieldName} == null) || (${f.fieldName} !=null && ${f.fieldName}.equals(o.${f.fieldName})) "
        }
      }

      def __write_d_diff(f: Field): String = {
        if (!f._noDiff) {
          f.convertedFieldType match {
            case IntType | BooleanType | LongType | FloatType =>
              "// TODO primitive type cannot have diff"
            case ObjectType(n, _, true) =>
              s"""
           |
           | if(${f.fieldName} == null && o.${f.fieldName} != null) {
           |   d.${f.fieldName} = o.${f.fieldName};
           | } else if (${f.fieldName} != null && !${f.fieldName}.equals(o.${f.fieldName})) {
           |   d.${f.fieldName} = ${f.fieldName}.diff(o.${f.fieldName});
           | }
         """.stripMargin
            case _ =>
              s"""
           |
           | if ((${f.fieldName} == null && o.${f.fieldName} != null) || (${f.fieldName} != null && !${f.fieldName}.equals(o.${f.fieldName}))) {
           |   d.${f.fieldName} = o.${f.fieldName};
           | }
         """.stripMargin
        }
        } else {
          ""
        }
      }

      val parser =
        s"""
           |
           |
           |     public static class Parser implements StreamParser<${name}> {
           |         @Override
           |         public ${name} parse(InputStream in) throws IOException, JsonParseException {
           |            JsonParser jp = JsonFactoryHolder.APP_FACTORY.createParser(in);
           |            jp.nextToken();
           |            return ${name}.parse(jp);
           |        }
           |}
           |
           |     public static class ArrayParser implements StreamParser<ArrayList<${name}>> {
           |               @Override
           |          public ArrayList<${name}> parse(InputStream in) throws IOException, JsonParseException {
           |            JsonParser jp = JsonFactoryHolder.APP_FACTORY.createParser(in);
           |            jp.nextToken();
           |            ArrayList<${name}> arrayValue = null;
           |            if (jp.getCurrentToken() == JsonToken.START_ARRAY) {
           |                arrayValue = new ArrayList<${name}>();
           |                while (jp.nextToken() != JsonToken.END_ARRAY) {
           |                   ${name} parsed = ${name}.parse(jp);
           |                   if (parsed != null) {
           |                     arrayValue.add(parsed);
           |  }
           |}
         | }
         | jp.nextToken();
         | return arrayValue;
            | }
           |     }
           |     public static Parser parser = new Parser();
           |     public static ArrayParser arrayParser = new ArrayParser();
           |
           |  public static ${name} parse(JsonParser jp) throws IOException {
           |    ${name} instance = new ${name}();
           |    if (jp.getCurrentToken() != JsonToken.START_OBJECT) {
           |      jp.skipChildren();
           |      return null;
           |    }
           |    while (jp.nextToken() != JsonToken.END_OBJECT) {
           |      String fieldName = jp.getCurrentName();
           |      jp.nextToken();
           |      processSingleField(instance, fieldName, jp);
           |      jp.skipChildren();
           |    }
           |    return instance;
           |  }
           |
           |  private static final boolean processSingleField(${name} instance, String fieldName, JsonParser jp) throws IOException {
           |    ${fields.map(if_jsonField_equals_fieldName_then_parse_field).mkString}
           |    return false;
           |  }
           |
           |  public static final ${name} parse(String inputString) throws IOException {
           |    JsonParser jp = JsonFactoryHolder.APP_FACTORY.createParser(inputString);
           |    jp.nextToken();
           |    return parse(jp);
           |  }
           |
           |  public final void serialize(JsonGenerator generator, boolean writeStartAndEnd) throws IOException {
           |    if (writeStartAndEnd) {
           |      generator.writeStartObject();
           |    }
           |    ${fields.map(a => if (a.serializable) write_field_if_not_null(a.jsonFieldName, a.fieldName, a.fieldType) else "").mkString}
           |    if (writeStartAndEnd) {
           |      generator.writeEndObject();
           |    }
           |  }
           |
           |  public final String serialize() throws IOException {
           |    StringWriter stringWriter = new StringWriter();
           |    JsonGenerator generator = JsonFactoryHolder.APP_FACTORY.createGenerator(stringWriter);
           |    serialize(generator, true);
           |    generator.close();
           |    return stringWriter.toString();
           |  }
           |
           |  public String toString() {
           |    try {
           |      return serialize();
           |    } catch (Exception e) {
           |       e.printStackTrace();
           |       return null;
           |    }
           |  }
           |
           |
           |
         """.stripMargin
      val diff = if (genereateDiff) {
        s"""
         |   public ${name} diff(${name} o) {
         |     ${name} d = new ${name}();
         |     ${fields.map(__write_d_diff).mkString("\n")}
         |     return d;
         |   }
         """.stripMargin
      } else {
        ""
      }
      val content =
        s"""
         |
         |
         |
         | /**
         | except for code between the two "extra code" mark,
         | all content auto-generated, so do not modify. they will be overwritten
         | */
         | public final class ${name} ${if (t._extends != null) "extends " + t._extends else ""} implements Cloneable, Serializable {
         |
         |  ${EXTRA_CODE_MARK}
         |
         |
         |
         |
         |   ${dataFields}
         |   ${parser}
         |
         |
         |
         |  @Override
         |   public boolean equals(Object obj) {
         |    if (obj instanceof ${name}) {
         |    ${name} o = (${name}) obj;
         |     return (obj == this)  || (${fields.map(__field_equals).mkString(") &&\n       (")});
         |    } else {
         |      return false;
         |    }
         |   }
         |
         |   ${diff}
         |
         |   public ${name} deepCopy() {
         |   // the funny thing is that it is faster than java Serialization
         |       try {
         |           return  parse(toString());
         |      } catch (IOException e) {
         |          e.printStackTrace();
         |      }
         |      return null;
         |   }
         |   public ${name} shallowCopy() {
         |   try {
         |     return (${name}) clone();
         |   } catch (CloneNotSupportedException e) {
         |     e.printStackTrace();
         |   }
         |  return null;
         |  }
         |
         | }
       """.stripMargin
      ClassFile(spec.packageName, name, content)
  }

}


object JsonToSpec {

  var classes: Map[String, ObjectType] = Map.empty


  def fieldName(s: String) = {
    assert(s.length > 0)
    if (s == "new") "new_" else {
      val a = s.split("_")
      a.head + a.tail.map(upperFirst).mkString
    }
  }

  def upperFirst(s: String) = {
    assert(s.length > 0)
    s(0).toUpper + s.substring(1)
  }

  def clazzName(s: String) = {
    var l = upperFirst(s)
    if (l.last == 's') {
      l = l.dropRight(1)
    }
    l
  }

  def genJsonType(o: Any, guessName: String): JsonType = o match {
    case map: Map[String, Any] =>
      val res = ObjectType(guessName, map.map(f => {
        Field(f._1, fieldName(f._1), genJsonType(f._2, clazzName(f._1)))
      }).toSeq)
      classes.get(guessName) match {
        case None => classes = classes.updated(guessName, res)
        case a@Some(v) => classes = classes.updated(guessName, if (v.toString.length < res.toString.length) res else v)
      }
      res
    case list: List[Any] => list.length match {
      case 0 => ArrayType(UnknownType)
      case _ => ArrayType(genJsonType(list(0), guessName))
    }
    case int: Int => IntType
    case string: String => StringType
    case boolean: Boolean => BooleanType
    case long: Long => LongType
    case double: Double => if(double.toInt == double) IntType else DoubleType
    case _ => UnknownType
  }

  def jsonDirToSpec(dir: String) = {
    val jsonDir = new File(dir)
    for (f <- jsonDir.listFiles) {
      val cn = upperFirst(f.getName.split("\\.")(0))
      JSON.parseFull(Source.fromFile(f).mkString) match {
        case Some(v) => genJsonType(v, cn)
        case None => ""
      }
    }
    classes.map(_._2).toSeq
  }

  def jsonDirToSpecString(specName: String, dir: String) = {
    val cs = jsonDirToSpec(dir)

    def c(s: ObjectType) = {
      s"""lazy val ${s.typeName} = ${s.toString}"""
    }
    s"""
       |
       | object ${specName} {
       |  ${cs.map(c).mkString("\n\n  ")}
       | }
     """.stripMargin
  }
}


trait Spec {
  val classSpecs: Seq[ObjectType]
  val enumSpecs: Seq[EnumType]
  val converterClassName: String
  val packageName: String
}



object TestSpec extends Spec {

  lazy val Meta = ObjectType("Meta", Seq(
    Field("code", "code", IntType),
    Field("message", "message", StringType)))



  lazy val Conversation = ObjectType("Conversation", Seq(
    Field("createdTime", "createdTime", ApiTimeStringToDate),
    Field("id", "id", StringType),
    Field("owner", "owner", UserIdUnboxed)
  ))

  lazy val Data = ObjectType("Data", Seq(
    Field("users", "users", ArrayType(User))
  ))


  lazy val UserIdBoxed = ObjectType("UserIdBoxed", Seq(
    //Field("type", "type", StringType),
    Field("id", "id", StringType)
  ))

  lazy val UserIdUnboxed = ConvertedType(UserIdBoxed, StringType, "UserIdUnboxed", "UserIdBoxed")

  lazy val ApiTimeStringToDate = ConvertedType(StringType, JavaObjectType("java.util.Date"), "ApiTimeStringToDate", "DateToApiTimeString")

  lazy val ApiDateStringToDate = ConvertedType(StringType, JavaObjectType("java.util.Date"), "ApiDateStringToDate", "DateToApiDateString")

  lazy val StringToYear = ConvertedType(StringType, JavaObjectType("java.util.Date"), "StringToYear", "YearToString")



  //  val Parameter = ObjectType("Parameter", Seq(
  //    Field("name", "name", StringType),
  //    Field("values", "values", ArrayType(StringType))))


  lazy val PhoneNumber = ObjectType("PhoneNumber", Seq(
    Field("countryCode", "countryCode", IntType),
    Field("number", "number", StringType)))




  lazy val Tag = ObjectType("Tag", Seq(
    Field("value", "value", StringType),
    Field("category", "category", StringType)))


  lazy val Profile = ObjectType("Profile", Seq(
    Field("school", "school", StringType),
    Field("hangouts", "hangouts", StringType),
    Field("hometown", "hometown", StringType),
    Field("tags", "tags", ArrayType(Tag)),
    Field("job", "job", StringType),
    Field("zodiac", "zodiac", StringType),
    Field("occupation", "occupation", StringType)))


  lazy val Setting = ObjectType("Setting", Seq(
    Field("birthdate", "birthdate", StringToYear)))



  lazy val StatusToEmun = ConvertedType(StringType, JavaObjectType("UserStatus"), "StatusToEnum", "EnumToStatus")


  lazy val User = ObjectType("User", Seq(
    Field("name", "name", StringType),
    Field("description", "description", StringType),
    Field("age", "age", IntType),
    Field("pictures", "pictures", ArrayType(RawPictureToCorrectType)),
    Field("status", "status", ArrayType(StatusToEmun)),
    Field("settings", "settings", Setting)))

  lazy val Message = ObjectType("Message", Seq(
    Field("owner", "owner", UserIdUnboxed),
    Field("value", "value", StringType)))


  lazy val RawPictureToCorrectType = ConvertedType(RawPicture, JavaObjectType("MediaBase"), "RawPictureToCorrectType", "MediaToRawPicture")

  lazy val RawPicture: ObjectType = ObjectType("RawPicture", Vector(
    //Field("parameters", "parameters", ArrayType(Parameter)),
    Field("duration", "duration", IntType),
    Field("mediaType", "mediaType", StringType),
    Field("size", "size", ArrayType(IntType, 2)),
    Field("url", "url", StringType),
    Field("attachments", "attachments", ArrayType(null))
  ))

  RawPicture.fields.last.fieldType = ArrayType(RawPicture)

  override val converterClassName: String = "Converter"
  
  override lazy val enumSpecs: Seq[EnumType] = Seq.empty
  override lazy val classSpecs: Seq[ObjectType] = Seq(Meta, Data, UserIdBoxed, Conversation, PhoneNumber, Tag, Profile,Setting, User, Message, RawPicture)
  override val packageName: String = "org.snailya.test"
}




GenerateContent(TestSpec).map(_.writeTo(new File("desc_java_folder")))
