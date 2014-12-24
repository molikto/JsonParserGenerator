
inspired by [ig-json-parser](https://github.com/Instagram/ig-json-parser)

the difference is:

* do not use annotations to generate the parsers. but use Scala raw value like:
    ```scala
      lazy val Tag = ObjectType("Tag", Seq(
        Field("value", "value", StringType),
        Field("category", "category", StringType)))
    ```

    * one small drawback is if you data is recursive, you need to link the recursive data outside a constructor

* including a class `JsonToSpec` which can generate the spec from a dictionary of json samples. most of time you will want to customize the result by editing the result spec
* you can write converter code to convert between types:

    ```scala
          lazy val ApiTimeStringToDate = ConvertedType(StringType, JavaObjectType("java.util.Date"), "ApiTimeStringToDate", "DateToApiTimeString")
    ```
* it is dead simple
    * you should be able to use it from commandline, or in any build tool you use
    * you can easily customize this thing. as the code is very short
