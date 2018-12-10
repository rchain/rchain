package coop.rchain.scalapb

import com.google.protobuf.Descriptors.{
  Descriptor,
  FieldDescriptor,
  FileDescriptor,
  OneofDescriptor
}
import com.google.protobuf.ExtensionRegistry
import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorRequest, CodeGeneratorResponse}
import protocbridge.{Artifact, JvmGenerator, ProtocCodeGenerator}
import scalapb.compiler._
import scalapb.options.compiler.Scalapb

object StacksafeScalapbGenerator extends ProtocCodeGenerator {

  //copied and adapted from scalapb pakage object
  def gen(
      flatPackage: Boolean = false,
      javaConversions: Boolean = false,
      grpc: Boolean = true,
      singleLineToProtoString: Boolean = false,
      asciiFormatToString: Boolean = false
  ): (JvmGenerator, Seq[String]) =
    (
      JvmGenerator("scala", StacksafeScalapbGenerator),
      Seq(
        "flat_package"                -> flatPackage,
        "java_conversions"            -> javaConversions,
        "grpc"                        -> grpc,
        "single_line_to_proto_string" -> singleLineToProtoString,
        "ascii_format_to_string"      -> asciiFormatToString
      ).collect { case (name, v) if v => name }
    )

  //Copied and adpated from scalapb.ScalaPbCodeGenerator
  def run(req: Array[Byte]): Array[Byte] = {
    val registry = ExtensionRegistry.newInstance()
    Scalapb.registerAllExtensions(registry)
    val request = CodeGeneratorRequest.parseFrom(req, registry)
    StacksafeProtobufGenerator.handleCodeGeneratorRequest(request).toByteArray
  }

  //Copied and adpated from scalapb.ScalaPbCodeGenerator
  override def suggestedDependencies: Seq[Artifact] = Seq(
    Artifact(
      "com.thesamet.scalapb",
      "scalapb-runtime",
      scalapb.compiler.Version.scalapbVersion,
      crossVersion = true
    )
  )
}

//copied and adapted from scalapb.compiler.ProtobufGenerator companion object
object StacksafeProtobufGenerator {

  import scala.collection.JavaConverters._

  def parseParameters(params: String): Either[String, GeneratorParams] =
    params
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
      .foldLeft[Either[String, GeneratorParams]](Right(GeneratorParams())) {
        case (Right(params), "java_conversions") => Right(params.copy(javaConversions = true))
        case (Right(params), "flat_package")     => Right(params.copy(flatPackage = true))
        case (Right(params), "grpc")             => Right(params.copy(grpc = true))
        case (Right(params), "single_line_to_proto_string") =>
          Right(params.copy(singleLineToProtoString = true))
        case (Right(params), "ascii_format_to_string") =>
          Right(params.copy(asciiFormatToString = true))
        case (Right(params), p) => Left(s"Unrecognized parameter: '$p'")
        case (x, _)             => x
      }

  def handleCodeGeneratorRequest(request: CodeGeneratorRequest): CodeGeneratorResponse = {
    val b = CodeGeneratorResponse.newBuilder
    parseParameters(request.getParameter) match {
      case Right(params) =>
        try {
          val generator = new StacksafeProtobufGenerator(params)
          import generator.FileDescriptorPimp
          val filesByName: Map[String, FileDescriptor] =
            request.getProtoFileList.asScala.foldLeft[Map[String, FileDescriptor]](Map.empty) {
              case (acc, fp) =>
                val deps = fp.getDependencyList.asScala.map(acc)
                acc + (fp.getName -> FileDescriptor.buildFrom(fp, deps.toArray))
            }
          val validator = new ProtoValidation(params)
          filesByName.values.foreach(validator.validateFile)
          request.getFileToGenerateList.asScala.foreach { name =>
            val file = filesByName(name)
            val responseFiles =
              if (file.scalaOptions.getSingleFile)
                generator.generateSingleScalaFileForFileDescriptor(file)
              else generator.generateMultipleScalaFilesForFileDescriptor(file)
            b.addAllFile(responseFiles.asJava)
          }
        } catch {
          case e: GeneratorException =>
            b.setError(e.message)
        }
      case Left(error) =>
        b.setError(error)
    }
    b.build
  }

}

//copied and adapted from scalapb.compiler.ProtobufGenerator
class StacksafeProtobufGenerator(params: GeneratorParams) extends ProtobufGenerator(params) {

  override def printMessage(printer: FunctionalPrinter, message: Descriptor): FunctionalPrinter = {
    val value = super.printMessage(printer, message).result()
    val extended = value.replace(
      " extends scalapb.GeneratedMessage with scalapb.Message[",
      " extends coop.rchain.models.StacksafeMessage["
    )
    new FunctionalPrinter(Vector(extended), printer.indentLevel)
  }

  override def generateSerializedSize(
      message: Descriptor
  )(fp: FunctionalPrinter): FunctionalPrinter = {

    //we piggy-back on this method to emit a stacksafe equals and hashCode overrides
    val withEquals            = generateEqualsOverride(message)(fp.newline)
    val withEqualsAndHashCode = generateHashCodeOverride(message)(withEquals)

    super
      .generateSerializedSize(message)(withEqualsAndHashCode)
      .newline
      .add(
        "@transient val serializedSizeM = new coop.rchain.models.Memo(coop.rchain.models.ProtoM.serializedSize(this))"
      )
      .newline
  }

  private def generateEqualsOverride(message: Descriptor)(fp: FunctionalPrinter) = {
    val myFullScalaName = message.scalaTypeNameWithMaybeRoot(message)
    fp.add(
        s"override def equals(x: Any): Boolean = coop.rchain.models.EqualM[$myFullScalaName].equals[monix.eval.Coeval](this, x).value"
      )
      .newline
  }

  private def generateHashCodeOverride(message: Descriptor)(fp: FunctionalPrinter) = {
    val myFullScalaName = message.scalaTypeNameWithMaybeRoot(message)
    fp.add(
        s"override def hashCode(): Int = coop.rchain.models.HashM[$myFullScalaName].hash[monix.eval.Coeval](this).value"
      )
      .newline
  }

  override def generateMergeFrom(
      message: Descriptor
  )(originalPrinter: FunctionalPrinter): FunctionalPrinter = {

    val printer = super.generateMergeFrom(message)(originalPrinter)

    generateMergeFromM(message)(printer)
  }

  private def generateMergeFromM(
      message: Descriptor
  )(printer: FunctionalPrinter): FunctionalPrinter = {

    import scala.collection.JavaConverters._

    val myFullScalaName = message.scalaTypeNameWithMaybeRoot(message)
    val requiredFieldMap: Map[FieldDescriptor, Int] =
      message.fields.filter(_.isRequired).zipWithIndex.toMap
    printer.newline
      .add(
        s"def mergeFromM[F[_]: cats.effect.Sync](`_input__`: _root_.com.google.protobuf.CodedInputStream): F[$myFullScalaName] = {"
      )
      .indent
      .newline
      .add("import cats.effect.Sync")
      .add("import cats.implicits._")
      .newline
      .add("Sync[F].defer {")
      .indent
      .print(message.fieldsWithoutOneofs)(
        (printer, field) =>
          if (!field.isRepeated)
            printer.add(s"var __${field.scalaName} = this.${field.scalaName.asSymbol}")
          else if (field.isMapField)
            printer.add(
              s"val __${field.scalaName} = (scala.collection.immutable.Map.newBuilder[${field.mapType.keyType}, ${field.mapType.valueType}] ++= this.${field.scalaName.asSymbol})"
            )
          else
            printer.add(
              s"val __${field.scalaName} = (${field.collectionBuilder} ++= this.${field.scalaName.asSymbol})"
            )
      )
      .when(message.preservesUnknownFields) { _ =>
        throw new UnsupportedOperationException("Unknown fields are not supported")
      }
      .when(requiredFieldMap.nonEmpty) { fp =>
        // Sets the bit 0...(n-1) inclusive to 1.
        def hexBits(n: Int): String = "0x%xL".format((0 to (n - 1)).map(i => (1L << i)).sum)
        val requiredFieldCount      = requiredFieldMap.size
        val fullWords               = (requiredFieldCount - 1) / 64
        val bits: Seq[String] = (1 to fullWords).map(_ => hexBits(64)) :+ hexBits(
          requiredFieldCount - 64 * fullWords
        )
        fp.print(bits.zipWithIndex) {
          case (fp, (bn, index)) =>
            fp.add(s"var __requiredFields$index: _root_.scala.Long = $bn")
        }
      }
      .print(message.getOneofs.asScala)(
        (printer, oneof) =>
          printer.add(s"var __${oneof.scalaName} = this.${oneof.scalaName.asSymbol}")
      )
      .addStringMargin(s"""var _done__ = false
           |
           |Sync[F].whileM_ (Sync[F].delay { !_done__ }) {
           |  for {
           |    _tag__ <- Sync[F].delay { _input__.readTag() }
           |    _ <- _tag__ match {
           |      case 0 => Sync[F].delay { _done__ = true }""")
      .print(message.fields) { (printer, field) =>
        val p = {

          val newValBaseF = if (field.isMessage) {
            val defInstance =
              s"${field.getMessageType.scalaTypeNameWithMaybeRoot(message)}.defaultInstance"
            val baseInstance =
              if (field.isRepeated) defInstance
              else {
                val expr =
                  if (field.isInOneof)
                    fieldAccessorSymbol(field)
                  else s"__${field.scalaName}"
                val mappedType =
                  toBaseFieldType(field).apply(expr, field.enclosingType)
                if (field.isInOneof || field.supportsPresence)
                  (mappedType + s".getOrElse($defInstance)")
                else mappedType
              }
            s"coop.rchain.models.SafeParser.readMessage(_input__, $baseInstance)"
          } else if (field.isEnum)
            throw new UnsupportedOperationException("Enums are not supported")
          else s"Sync[F].delay { _input__.read${Types.capitalizedType(field.getType)}() }"

          val customVal = toCustomType(field)("readValue")

          val updateOp =
            if (field.supportsPresence) s"__${field.scalaName} = Option(customTypeValue)"
            else if (field.isInOneof) {
              s"__${field.getContainingOneof.scalaName} = ${field.oneOfTypeName}(customTypeValue)"
            } else if (field.isRepeated) s"__${field.scalaName} += customTypeValue"
            else s"__${field.scalaName} = customTypeValue"

          printer
            .addStringMargin(
              s"""      case ${(field.getNumber << 3) + Types.wireType(field.getType)} =>
                 |        for {
                 |          readValue       <- $newValBaseF
                 |          customTypeValue =  $customVal
                 |          _               <- Sync[F].delay { $updateOp }
                 |        } yield ()"""
            )
            .when(field.isRequired) { _ =>
              throw new UnsupportedOperationException("Required fields are not supported")
            }
        }

        if (field.isPackable) {
          throw new UnsupportedOperationException("Packable fields are not supported")
        } else p
      }
      .when(!message.preservesUnknownFields)(
        _.add("    case tag => Sync[F].delay { _input__.skipField(tag) }")
      )
      .add("    }")
      .add("  } yield ()")
      .add(s"}.map { _ => $myFullScalaName(")
      .indent
      .addWithDelimiter(",")(
        (message.fieldsWithoutOneofs ++ message.getOneofs.asScala).map {
          case e: FieldDescriptor if e.isRepeated =>
            s"  ${e.scalaName.asSymbol} = __${e.scalaName}.result()"
          case e: FieldDescriptor =>
            s"  ${e.scalaName.asSymbol} = __${e.scalaName}"
          case e: OneofDescriptor =>
            s"  ${e.scalaName.asSymbol} = __${e.scalaName}"
        } ++ (if (message.preservesUnknownFields) Seq("  unknownFields = _unknownFields__.result()")
              else Seq())
      )
      .add(")")
      .outdent
      .add("}")
      .outdent
      .add("}")
      .outdent
      .add("}")
      .newline
  }

}
