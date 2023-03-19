package coop.rchain.scalapb

import com.google.protobuf.Descriptors.FieldDescriptor.Type
import com.google.protobuf.Descriptors._
import com.google.protobuf.ExtensionRegistry
import protocbridge.{Artifact, JvmGenerator}
import protocgen.{CodeGenApp, CodeGenRequest, CodeGenResponse}
import scalapb.compiler._
import scalapb.options.compiler.Scalapb

object gen {
  def apply(
      flatPackage: Boolean = false,
      javaConversions: Boolean = false,
      grpc: Boolean = true,
      singleLineToProtoString: Boolean = false,
      asciiFormatToString: Boolean = false,
      noLenses: Boolean = false
  ): (JvmGenerator, Seq[String]) =
    (
      JvmGenerator("scala", StacksafeScalapbGenerator),
      Seq(
        "flat_package"                -> flatPackage,
        "java_conversions"            -> javaConversions,
        "grpc"                        -> grpc,
        "single_line_to_proto_string" -> singleLineToProtoString,
        "ascii_format_to_string"      -> asciiFormatToString,
        "no_lenses"                   -> noLenses
      ).collect { case (name, v) if v => name }
    )
}

object StacksafeScalapbGenerator extends CodeGenApp {
  override def registerExtensions(registry: ExtensionRegistry): Unit =
    Scalapb.registerAllExtensions(registry)

  override def suggestedDependencies: Seq[Artifact] = Seq(
    Artifact(
      "com.thesamet.scalapb",
      "scalapb-runtime",
      scalapb.compiler.Version.scalapbVersion,
      crossVersion = true
    )
  )

  // Adapted from scalapb ProtobufGenerator
  // https://github.com/scalapb/ScalaPB/blob/v0.10.8/compiler-plugin/src/main/scala/scalapb/compiler/ProtobufGenerator.scala#L1732
  def process(request: CodeGenRequest): CodeGenResponse =
    ProtobufGenerator.parseParameters(request.parameter) match {
      case Right(params) =>
        try {
          val implicits = new DescriptorImplicits(params, request.allProtos)
          // Inserted custom printer
          val generator = new StacksafeMessagePrinter(params, implicits)
          val validator = new ProtoValidation(implicits)
          validator.validateFiles(request.allProtos)
          import implicits.FileDescriptorPimp
          val files = request.filesToGenerate.flatMap { file =>
            if (file.scalaOptions.getSingleFile)
              generator.generateSingleScalaFileForFileDescriptor(file)
            else generator.generateMultipleScalaFilesForFileDescriptor(file)
          }
          CodeGenResponse.succeed(files)
        } catch {
          case e: GeneratorException =>
            CodeGenResponse.fail(e.message)
        }
      case Left(error) =>
        CodeGenResponse.fail(error)
    }
}

class StacksafeMessagePrinter(
    params: GeneratorParams,
    implicits: DescriptorImplicits
) extends ProtobufGenerator(params, implicits) {

  import DescriptorImplicits.AsSymbolPimp
  import implicits._

  // Override printing of the whole message
  override def printMessage(printer: FunctionalPrinter, message: Descriptor): FunctionalPrinter = {
    val value     = super.printMessage(printer, message).result()
    val scalaType = message.scalaType.name
    val lensDef   = s"scalapb.lenses.Updatable[${scalaType}]"
    val extended = value.replace(
      s"extends scalapb.GeneratedMessage with $lensDef",
      s"extends coop.rchain.models.StacksafeMessage[$scalaType] with $lensDef"
    )
    new FunctionalPrinter(Vector(extended), printer.indentLevel)
  }

  // Override size calculation
  override def generateSerializedSize(
      message: Descriptor
  )(fp: FunctionalPrinter): FunctionalPrinter = {

    //we piggy-back on this method to emit a stacksafe equals and hashCode overrides
    val withEquals            = generateEqualsOverride(message, fp.newline)
    val withEqualsAndHashCode = generateHashCodeOverride(message, withEquals)

    super
      .generateSerializedSize(message)(withEqualsAndHashCode)
      .newline
      .add("@transient var _serializedSizeM: coop.rchain.models.Memo[Int] = null")
      .newline
      .add("def serializedSizeM: coop.rchain.models.Memo[Int] = synchronized {")
      .add("  if(_serializedSizeM == null) {")
      .add(
        "    _serializedSizeM = new coop.rchain.models.Memo(coop.rchain.models.ProtoM.serializedSize(this))"
      )
      .add("    _serializedSizeM")
      .add("  } else _serializedSizeM")
      .add("}")
  }

  private def generateEqualsOverride(
      message: Descriptor,
      fp: FunctionalPrinter
  ): FunctionalPrinter = {
    val myFullScalaName = message.scalaType.fullNameWithMaybeRoot(message)
    fp.add(s"override def equals(x: Any): Boolean = {")
      .newline
      .add("  import coop.rchain.catscontrib.effect.implicits.sEval")
      .newline
      .add(s" coop.rchain.models.EqualM[$myFullScalaName].equals[cats.Eval](this, x).value")
      .newline
      .add("}")
      .newline
  }

  private def generateHashCodeOverride(
      message: Descriptor,
      fp: FunctionalPrinter
  ): FunctionalPrinter = {
    val myFullScalaName = message.scalaType.fullNameWithMaybeRoot(message)

    val printer =
      fp.add(s"override def hashCode(): Int = {")
        .newline
        .add("  import coop.rchain.catscontrib.effect.implicits.sEval")
        .newline
        .add(s" coop.rchain.models.HashM[$myFullScalaName].hash[cats.Eval](this).value")
        .newline
        .add("}")
        .newline

    // In new version of scalapb (0.10.8) `merge` method is moved to companion object
    //  so it's added here to be a member method.
    generateMergeM(message)(printer)
  }

  // Generated as member method
  private def generateMergeM(message: Descriptor)(printer: FunctionalPrinter): FunctionalPrinter = {
    import scala.collection.JavaConverters._
    val myFullScalaName = message.scalaType.fullNameWithMaybeRoot(message)
    val requiredFieldMap: Map[FieldDescriptor, Int] =
      message.fields.filter(_.isRequired).zipWithIndex.toMap
    printer.newline
      .add(
        s"def mergeFromM[F[_]: cats.effect.Sync](`_input__`: _root_.com.google.protobuf.CodedInputStream): F[$myFullScalaName] = {"
      )
      .indent
      .newline
      .add("import cats.effect.Sync")
      .add("import cats.syntax.all._")
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
        def hexBits(n: Int): String = "0x%xL".format((0 until n).map(i => (1L << i)).sum)
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
          printer.add(s"var __${oneof.scalaName.name} = this.${oneof.scalaName.nameSymbol}")
      )
      .add(s"""var _done__ = false
              |
              |Sync[F].whileM_ (Sync[F].delay { !_done__ }) {
              |  for {
              |    _tag__ <- Sync[F].delay { _input__.readTag() }
              |    _ <- _tag__ match {
              |      case 0 => Sync[F].delay { _done__ = true }""".stripMargin)
      .print(message.fields) { (printer, field) =>
        val p = {
          val newValBaseF = if (field.isMessage) {
            val defInstance =
              s"${field.getMessageType.scalaType.fullNameWithMaybeRoot(message)}.defaultInstance"
            val baseInstance =
              if (field.isRepeated) defInstance
              else {
                val expr =
                  if (field.isInOneof) fieldAccessorSymbol(field)
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
          else if (field.getType == Type.STRING)
            s"Sync[F].delay { _input__.readStringRequireUtf8() }"
          else s"Sync[F].delay { _input__.read${Types.capitalizedType(field.getType)}() }"

          val customVal = toCustomType(field)("readValue")

          val updateOp =
            if (field.supportsPresence) s"__${field.scalaName} = Option(customTypeValue)"
            else if (field.isInOneof) {
              s"__${field.getContainingOneof.scalaName.name} = ${field.oneOfTypeName.fullName}(customTypeValue)"
            } else if (field.isRepeated) s"__${field.scalaName} += customTypeValue"
            else s"__${field.scalaName} = customTypeValue"

          printer
            .add(
              s"""      case ${(field.getNumber << 3) + Types.wireType(field.getType)} =>
                 |        for {
                 |          readValue       <- $newValBaseF
                 |          customTypeValue =  $customVal
                 |          _               <- Sync[F].delay { $updateOp }
                 |        } yield ()""".stripMargin
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
      .add("}")
      .add(s".map { _ => $myFullScalaName(")
      .addWithDelimiter(",")(
        (message.fieldsWithoutOneofs ++ message.getOneofs.asScala).map {
          case e: FieldDescriptor if e.isRepeated =>
            s"  ${e.scalaName.asSymbol} = __${e.scalaName}.result()"
          case e: FieldDescriptor =>
            s"  ${e.scalaName.asSymbol} = __${e.scalaName}"
          case e: OneofDescriptor =>
            s"  ${e.scalaName.nameSymbol} = __${e.scalaName.name}"
        } ++ (if (message.preservesUnknownFields)
                Seq(
                  "  unknownFields = if (_unknownFields__ == null) this.unknownFields else _unknownFields__.result()"
                )
              else Seq())
      )
      .add(")}")
      .outdent
      .add("}")
      .outdent
      .add("}")
      .newline
  }

}
