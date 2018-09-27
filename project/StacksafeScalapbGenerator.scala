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
class StacksafeProtobufGenerator(params: GeneratorParams) extends ProtobufGenerator(params) {}
