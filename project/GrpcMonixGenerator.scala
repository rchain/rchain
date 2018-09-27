package grpcmonix.generators

import com.google.protobuf.Descriptors._
import com.google.protobuf.ExtensionRegistry
import com.google.protobuf.compiler.PluginProtos.{CodeGeneratorRequest, CodeGeneratorResponse}
import scalapb.compiler.FunctionalPrinter.PrinterEndo
import scalapb.compiler.{
  DescriptorPimps,
  FunctionalPrinter,
  GeneratorParams,
  ProtobufGenerator,
  StreamType
}
import scalapb.options.compiler.Scalapb

import scala.collection.JavaConverters._

object GrpcMonixGenerator {
  def apply(flatPackage: Boolean = false): GrpcMonixGenerator = {
    val params = GeneratorParams().copy(flatPackage = flatPackage)
    new GrpcMonixGenerator(params)
  }
}

class GrpcMonixGenerator(override val params: GeneratorParams)
    extends protocbridge.ProtocCodeGenerator
    with DescriptorPimps {
  def run(requestBytes: Array[Byte]): Array[Byte] = {
    // Read scalapb.options (if present) in .proto files
    val registry = ExtensionRegistry.newInstance()
    Scalapb.registerAllExtensions(registry)
    val b       = CodeGeneratorResponse.newBuilder
    val request = CodeGeneratorRequest.parseFrom(requestBytes, registry)

    val fileDescByName: Map[String, FileDescriptor] =
      request.getProtoFileList.asScala.foldLeft[Map[String, FileDescriptor]](Map.empty) {
        case (acc, fp) =>
          val deps = fp.getDependencyList.asScala.map(acc)
          acc + (fp.getName -> FileDescriptor.buildFrom(fp, deps.toArray))
      }

    request.getFileToGenerateList.asScala.foreach { name =>
      val fileDesc     = fileDescByName(name)
      val responseFile = generateFile(fileDesc)
      b.addFile(responseFile)
    }
    b.build.toByteArray
  }

  private[this] def grpcObserver(typeName: String) = s"StreamObserver[$typeName]"

  private[this] def serviceCompanion(typeName: String) = s"ServiceCompanion[$typeName]"

  private[this] def task(typeParam: String) = s"Task[$typeParam]"

  private[this] def serviceMethodDescriptor(method: MethodDescriptor): PrinterEndo = { printer =>
    val methodType = method.streamType match {
      case StreamType.Unary           => "UNARY"
      case StreamType.ClientStreaming => "CLIENT_STREAMING"
      case StreamType.ServerStreaming => "SERVER_STREAMING"
      case StreamType.Bidirectional   => "BIDI_STREAMING"
    }

    def marshaller(typeName: String) = s"new Marshaller($typeName)"

    printer
      .add(
        s"val ${method.descriptorName}: MethodDescriptor[${method.scalaIn}, ${method.scalaOut}] ="
      )
      .indent
      .add("MethodDescriptor.newBuilder()")
      .addIndented(
        s".setType(MethodDescriptor.MethodType.$methodType)",
        s""".setFullMethodName(MethodDescriptor.generateFullMethodName("${method.getService.getFullName}", "${method.getName}"))""",
        s".setRequestMarshaller(new Marshaller(${method.scalaIn}))",
        s".setResponseMarshaller(new Marshaller(${method.scalaOut}))",
        ".build()"
      )
      .outdent
  }

  private[this] def serviceDescriptor(service: ServiceDescriptor): PrinterEndo =
    _.add(
      s"""val SERVICE: _root_.io.grpc.ServiceDescriptor = _root_.io.grpc.ServiceDescriptor.newBuilder("${service.getFullName}")"""
    ).indent
      .add(
        s".setSchemaDescriptor(new ConcreteProtoFileDescriptorSupplier(${service.getFile.fileDescriptorObjectFullName}.javaDescriptor))"
      )
      .print(service.methods) {
        case (printer, method) =>
          printer.add(s".addMethod(${method.descriptorName})")
      }
      .add(".build()")
      .outdent

  private[this] def serviceMethodSignature(method: MethodDescriptor) =
    s"def ${method.name}" + (method.streamType match {
      case StreamType.Unary =>
        s"(request: ${method.scalaIn}): ${task(method.scalaOut)}"
      case StreamType.ClientStreaming =>
        s"(input: Observable[${method.scalaIn}]): ${task(method.scalaOut)}"
      case StreamType.ServerStreaming =>
        s"(request: ${method.scalaIn}): Observable[${method.scalaOut}]"
      case StreamType.Bidirectional =>
        s"(input: Observable[${method.scalaIn}]): Observable[${method.scalaOut}]"
    })

  private[this] def serviceTrait(service: ServiceDescriptor): PrinterEndo = { printer =>
    printer
      .add(s"trait ${service.getName} extends AbstractService {")
      .indent
      .add(s"override def serviceCompanion = ${service.getName}")
      .seq(service.methods.map(serviceMethodSignature))
      .outdent
      .add("}")
  }

  private[this] def serviceTraitCompanion(
      service: ServiceDescriptor,
      fileDesc: FileDescriptor
  ): PrinterEndo = { printer =>
    printer
      .add(s"object ${service.getName} extends ${serviceCompanion(service.getName)} {")
      .indent
      .add(s"implicit def serviceCompanion: ${serviceCompanion(service.getName)} = this")
      .add(
        s"def javaDescriptor: ServiceDescriptor = ${fileDesc.fileDescriptorObjectFullName}.javaDescriptor.getServices().get(0)"
      )
      .outdent
      .add("}")
  }

  private[this] def stub(service: ServiceDescriptor): PrinterEndo = { printer =>
    printer
      .add(s"class ${service.stub}(")
      .indent
      .add(s"channel: Channel,")
      .add(s"options: CallOptions = CallOptions.DEFAULT")
      .outdent
      .add(s") extends AbstractStub[${service.stub}](channel, options) with ${service.name} {")
      .indent
      .print(service.getMethods.asScala) {
        case (p, m) => p.call(clientMethodImpl(m))
      }
      .add(s"override def build(channel: Channel, options: CallOptions): ${service.stub} = ")
      .indent
      .add(s"new ${service.stub}(channel, options)")
      .outdent
      .outdent
      .add("}")
  }

  private[this] def clientMethodImpl(method: MethodDescriptor): PrinterEndo = { printer =>
    def liftByGrpcOperator(inputType: String, outputType: String) =
      s"liftByGrpcOperator[$inputType, $outputType]"

    method.streamType match {
      case StreamType.Unary =>
        printer
          .add(s"override ${serviceMethodSignature(method)} = ")
          .indent
          .add("guavaFutureToMonixTask(")
          .indent
          .add(
            s"ClientCalls.futureUnaryCall(channel.newCall(${method.descriptorName}, options), request)"
          )
          .outdent
          .add(")")
          .outdent
      case StreamType.ClientStreaming =>
        printer
          .add(s"override ${serviceMethodSignature(method)} = ")
          .indent
          .add(s"${liftByGrpcOperator(method.scalaIn, method.scalaOut)}(")
          .indent
          .add("input,")
          .add(s"outputObserver =>")
          .indent
          .add("ClientCalls.asyncClientStreamingCall(")
          .indent
          .add(s"channel.newCall(${method.descriptorName}, options),")
          .add("outputObserver")
          .outdent
          .add(")")
          .outdent
          .outdent
          .add(").firstL")
          .outdent
      case StreamType.ServerStreaming =>
        printer
          .add(s"override ${serviceMethodSignature(method)} = ")
          .indent
          .add(s"Observable.fromReactivePublisher(new PublisherR[${method.scalaOut}] {")
          .indent
          .add(
            s"override def subscribe(subscriber: SubscriberR[_ >: ${method.scalaOut}]): Unit = {"
          )
          .indent
          .add("ClientCalls.asyncServerStreamingCall(")
          .addIndented(
            s"channel.newCall(${method.descriptorName}, options),",
            "request,",
            s"reactiveSubscriberToGrpcObserver[${method.scalaOut}](subscriber)"
          )
          .add(")")
          .outdent
          .add("}")
          .outdent
          .add("})")
          .outdent
      case StreamType.Bidirectional =>
        printer
          .add(s"override ${serviceMethodSignature(method)} = ")
          .indent
          .add(s"${liftByGrpcOperator(method.scalaIn, method.scalaOut)}(")
          .indent
          .add("input,")
          .add("outputObserver =>")
          .indent
          .add("ClientCalls.asyncBidiStreamingCall(")
          .indent
          .add(s"channel.newCall(${method.descriptorName}, options),")
          .add("outputObserver")
          .outdent
          .add(")")
          .outdent
          .outdent
          .add(")")
          .outdent
    }
  }

  private[this] def bindService(service: ServiceDescriptor): PrinterEndo = { printer =>
    printer
      .add(
        s"def bindService(serviceImpl: ${service.name}, scheduler: Scheduler): ServerServiceDefinition = "
      )
      .indent
      .add("ServerServiceDefinition")
      .indent
      .add(".builder(SERVICE)")
      .print(service.methods) {
        case (p, m) =>
          p.call(addMethodImplementation(m))
      }
      .add(".build()")
      .outdent
      .outdent
  }

  private[this] def addMethodImplementation(method: MethodDescriptor): PrinterEndo = { printer =>
    def unliftByTransformer(inputType: String, outputType: String) =
      s"unliftByTransformer[$inputType, $outputType]"

    val call = method.streamType match {
      case StreamType.Unary           => "ServerCalls.asyncUnaryCall"
      case StreamType.ClientStreaming => "ServerCalls.asyncClientStreamingCall"
      case StreamType.ServerStreaming => "ServerCalls.asyncServerStreamingCall"
      case StreamType.Bidirectional   => "ServerCalls.asyncBidiStreamingCall"
    }
    val serverMethod = method.streamType match {
      case StreamType.Unary => s"ServerCalls.UnaryMethod[${method.scalaIn}, ${method.scalaOut}]"
      case StreamType.ClientStreaming =>
        s"ServerCalls.ClientStreamingMethod[${method.scalaIn}, ${method.scalaOut}]"
      case StreamType.ServerStreaming =>
        s"ServerCalls.ServerStreamingMethod[${method.scalaIn}, ${method.scalaOut}]"
      case StreamType.Bidirectional =>
        s"ServerCalls.BidiStreamingMethod[${method.scalaIn}, ${method.scalaOut}]"
    }
    val impl: PrinterEndo = method.streamType match {
      case StreamType.Unary =>
        _.add(
          s"override def invoke(request: ${method.scalaIn}, observer: ${grpcObserver(method.scalaOut)}): Unit ="
        ).indent
          .add(
            s"serviceImpl.${method.name}(request).runAsync(grpcObserverToMonixCallback(observer))(scheduler)"
          )
          .outdent
      case StreamType.ClientStreaming =>
        _.add(
          s"override def invoke(observer: ${grpcObserver(method.scalaOut)}): ${grpcObserver(method.scalaIn)} = {"
        ).indent
          .add("val outputSubscriber = grpcObserverToMonixSubscriber(observer, scheduler)")
          .add(s"val inputSubscriber = ${unliftByTransformer(method.scalaIn, method.scalaOut)}(")
          .indent
          .add(
            s"inputObservable => Observable.fromTask(serviceImpl.${method.name}(inputObservable)),"
          )
          .add("outputSubscriber")
          .outdent
          .add(")")
          .add("monixSubscriberToGrpcObserver(inputSubscriber)")
          .outdent
          .add("}")
      case StreamType.ServerStreaming =>
        _.add(
          s"override def invoke(request: ${method.scalaIn}, observer: ${grpcObserver(method.scalaOut)}): Unit = "
        ).indent
          .add(
            s"serviceImpl.${method.name}(request).subscribe(grpcObserverToMonixSubscriber(observer, scheduler))"
          )
          .outdent
      case StreamType.Bidirectional =>
        _.add(
          s"override def invoke(observer: ${grpcObserver(method.scalaOut)}): ${grpcObserver(method.scalaIn)} = {"
        ).indent
          .add("val outputSubscriber = grpcObserverToMonixSubscriber(observer, scheduler)")
          .add(s"val inputSubscriber = ${unliftByTransformer(method.scalaIn, method.scalaOut)}(")
          .indent
          .add(s"inputObservable => serviceImpl.${method.name}(inputObservable),")
          .add("outputSubscriber")
          .outdent
          .add(")")
          .add("monixSubscriberToGrpcObserver(inputSubscriber)")
          .outdent
          .add("}")
    }

    printer
      .add(".addMethod(")
      .indent
      .add(s"${method.descriptorName},")
      .add(s"$call(")
      .indent
      .add(s"new $serverMethod {")
      .indent
      .call(impl)
      .outdent
      .add("}")
      .outdent
      .add(")")
      .outdent
      .add(")")
  }

  private[this] def javaDescriptor(service: ServiceDescriptor): PrinterEndo = { printer =>
    printer
      .add(s"def javaDescriptor: ServiceDescriptor = ")
      .indent
      .add(
        s"${service.getFile.fileDescriptorObjectFullName}.javaDescriptor.getServices().get(${service.getIndex})"
      )
      .outdent
  }

  def generateFile(fileDesc: FileDescriptor): CodeGeneratorResponse.File = {
    val b = CodeGeneratorResponse.File.newBuilder()

    val objectName = fileDesc.fileDescriptorObjectName
      .substring(0, fileDesc.fileDescriptorObjectName.length - 5) + "GrpcMonix"

    b.setName(s"${fileDesc.scalaDirectory}/$objectName.scala")
    val fp = FunctionalPrinter()
      .add(s"package ${fileDesc.scalaPackageName}")
      .newline
      .add("import _root_.com.google.protobuf.Descriptors.ServiceDescriptor")
      .add(
        "import _root_.scalapb.grpc.{ AbstractService, ConcreteProtoFileDescriptorSupplier, Marshaller, ServiceCompanion }"
      )
      .add(
        "import _root_.io.grpc.{ CallOptions, Channel, MethodDescriptor, ServerServiceDefinition }"
      )
      .add("import _root_.coop.rchain.grpcmonix.GrpcMonix._")
      .add("import _root_.io.grpc.stub.{ AbstractStub, ClientCalls, ServerCalls, StreamObserver }")
      .add("import _root_.monix.eval.Task")
      .add("import _root_.monix.execution.{ Cancelable, Scheduler }")
      .add("import _root_.monix.reactive.Observable")
      .add(
        "import _root_.org.reactivestreams.{ Publisher => PublisherR, Subscriber => SubscriberR }"
      )
      .newline
      .add(s"object $objectName {")
      .indent
      .newline
      .print(fileDesc.getServices.asScala) {
        case (printer, service) =>
          printer
            .print(service.getMethods.asScala) {
              case (p, m) => p.call(serviceMethodDescriptor(m))
            }
            .newline
            .call(serviceDescriptor(service))
            .newline
            .call(serviceTrait(service))
            .newline
            .call(serviceTraitCompanion(service, fileDesc))
            .newline
            .call(stub(service))
            .newline
            .call(bindService(service))
            .newline
            .add(s"def stub(channel: Channel): ${service.stub} = new ${service.stub}(channel)")
            .newline
            .call(javaDescriptor(service))
      }
      .outdent
      .add("}")
      .newline

    b.setContent(fp.result)
    b.build
  }
}
