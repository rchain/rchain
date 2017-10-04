package r.net

import java.util.UUID

case class JustStrRequest(
    override val msgId  : UUID,
    //override val to     : URI,
    override val to     : Moniker,
    //override val from   : URI,
    override val from   : Moniker,
    override val flowId : UUID,
    override val body   : String,
    override val justification : Option[Response[AbstractJustifiedRequest[String,String],String]])
  extends AbstractJustifiedRequest[String,String](msgId, to, from, flowId, body, justification) {

  //implicit val pumpkinPickler = Pickler.generate[JustStrRequest]
  //implicit val pumpkinUnpickler = Unpickler.generate[JustStrRequest]
}

case class JustStrResponse(
     override val msgId  : UUID,
     //override val to     : URI,
     override val to     : Moniker,
     //override val from   : URI,
     override val from   : Moniker,
     override val flowId : UUID,
     override val body   : String,
     override val justification : Option[Request[AbstractJustifiedResponse[String,String],String]])
  extends AbstractJustifiedResponse[String,String](msgId, to, from, flowId, body, justification) {
  //implicit val pumpkinPickler = Pickler.generate[JustStrResponse]
  //implicit val pumpkinUnpickler = Unpickler.generate[JustStrResponse]

}