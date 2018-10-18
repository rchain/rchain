package coop.rchain.roscala.prim

import coop.rchain.roscala.GlobalEnv
import coop.rchain.roscala.Vm.State
import coop.rchain.roscala.ob._
import coop.rchain.roscala.prim.actor.{actorNextBang, actorUpdateBang}
import coop.rchain.roscala.prim.fixnum.fxPlus
import coop.rchain.roscala.prim.rblfloat.flPlus
import coop.rchain.roscala.prim.tuple.{tplHead, tplTail}
import coop.rchain.roscala.util.misc.{numberSuffix, properPrep}

import scala.reflect.{classTag, ClassTag}

sealed trait PrimError
case class TypeMismatch(argNum: Int, typeName: String) extends PrimError {
  override def toString: String = ""
}
case class IndexOutOfBounds(argNum: Int, size: Int) extends PrimError {
  override def toString: String = s"Index $argNum out of bounds $size"
}
case object ArithmeticError extends PrimError {
  override def toString: String = "arithmetic exception"
}

abstract class Prim extends Ob {
  val name: String
  val minArgs: Int
  val maxArgs: Int

  def fn(ctxt: Ctxt, state: State): Ob =
    fnSimple(ctxt) match {
      case Right(m)              => m
      case Left(e: TypeMismatch) => Prim.mismatch(ctxt, e.argNum, e.typeName)
      case Left(e)               => Prim.runtimeError(ctxt, e.toString)
    }

  def fnSimple(ctxt: Ctxt): Either[PrimError, Ob]

  def dispatchHelper(state: State): Ob = {
    val n = state.ctxt.nargs
    if (minArgs <= n && n <= maxArgs)
      fn(state.ctxt, state)
    else
      Prim.mismatchArgs(state, minArgs, maxArgs)
  }

  override def dispatch(ctxt: Ctxt, state: State): Ob = {
    val result = dispatchHelper(state)

    if (result != Invalid && result != Upcall && result != Deadthread) {
      state.ctxt.ret(result, state)
    }

    result
  }

  override def invoke(ctxt: Ctxt, state: State): Ob =
    dispatch(ctxt, state)
}

object Prim {
  val MaxArgs = 255

  /**
    * The mapping from primnum to function-name show below.
    *
    * 0	prim-externalAppCBUnregister
    * 1	prim-externalAppCBLookup
    * 2	prim-externalAppCBRegister
    * 3	prim-externalAppCBRegistry
    * 4	prim->tuple
    * 5	tuple-reverse
    * 6	tuple-include
    * 7	tuple-exclude
    * 8	tuple-zip
    * 9	tuple-unzip
    * 10	prim-configuration-parameters
    * 11	ostream-display-join
    * 12	prim-execvp
    * 13	socketpair
    * 14	regexpCompare
    * 15	fd-open-istream
    * 16	fd-open-ostream
    * 17	uWrite
    * 18	uRead
    * 19	uClose
    * 20	uOpen
    * 21	delete-io-handler
    * 22	set-io-pool
    * 23	prim-string->
    * 24	strlen
    * 25	fx->string
    * 26	string->fx
    * 27	c2str
    * 28	c2bv
    * 29	_c2bv
    * 30	unlink
    * 31	exit
    * 32	sbrk
    * 33	sh
    * 34	fcntl
    * 35	memcpy
    * 36	u_free
    * 37	u_bzero
    * 38	malloc
    * 39	tenured-bytevec-new
    * 40	basic-bytevec-new
    * 41	saddr
    * 42	slot0
    * 43	ob@
    * 44	char*->string
    * 45	M-set
    * 46	M-get
    * 47	prim-init
    * 48	identity1
    * 49	prim-gen-actor
    * 50	prim-new-SBO
    * 51	symb->printString
    * 52	str->printString
    * 53	ch->printString
    * 54	stack-reset
    * 55	stack-top
    * 56	stack-pop
    * 57	stack-push
    * 58	stack-empty?
    * 59	stack-depth
    * 60	stack-new
    * 61	tuple-matches?
    * 62	tuple-mem?
    * 63	tuple-new-n
    * 64	tuple-new
    * 65	tuple-tail
    * 66	tuple-last
    * 67	tuple-head
    * 68	tuple-xchg
    * 69	tuple-safe-nth
    * 70	tuple-concat
    * 71	tuple-rcons
    * 72	tuple-cons*
    * 73	tuple-cons
    * 74	null?
    * 75	tbl-del
    * 76	tbl-get
    * 77	tbl-add
    * 78	getFd
    * 79	prim-flush
    * 80	log-time-string
    * 81	ostream-log-time
    * 82	prim-print
    * 83	prim-display
    * 84	stream-status
    * 85	ostream-close
    * 86	ostream-print
    * 87	ostream-display
    * 88	ostream-new
    * 89	istream-close
    * 90	istream-rdstate
    * 91	istream-clear
    * 92	istream-readline
    * 93	istream-readch
    * 94	istream-resume-io
    * 95	istream-read
    * 96	istream-new
    * 97	string-split
    * 98	string-get-token
    * 99	string-mem?
    * 100	string-new
    * 101	string-length
    * 102	string-set-nth
    * 103	string-size
    * 104	string-join
    * 105	string-concat
    * 106	string-ci>=
    * 107	string-ci>
    * 108	string-ci<=
    * 109	string-ci<
    * 110	string-ci!=
    * 111	string-ci=
    * 112	string>=
    * 113	string>
    * 114	string<=
    * 115	string<
    * 116	string!=
    * 117	string=
    * 118	queue-reset
    * 119	queue-dequeue-nth
    * 120	queue-read-nth
    * 121	queue-pat-read
    * 122	queue-pat-dequeue
    * 123	queue-read
    * 124	queue-dequeue
    * 125	queue-enqueue
    * 126	queue-empty?
    * 127	queue-depth
    * 128	queue-new
    * 129	proc-new
    * 130	runtime-error
    * 131	parser-reset
    * 132	parser-resume
    * 133	parser-parse
    * 134	parser-new
    * 135	syncoprn-new
    * 136	oprn-new
    * 137	now
    * 138	random-number
    * 139	random-number-init
    * 140	cwd
    * 141	version
    * 142	sys-reset
    * 143	sleep
    * 144	scavenge
    * 145	gc
    * 146	classname
    * 147	set-field
    * 148	get-field
    * 149	object->symbol
    * 150	object->string
    * 151	prim-sub-object
    * 152	prim-set-nth
    * 153	prim-nth
    * 154	prim-size
    * 155	mbox:
    * 156	mbox
    * 157	parent:
    * 158	parent
    * 159	meta:
    * 160	meta
    * 161	lookup
    * 162	set
    * 163	add
    * 164	become!
    * 165	clone-to
    * 166	clone
    * 167	home
    * 168	self
    * 169	same!?
    * 170	same?
    * 171	absent!?
    * 172	absent?
    * 173	niv?
    * 174	identity
    * 175	suicide
    * 176	run-with-env
    * 177	run
    * 178	compile
    * 179	object-lookup-and-invoke
    * 180	flFormat:
    * 181	fl->fx
    * 182	fl-sin
    * 183	fl-cos
    * 184	fl-atan
    * 185	fl-ceil
    * 186	fl-floor
    * 187	fl-log10
    * 188	fl-log
    * 189	fl-expt
    * 190	fl-exp
    * 191	fl-abs
    * 192	fl-max
    * 193	fl-min
    * 194	fl!=
    * 195	fl=
    * 196	fl>=
    * 197	fl>
    * 198	fl<=
    * 199	fl<
    * 200	fl/
    * 201	fl*
    * 202	fl-
    * 203	fl+
    * 204	fx-lsr
    * 205	fx-lsl
    * 206	fx-asr
    * 207	fx-asl
    * 208	fx->ch
    * 209	fx->fl
    * 210	fx-cdiv
    * 211	fx-mdiv
    * 212	fx-lognot
    * 213	fx-logxor
    * 214	fx-logor
    * 215	fx-logand
    * 216	fx-lgf
    * 217	fx-lg
    * 218	fx-expt
    * 219	fx-abs
    * 220	fx-max
    * 221	fx-min
    * 222	fx!=
    * 223	fx=
    * 224	fx>=
    * 225	fx>
    * 226	fx<=
    * 227	fx<
    * 228	fx%
    * 229	fx/
    * 230	fx*
    * 231	fx-
    * 232	fx+
    * 233	sumNormalize
    * 234	typeDominator
    * 235	typeLub
    * 236	typeMatches?
    * 237	hasParent?
    * 238	type!=
    * 239	type<>
    * 240	type<
    * 241	type>
    * 242	type=
    * 243	type<=
    * 244	type>=
    * 245	type?
    * 246	multiMethod-lookup-and-invoke
    * 247	monitor-dump
    * 248	monitor-reset
    * 249	monitor-convert
    * 250	monitor-stop
    * 251	monitor-start
    * 252	monitor-new
    * 253	reflective-method-new
    * 254	method-new
    * 255	bitfield00
    * 256	bitfield
    * 257	lexvar
    * 258	set-obo
    * 259	add-obo
    * 260	get-obo
    * 261	lookup-obo
    * 262	contour
    * 263	loc-contour
    * 264	keys
    * 265	prim-unregister-alarm
    * 266	prim-register-alarm
    * 267	prim-handle-alarm
    * 268	io-catch
    * 269	sig-catch
    * 270	tupleexpr-concat
    * 271	tupleexpr-tail
    * 272	tupleexpr-head
    * 273	tupleexpr-split
    * 274	tupleexpr->tuple
    * 275	tupleexpr-new
    * 276	tupleexpr-new-n
    * 277	requestexpr->tuple
    * 278	tupleexpr-basic-new
    * 279	seqexpr-basic-new
    * 280	sendexpr-basic-new
    * 281	requestexpr-basic-new
    * 282	quoteexpr-basic-new
    * 283	reflectivemethodexpr-basic-new
    * 284	procexpr-basic-new
    * 285	methodexpr-basic-new
    * 286	blockexpr-basic-new
    * 287	image-restore
    * 288	image-dump
    * 289	ctxt-resume
    * 290	ctxt-rtn
    * 291	ff-create
    * 292	ff-new
    * 293	unix-resolve
    * 294	wizard-load
    * 295	unix-load
    * 296	csNth
    * 297	>-A-
    * 298	-A->
    * 299	prim-flatten
    * 300	null
    * 301	S-tupleSet
    * 302	S-set
    * 303	S-deref
    * 304	S-desc
    * 305	S-get
    * 306	select
    * 307	primNumber
    * 308	opcode->string
    * 309	code-dump
    * 310	word32vec-new
    * 311	word16vec-new
    * 312	bytevec-new
    * 313	fdAsync
    * 314	async
    * 315	ch->fx
    * 316	ch>
    * 317	ch>=
    * 318	ch!=
    * 319	ch=
    * 320	ch<=
    * 321	ch<
    * 322	fxFormat:
    * 323	next!!
    * 324	next!
    * 325	update!!
    * 326	update!
    * 327	actor-new
    */
  val map = Map(
    65  -> tplTail,
    67  -> tplHead,
    202 -> flPlus,
    232 -> fxPlus,
    324 -> actorNextBang,
    326 -> actorUpdateBang
  )

  def mismatchArgs(state: State, minArgs: Int, maxArgs: Int): Ob = {
    val msg = if (maxArgs == MaxArgs) {
      s"expected $minArgs or more arguments"
    } else if (minArgs == maxArgs) {
      if (minArgs == 1) {
        "expected 1 argument"
      } else {
        s"expected $minArgs arguments"
      }
    } else {
      s"expected between $minArgs and $maxArgs arguments"
    }

    runtimeError(state.ctxt, msg)
  }

  def runtimeError(ctxt: Ctxt, msg: String, xs: Any*): Ob =
    //todo implement this
    Deadthread

  def mismatch(ctxt: Ctxt, argNum: Int, typeName: String): Ob =
    runtimeError(
      ctxt,
      "%d%s argument is not %s %s",
      argNum + 1,
      numberSuffix(argNum + 1),
      properPrep(typeName),
      typeName
    )

  def nthPrim(n: Int): Prim = map(n)

  def mismatchType[T <: Ob: ClassTag](ctxt: Ctxt): Option[TypeMismatch] = {
    val n        = ctxt.nargs
    val typeName = classTag[T].runtimeClass.getName

    val nonT = ctxt.argvec.value.take(n).find {
      case _: T => false
      case _    => true
    }

    nonT.map(ob => TypeMismatch(ctxt.argvec.value.indexOf(ob), typeName))
  }
}
