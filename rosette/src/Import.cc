#include "Ob.h"
#include "Code.h"
#include "Prim.h"
#include "Number.h"
#include "Expr.h"
#include "RblAtom.h"
#include "RBLstring.h"
#include "Proc.h"
#include "Method.h"
#include "Pattern.h"
#include "Table.h"

#include "CommandLine.h"
#include "Export.h"

#include <google/protobuf/text_format.h>
#include <Ob.pb.h>

#include <fstream>
#include <string>
#include <map>

extern pOb NILmeta;
extern CompoundPattern* NILpattern;

ObjectCodePB::ObjectCode importObjectCode;
class ObjectInfo {
public:
    ObjectInfo() : id(0), pbIndex(0), object(NULL) {}
    IdType id;
    int pbIndex;
    pOb object;
};

std::map<IdType, ObjectInfo> objects;
ObjectInfo * getObject(IdType id) {
    auto oi = objects.find(id);
    if (oi != objects.end()) {
        return &oi->second;
    }

    warning("Unable to locate object %llu.", id);
    return NULL;
}

pOb createRosetteObject(ObjectCodePB::Object * ob) {

    pOb retval = NULL;
    ObjectInfo * oi = NULL;

    if (VerboseFlag) {
        fprintf(stderr, "%s: Object=\n%s\n", __PRETTY_FUNCTION__, ob->DebugString().c_str());
    }

    // Is this a regular (not RblAtom) object
    if (ob->object_id() != 0) {

        // Find its info record
        oi = getObject(ob->object_id());
        if (oi->object != NULL) {
            // It's there and already built, return the pointer
            return oi->object;
        }

        if (ob->looped() == true) {  // Object referenced itself
            return oi->object;
        }
    }

    // If this is a reference, find the real object
    if (ob->reference() != 0) {
        ob = importObjectCode.mutable_objects(oi->pbIndex);
        if (VerboseFlag) {
            fprintf(stderr, " Referenced Object=\n%s\n", ob->DebugString().c_str());
        }
    }

    // Create the object by type
    switch(ob->type()) {

    case ObjectCodePB::OT_ACTOR: {
        ObjectCodePB::Actor * pbob = ob->mutable_actor();
        retval = Actor::create();
        // TODO: extension
        break;
    }

    case ObjectCodePB::OT_BLOCK_EXPR: {
        ObjectCodePB::BlockExpr * pbob = ob->mutable_block_expr();
        retval = BlockExpr::create( (Tuple *)createRosetteObject(pbob->mutable_subexprs()),
                                    createRosetteObject(pbob->mutable_implicit()) );
        break;
    }

    case ObjectCodePB::OT_CHAR:
        retval = RBLCHAR(ob->char_().value());
        break;

    case ObjectCodePB::OT_CODE:
        retval = oi->object;
        break;

    case ObjectCodePB::OT_EXPANDED_LOCATION: {
        ObjectCodePB::ExpandedLocation * pbob = ob->mutable_expanded_location();
        Location loc;

        switch(pbob->type()) {
        case LT_CtxtRegister: {
            ObjectCodePB::ExpandedLocation::LocCtxt * ctxt = pbob->mutable_ctxt(); 
            loc = CtxtReg( (CtxtRegName)ctxt->reg() );
            break;
        }

        case LT_ArgRegister: {
            ObjectCodePB::ExpandedLocation::LocArg * arg = pbob->mutable_arg(); 
            loc = ArgReg( arg->arg() );
            break;
        }

        case LT_LexVariable: {
            ObjectCodePB::ExpandedLocation::LocLexVar * lex = pbob->mutable_lexvar(); 
            loc = LexVar( lex->level(), lex->offset(), lex->indirect() );
            break;
        }

        case LT_AddrVariable: {
            ObjectCodePB::ExpandedLocation::LocAddrVar * addr = pbob->mutable_addrvar(); 
            loc = AddrVar( addr->level(), addr->offset(), addr->indirect() );
            break;
        }

        case LT_GlobalVariable: {
            ObjectCodePB::ExpandedLocation::LocGlobalVar * lex = pbob->mutable_globalvar(); 
            loc = GlobalVar( lex->offset() );
            break;
        }

        case LT_BitField: {
            ObjectCodePB::ExpandedLocation::LocBitField * bf = pbob->mutable_bitfield(); 
            loc = BitField( bf->level(), bf->offset(), bf->span(), bf->indirect(), bf->signed_() );
            break;
        }

        case LT_BitField00: {
            ObjectCodePB::ExpandedLocation::LocBitField00 * bf = pbob->mutable_bitfield00(); 
            loc = BitField00( bf->offset(), bf->span(), bf->signed_() );
            break;
        }

        case LT_Limbo: {
            ObjectCodePB::ExpandedLocation::LocLimbo * limbo = pbob->mutable_limbo(); 
            loc = LocLimbo;
            break;
        }

        default:
            warning("Import expanded location");

        }

        ExpandedLocation * el = ExpandedLocation::create();
// TODO: This isn't correct. Figure out the correct way to turn a loc into a ExpandedLocation Object
//        retval = valWrt(loc, el);
        break;
    }

    case ObjectCodePB::OT_FIXNUM:
        retval = FIXNUM(ob->fixnum().value());
        break;

    case ObjectCodePB::OT_FLOAT:
        retval = Float::create((double)ob->float_().value());
        break;

    case ObjectCodePB::OT_FREE_EXPR: {
        ObjectCodePB::FreeExpr * pbob = ob->mutable_free_expr();
        retval = FreeExpr::create( (TupleExpr *)createRosetteObject(pbob->mutable_freeids()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_GOTO_EXPR:
        retval = GotoExpr::create(SYMBOL(ob->goto_expr().label().symbol().name().c_str()));
        break;

    case ObjectCodePB::OT_IF_EXPR: {
        ObjectCodePB::IfExpr * pbob = ob->mutable_if_expr();
        retval = IfExpr::create(    createRosetteObject(pbob->mutable_condition()),
                                    createRosetteObject(pbob->mutable_truebranch()),
                                    createRosetteObject(pbob->mutable_falsebranch()) );
        break;
    }

    case ObjectCodePB::OT_LABEL_EXPR: {
        ObjectCodePB::LabelExpr * pbob = ob->mutable_label_expr();
        retval = LabelExpr::create( SYMBOL(pbob->label().symbol().name().c_str()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_LET_EXPR: {
        ObjectCodePB::LetExpr * pbob = ob->mutable_let_expr();
        retval = LetExpr::create( (TupleExpr *)createRosetteObject(pbob->mutable_bindings()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_LET_REC_EXPR:  {
        ObjectCodePB::LetrecExpr * pbob = ob->mutable_let_rec_expr();
        retval = LetrecExpr::create( (TupleExpr *)createRosetteObject(pbob->mutable_bindings()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_METHOD_EXPR: {
        ObjectCodePB::MethodExpr * pbob = ob->mutable_method_expr();
        retval = MethodExpr::create(    createRosetteObject(pbob->mutable_identity()),
                                    createRosetteObject(pbob->mutable_formals()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_PROC: {
        ObjectCodePB::Proc * pbob = ob->mutable_proc();
        retval = Proc::create(    createRosetteObject(pbob->mutable_env()),
                                    (Code *)createRosetteObject(pbob->mutable_code()),
                                    createRosetteObject(pbob->mutable_id()),
                                    createRosetteObject(pbob->mutable_source()) );
        break;
    }

    case ObjectCodePB::OT_PROC_EXPR: {
        ObjectCodePB::ProcExpr * pbob = ob->mutable_proc_expr();
        retval = ProcExpr::create(    createRosetteObject(pbob->mutable_identity()),
                                    createRosetteObject(pbob->mutable_formals()),
                                    createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_QUOTE_EXPR:
        retval = QuoteExpr::create(SYMBOL(ob->quote_expr().expr().symbol().name().c_str()));
        break;

    case ObjectCodePB::OT_RBL_STRING: {
        retval = RBLstring::create((char *)ob->rbl_string().value().c_str());
        break;
    }

    case ObjectCodePB::OT_REFLECTIVE_METHOD_EXPR: {
        ObjectCodePB::ReflectiveMethodExpr * pbob = ob->mutable_reflective_method_expr();
        retval = ReflectiveMethodExpr::create(  createRosetteObject(pbob->mutable_identity()),
                                                createRosetteObject(pbob->mutable_formals()),
                                                createRosetteObject(pbob->mutable_body()) );
        break;
    }

    case ObjectCodePB::OT_REQUEST_EXPR: {
        ObjectCodePB::RequestExpr * pbob = ob->mutable_request_expr();
        retval = RequestExpr::create(
                    SYMBOL(pbob->target().symbol().name().c_str()),
                    (TupleExpr *)createRosetteObject( pbob->mutable_msg() ));
        break;
    }

    case ObjectCodePB::OT_SEND_EXPR: {
        ObjectCodePB::SendExpr * pbob = ob->mutable_send_expr();
        retval = SendExpr::create(
                    SYMBOL(pbob->target().symbol().name().c_str()),
                    (TupleExpr *)createRosetteObject( pbob->mutable_msg() ));
        break;
    }

    case ObjectCodePB::OT_SEQ_EXPR: {
        ObjectCodePB::SeqExpr * pbob = ob->mutable_seq_expr();
        retval = SeqExpr::create( (Tuple *)createRosetteObject(pbob->mutable_subexprs()) );
        break;
    }

    case ObjectCodePB::OT_SET_EXPR: {
        ObjectCodePB::SetExpr * pbob = ob->mutable_set_expr();
        retval = SetExpr::create( createRosetteObject(pbob->mutable_trgt()),
                                    createRosetteObject(pbob->mutable_val()) );
        break;
    }

    case ObjectCodePB::OT_STD_METHOD: {
        ObjectCodePB::StdMthd * pbob = ob->mutable_std_mthd();
        retval = StdMthd::create( (Code *)createRosetteObject(pbob->mutable_code()),
                                    createRosetteObject(pbob->mutable_id()),
                                    createRosetteObject(pbob->mutable_source()));
        break;
    }

    case ObjectCodePB::OT_SYMBOL:
        retval = SYMBOL(ob->symbol().name().c_str());
        break;

    case ObjectCodePB::OT_TBL_OBJECT: {
        ObjectCodePB::TblObject * pbob = ob->mutable_tbl_object();
        retval = TblObject::create();
        break;
    }

    case ObjectCodePB::OT_TEMPLATE: {
        ObjectCodePB::Template * pbob = ob->mutable_template_();
        retval = Template::create( (Tuple *)createRosetteObject(pbob->mutable_keytuple()),
                                    createRosetteObject(pbob->mutable_keymeta()),
                                    (CompoundPattern *)createRosetteObject(pbob->mutable_pat()) );
        break;
    }

    case ObjectCodePB::OT_TUPLE: {
        ObjectCodePB::Tuple * pbob = ob->mutable_tuple();
        Tuple * tup = Tuple::create(pbob->elements().size(), INVALID);
        for (int i = 0; i < pbob->elements().size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
    }

    case ObjectCodePB::OT_TUPLE_EXPR: {
        ObjectCodePB::TupleExpr * pbob = ob->mutable_tuple_expr();
        TupleExpr * tup = TupleExpr::create();
        tup->rest = createRosetteObject( pbob->mutable_rest() );
    }

    case ObjectCodePB::OT_RBL_BOOL:
        retval = RBLBOOL(ob->rbl_bool().value());
        break;

    case ObjectCodePB::OT_NIV:
        retval = NIV;
        break;

    case ObjectCodePB::OT_STD_EXTENSION: {
        ObjectCodePB::StdExtension * pbob = ob->mutable_std_extension();
        Tuple * tup = Tuple::create(pbob->elements().size(), INVALID);
        for (int i = 0; i < pbob->elements().size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
        retval = StdExtension::create( tup );
    }

    case ObjectCodePB::OT_NULL_EXPR:
        retval = NullExpr::create();
        break;

    case ObjectCodePB::OT_COMPLEX_PATTERN: {
// TODO fix this
        // ObjectCodePB::ComplexPattern * pbob = ob->mutable_complex_pattern();
        // retval = ComplexPattern::create(
        //     (TupleExpr *)createRosetteObject(pbob->mutable_expr()));
        // break;
    }

    case ObjectCodePB::OT_STD_META:
        retval = StdMeta::create(NIL);
        break;

    case ObjectCodePB::OT_ID_VEC_PATTERN:
        retval = IdVecPattern::create(NILexpr);
        break;

    case ObjectCodePB::OT_ID_AMPR_REST_PATTERN:
        retval = IdAmperRestPattern::create(NULL);
        break;

    case ObjectCodePB::OT_ID_PATTERN:
        retval = IdPattern::create( SYMBOL(ob->id_pattern().symbol().symbol().name().c_str()) );
        break;

    case ObjectCodePB::OT_PRIM:
        retval =  Prim::create(
            (char *)ob->prim().id().symbol().name().c_str(),
            NULL,
            (int)ob->prim().minargs(),
            (int)ob->prim().maxargs());
        break;

    case ObjectCodePB::OT_SYSVAL: {
        std::string val = ob->sysval().value();

        if (val == "#inv")
            retval = INVALID;
        else if (val == "#upcall")
            retval = UPCALL;
        else if (val == "#suspended")
            retval = SUSPENDED;
        else if (val == "#interrupt")
            retval = INTERRUPT;
        else if (val == "#sleep")
            retval = SLEEP;
        else if (val == "#deadThread")
            retval = DEADTHREAD;
        break;
    }

    case ObjectCodePB::OT_CONST_PATTERN:
        retval = ConstPattern::create(FIXNUM(ob->const_pattern().val().fixnum().value()));
        break;

    case ObjectCodePB::OT_RBL_TABLE:
        retval = RblTable::create();
        break;

    default:
        warning("Import object type=%d not yet implemented!", ob->type());
    }

    // Save the result in the objects table so we don't create another next time this object is referenced.
    if (oi != NULL) {
        oi->object = retval;
    }
    return retval;
}


Code * createRosetteCode(ObjectCodePB::CodeBlock * cb) {
fprintf(stderr, "%s\n", __PRETTY_FUNCTION__);

    // Create the CodeBuf and CodeVec objects, then copy in the opcodes. 
    int codesize = cb->codevec().opcodes().size();   // In bytes
    CodeBuf * cbuf = CodeBuf::create();
    cbuf->growCodevec( codesize / sizeof(Instr) );  // Set the size in 16 bit words
    char * code_binary = (char *)cbuf->codevec->absolutize(0);  // Pointer to the buffer to hold the code
    memcpy(code_binary, cb->codevec().opcodes().data(), codesize);

    // Create the LitVec object and populate it from the protobuf objects
    Tuple * litvec = Tuple::create(cb->litvec().ob().size(), INVALID);
    for (int i=0; i<cb->litvec().ob().size(); i++) {
        ObjectCodePB::Object * pbob = cb->mutable_litvec()->mutable_ob(i);
        litvec->setNth(i, createRosetteObject(pbob));
    }

    Code * code = Code::create(cbuf, litvec);
    return code;
}

bool readImportCode() {

    if ('\0' == *ImportFile)
        return false;

    if (VerboseFlag) {
        fprintf(stderr, "%s\n", __PRETTY_FUNCTION__);
    }

    // Read the Object Code from disk
    std::fstream input(ImportFile, std::ios::in | std::ios::binary);
    if (!input) {
        warning("Import File %s not found.", ImportFile);
    } else if (!importObjectCode.ParseFromIstream(&input)) {
        warning("Failed to parse Object Code.");
        return false;
    }

    // Text output of the protobuf for debugging
    if (VerboseFlag) {
        std::string s;
        google::protobuf::TextFormat::PrintToString( importObjectCode, &s );

        fprintf(stderr, "*** Imported ObjectCode = \n%s\n", s.c_str());
        fprintf(stderr, "object count = %d\n", importObjectCode.objects_size());
        fprintf(stderr, "code count = %d\n", importObjectCode.code_block_size());
    }

    // Make a map of the objects by id
    for (int i=0; i<importObjectCode.objects_size(); i++) {
        ObjectInfo obi;
        obi.id = importObjectCode.objects(i).object_id();
        obi.pbIndex = i;
        obi.object = NULL;

        objects.insert(std::make_pair(obi.id, obi));
    } 

    // Process the code blocks and link the litvecs to actual objects
    for (int i=0; i<importObjectCode.code_block_size(); i++) {
        ObjectCodePB::CodeBlock * ob = importObjectCode.mutable_code_block(i);
        Code * code = createRosetteCode(ob);

        ObjectInfo obi;
        obi.id = ob->object_id();
        obi.pbIndex = -1;
        obi.object = NULL;

        objects.insert(std::make_pair(obi.id, obi));
    } 

    return true;
}


