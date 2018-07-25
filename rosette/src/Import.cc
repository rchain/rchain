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
#include "Vm.h"
#include "MI.h"
#include "Cstruct.h"
#include "Method.h"
#include "Operation.h"

#include "CommandLine.h"
#include "Export.h"

#include <google/protobuf/text_format.h>
#include <Ob.pb.h>

#include <fstream>
#include <string>
#include <map>

extern pOb NILmeta;
extern CompoundPattern* NILpattern;

// This is the locally imported Object Code Protobuf
ObjectCodePB::ObjectCode importObjectCode;

// This is an entry in the table of Rosette Objects
class ObjectInfo {
public:
    ObjectInfo() : id(0), pbIndex(0), object(NULL) {}
    IdType id;
    int pbIndex;
    pOb object;
};

// The table of Rosette Objects
std::map<IdType, ObjectInfo> objects;
ObjectInfo * getObject(IdType id) {
    auto oi = objects.find(id);
    if (oi != objects.end()) {
        return &oi->second;
    }

    warning("Unable to locate object %llu.", id);
    return NULL;
}

// When a Rosette object is referenced, this routine is called to either find the previously
// created object, or to create the object. If this object references other objects, this
// routine is called recursively to find or create them.
pOb createRosetteObject(ObjectCodePB::Object * ob) {

    pOb retval = NULL;
    ObjectInfo * oi = NULL;

    if (VerboseFlag) {
        fprintf(stderr, "%s:\nObject=\n%s\n", __PRETTY_FUNCTION__, ob->DebugString().c_str());
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
    if (ob->reference()) {
        if (oi->pbIndex >= 0) {
            ob = importObjectCode.mutable_objects(oi->pbIndex);
            if (VerboseFlag) {
                fprintf(stderr, " Referenced Object=\n%s\n", ob->DebugString().c_str());
            }
        } else {
            // It's an already built code object.
            return oi->object;
        }
    }

    pOb meta = INVALID;
    pOb parent = INVALID;
    if (ob->has_meta()) {
        meta = createRosetteObject(ob->mutable_meta());
    }
    if (ob->has_parent()) {
        parent = createRosetteObject(ob->mutable_parent());
    }

    // Create the object by type
    switch(ob->type()) {

    case ObjectCodePB::OT_ACTOR: {
        ObjectCodePB::Actor * pbob = ob->mutable_actor();
        ObjectCodePB::StdExtension * pbse = pbob->mutable_extension()->mutable_std_extension();

        // An Actor references a StdExtension which is similar to a tuple which is a list of objects.
        Tuple * tup = Tuple::create(pbse->elements_size(), INVALID);
        for (int i = 0; i < pbse->elements_size(); i++) {
            BASE(tup)->setNth( i, createRosetteObject(pbse->mutable_elements(i)) );
        }

        retval = Actor::create( meta, parent, StdExtension::create( tup ));
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
        // Code objects are previously build by createRosetteCode(), and the pointer
        // should be returned above.
        suicide("Referenced unbuilt code object id=%llu\n", ob->object_id());
        break;

    case ObjectCodePB::OT_EXPANDED_LOCATION: {
        ObjectCodePB::ExpandedLocation * pbob = ob->mutable_expanded_location();
        ObjectCodePB::Location *pbloc = pbob->mutable_loc();
        Location loc;

        switch(pbloc->type()) {
        case LT_CtxtRegister: {
            ObjectCodePB::Location::LocCtxt * ctxt = pbloc->mutable_ctxt(); 
            loc = CtxtReg( (CtxtRegName)ctxt->reg() );
            break;
        }

        case LT_ArgRegister: {
            ObjectCodePB::Location::LocArg * arg = pbloc->mutable_arg(); 
            loc = ArgReg( arg->arg() );
            break;
        }

        case LT_LexVariable: {
            ObjectCodePB::Location::LocLexVar * lex = pbloc->mutable_lexvar(); 
            loc = LexVar( lex->level(), lex->offset(), lex->indirect() );
            break;
        }

        case LT_AddrVariable: {
            ObjectCodePB::Location::LocAddrVar * addr = pbloc->mutable_addrvar(); 
            loc = AddrVar( addr->level(), addr->offset(), addr->indirect() );
            break;
        }

        case LT_GlobalVariable: {
            ObjectCodePB::Location::LocGlobalVar * lex = pbloc->mutable_globalvar(); 
            loc = GlobalVar( lex->offset() );
            break;
        }

        case LT_BitField: {
            ObjectCodePB::Location::LocBitField * bf = pbloc->mutable_bitfield(); 
            loc = BitField( bf->level(), bf->offset(), bf->span(), bf->indirect(), bf->signed_() );
            break;
        }

        case LT_BitField00: {
            ObjectCodePB::Location::LocBitField00 * bf = pbloc->mutable_bitfield00(); 
            loc = BitField00( bf->offset(), bf->span(), bf->signed_() );
            break;
        }

        case LT_Limbo: {
            ObjectCodePB::Location::LocLimbo * limbo = pbloc->mutable_limbo(); 
            loc = LocLimbo;
            break;
        }

        default:
            warning("Import expanded location");

        }

        // An ExpandedLocation is an RblAtom
        retval = (pOb)loc.atom;
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
        Tuple * tup = Tuple::create(pbob->elements_size(), INVALID);
        for (int i = 0; i < pbob->elements_size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
        retval = tup;
        break;
    }

    case ObjectCodePB::OT_TUPLE_EXPR: {
        ObjectCodePB::TupleExpr * pbob = ob->mutable_tuple_expr();
        TupleExpr * tup = TupleExpr::create(pbob->elements_size());
        tup->rest = createRosetteObject( pbob->mutable_rest() );

        // Create the list of objects contained in the TupleExpr
        for (int i = 0; i < pbob->elements_size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }

        retval = tup;
        break;
    }

    case ObjectCodePB::OT_RBL_BOOL:
        retval = RBLBOOL(ob->rbl_bool().value());
        break;

    case ObjectCodePB::OT_NIV:
        retval = NIV;
        break;

    case ObjectCodePB::OT_STD_EXTENSION: {
        ObjectCodePB::StdExtension * pbob = ob->mutable_std_extension();

        // A StdExtension is similar to a tuple which is a list of objects.
        Tuple * tup = Tuple::create(pbob->elements_size(), INVALID);
        for (int i = 0; i < pbob->elements_size(); i++) {
            BASE(tup)->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }

        retval = StdExtension::create( tup );
        break;
    }

    case ObjectCodePB::OT_NULL_EXPR:
        retval = NullExpr::create();
        break;

    case ObjectCodePB::OT_COMPLEX_PATTERN: {
        ObjectCodePB::ComplexPattern * pbob = ob->mutable_complex_pattern();

        void* loc = PALLOC(sizeof(ComplexPattern));
        retval = new (loc) ComplexPattern(
                            (TupleExpr *)createRosetteObject(pbob->mutable_expr()),
                            (Tuple *)createRosetteObject(pbob->mutable_patvec()),
                            (Tuple *)createRosetteObject(pbob->mutable_offsetvec()) );
        break;
    }

    case ObjectCodePB::OT_STD_META: {
        ObjectCodePB::StdMeta * pbob = ob->mutable_std_meta();
        retval = StdMeta::create( (Tuple *)createRosetteObject(pbob->mutable_extension()) );
        break;
    }

    case ObjectCodePB::OT_ID_VEC_PATTERN: {
        ObjectCodePB::IdVecPattern * pbob = ob->mutable_id_vec_pattern();
        retval = IdVecPattern::create( (TupleExpr *)createRosetteObject(pbob->mutable_expr()) );
        break;
    }

    case ObjectCodePB::OT_ID_AMPR_REST_PATTERN: {
        ObjectCodePB::IdAmprRestPattern * pbob = ob->mutable_id_ampr_rest_pattern();
        retval = IdAmperRestPattern::create( (TupleExpr *)createRosetteObject(pbob->mutable_expr()) );
        break;
    }

    case ObjectCodePB::OT_ID_PATTERN:
        retval = IdPattern::create( SYMBOL(ob->id_pattern().symbol().symbol().name().c_str()) );
        break;

    case ObjectCodePB::OT_PRIM:
        retval =  Prim::create(
            (char *)ob->prim().id().symbol().name().c_str(),
            NULL,   // TODO: Somehow populate this from the primnum()
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

    case ObjectCodePB::OT_RBL_TABLE: {
        ObjectCodePB::RblTable * pbob = ob->mutable_rbl_table();
        retval = RblTable::create( (Tuple *)createRosetteObject(pbob->mutable_tbl()) );
        break;
    }

    case ObjectCodePB::OT_INDEXED_META: {
        ObjectCodePB::IndexedMeta * pbob = ob->mutable_indexed_meta();

        // TODO: Figure this out
        // StdMeta * sm = StdMeta::create( (Tuple *)createRosetteObject( pbob->mutable_extension()) );
        // retval = 

        break;
    }
    case ObjectCodePB::OT_TOP_ENV: {
        ObjectCodePB::TopEnv * pbob = ob->mutable_top_env();
        retval = RBLtopenv::create();
        break;
    }

    case ObjectCodePB::OT_MI_ACTOR: {
        ObjectCodePB::MIActor * pbob = ob->mutable_mi_actor();

        // A MIActor has a tuple extension
        Tuple * tup = Tuple::create( (Tuple *)createRosetteObject(pbob->mutable_extension()) );
        retval = MIActor::create( tup );
        break;
    }
    
    case ObjectCodePB::OT_ATOMIC_DESCRIPTOR: {
        ObjectCodePB::AtomicDescriptor * pbob = ob->mutable_atomic_descriptor();

        AtomicDescriptor * ad = AtomicDescriptor::create();
        ad->_offset = pbob->_offset();
        ad->_align_to = pbob->_align_to();
        ad->_size = pbob->_size();
        ad->mnemonic = createRosetteObject(pbob->mutable_mnemonic());
        ad->imported = createRosetteObject(pbob->mutable_imported());
        ad->freeStructOnGC = createRosetteObject(pbob->mutable_freestructongc());
        ad->_signed = (RblBool *)createRosetteObject(pbob->mutable__signed());

        retval = ad;
        break;
    }
    
    case ObjectCodePB::OT_REFLECTIVE_MTHD: {
        ObjectCodePB::ReflectiveMthd * pbob = ob->mutable_reflective_mthd();
        retval = ReflectiveMthd::create( (Code *)createRosetteObject(pbob->mutable_code()),
                                                createRosetteObject(pbob->mutable_id()),
                                                createRosetteObject(pbob->mutable_source()));

        break;
    }
    
    case ObjectCodePB::OT_PRODUCT_TYPE: {
        ObjectCodePB::ProductType * pbob = ob->mutable_product_type();

        Tuple * tup = Tuple::create(pbob->elements_size(), INVALID);
        for (int i = 0; i < pbob->elements_size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
        retval = ProductType::create( tup, NIV );
        break;
    }
    
    case ObjectCodePB::OT_SUM_TYPE: {
        ObjectCodePB::SumType * pbob = ob->mutable_sum_type();

        Tuple * tup = Tuple::create(pbob->elements_size(), INVALID);
        for (int i = 0; i < pbob->elements_size(); i++) {
            tup->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
        retval = SumType::create( tup );
        break;
    }
    
    case ObjectCodePB::OT_MULTI_METHOD: {
        ObjectCodePB::MultiMethod * pbob = ob->mutable_multi_method();

        MultiMethod * mm = MultiMethod::create();

        // A StdExtension is similar to a tuple which is a list of objects.
        Tuple * tup = Tuple::create(pbob->elements_size(), INVALID);
        for (int i = 0; i < pbob->elements_size(); i++) {
            BASE(tup)->setNth( i, createRosetteObject(pbob->mutable_elements(i)) );
        }
        mm->extension = StdExtension::create( tup );

        retval = mm;
        break;
    }

    case ObjectCodePB::OT_STD_OPRN: {
        ObjectCodePB::StdOprn * pbob = ob->mutable_std_oprn();

        // TODO: fix these parameters
        retval = StdOprn::create(INVALID, RBLBOOL(false));
    }

    default:
        warning("Import object type=%d not yet implemented!\nObjectPB=\n%s\n", ob->type(), ob->DebugString().c_str());
        assert(false);
    }

    // Handle meta and parent fields. Not for RblAtom objects.
    if (retval && TAG(retval) == OTptr) {
        retval->meta() = meta;
        retval->parent() = parent;
    }

    // Save the result in the objects table so we don't create another next time this object is referenced.
    if (oi != NULL) {
        oi->object = retval;
    }
    return retval;
}

// This routine creates a Rosette Code object along with its respective CodeVec and LitVec
Code * createRosetteCode(ObjectCodePB::CodeBlock * cb) {

    if (VerboseFlag) {
        fprintf(stderr, "%s:\nCode=\n%s\n", __PRETTY_FUNCTION__, cb->DebugString().c_str());
    }

    // Create the CodeBuf object, then copy in the opcodes. 
    int codewords = cb->codevec().opcodes().size();   // In words
    CodeBuf * cbuf = CodeBuf::create(); // Create an empty CodeBuf

    // Import the object code
    ObjectCodePB::CodeVec * cv = cb->mutable_codevec();
    for(int i=0; i<codewords; i++) {
        Instr ins;
        ins.word = cv->opcodes(i);
        cbuf->deposit( ins );
    }

    // Create the LitVec object and populate it from the protobuf objects
    Tuple * litvec = Tuple::create(cb->litvec().ob_size(), INVALID);
    ObjectCodePB::LitVec * pblv = cb->mutable_litvec();
    for (int i=0; i<pblv->ob_size(); i++) {
        ObjectCodePB::Object * pbob = pblv->mutable_ob(i);
        litvec->setNth(i, createRosetteObject(pbob));
    }

    // Create the Code object with the CodeBuf and LitVec
    Code * code = Code::create(cbuf, litvec);

    // if (VerboseFlag) {
    //     fprintf(stderr, "Built code (%llu)=\n", cb->object_id());
    //     code->dumpOn(stderr);
    // }

    return code;
}

// This is the main driver for object code import
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

    // Make a map of the objects by id. The pointer to the objects will be added
    // later as they are referenced and created by a LitVec entry within a code block.
    for (int i=0; i<importObjectCode.objects_size(); i++) {
        ObjectInfo obi;
        obi.id = importObjectCode.objects(i).object_id();
        obi.pbIndex = i;
        obi.object = NULL;

        objects.insert(std::make_pair(obi.id, obi));
    } 

    // Process the code blocks and link the litvec objects to actual objects
    for (int i=0; i<importObjectCode.code_block_size(); i++) {
        ObjectCodePB::CodeBlock * ob = importObjectCode.mutable_code_block(i);

        // Create the Rosette Code object. This is where the LitVec is created
        //  as well as the Rosette data objects referenced therein.
        Code * code = createRosetteCode(ob);

        // Save the code object in the table for future references
        ObjectInfo * oi = getObject(ob->object_id());
        if (oi != NULL) {
            // It's there, set the pointer
            oi->object = code;
        } else {
            // Add it to the table
            ObjectInfo obi;
            obi.id = importObjectCode.objects(i).object_id();
            obi.pbIndex = -1;
            obi.object = code;

            objects.insert(std::make_pair(obi.id, obi));
        }

        // TODO: Here is where we should execute the code.
        // Somehow get a Ctxt created, and call into VirtualMachine::execute()
        // More research is needed.

    } 

    return true;
}


