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
typedef struct {
    IdType id;
    int pbIndex;
    pOb object;
} ObjectInfo;

std::map<IdType, ObjectInfo> objects;

pOb createRosetteObject(const ObjectCodePB::Object & ob) {

    pOb retval = NULL;

    switch(ob.type()) {
    case ObjectCodePB::OT_ACTOR:
        retval = Actor::create();
        break;
    case ObjectCodePB::OT_BLOCK_EXPR:
        retval = BlockExpr::create(NULL);
        break;
    case ObjectCodePB::OT_CHAR:
        retval = RBLCHAR(ob.char_().value());
        break;
    case ObjectCodePB::OT_CODE:
        // Handled separately
        break;
    case ObjectCodePB::OT_EXPANDED_LOCATION:
        // ???
        break;
    case ObjectCodePB::OT_FIXNUM:
        retval = FIXNUM(ob.fixnum().value());
        break;
    case ObjectCodePB::OT_FLOAT:
        retval = Float::create((double)ob.float_().value());
        break;
    case ObjectCodePB::OT_FREE_EXPR:
        retval = FreeExpr::create(NULL, NULL);
        break;
    case ObjectCodePB::OT_GOTO_EXPR:
        retval = GotoExpr::create(SYMBOL(ob.goto_expr().label().symbol().name().c_str()));
        break;
    case ObjectCodePB::OT_IF_EXPR:
        retval = IfExpr::create(NULL, NULL);
        break;
    case ObjectCodePB::OT_LABEL_EXPR:
        retval = LabelExpr::create(SYMBOL(ob.goto_expr().label().symbol().name().c_str()), NULL);
        break;
    case ObjectCodePB::OT_LET_EXPR:
        retval = LetExpr::create(NULL, NULL);
        break;
    case ObjectCodePB::OT_LET_REC_EXPR:
        retval = LetrecExpr::create(NULL, NULL);
        break;
    case ObjectCodePB::OT_METHOD_EXPR:
        retval = MethodExpr::create(NULL, NULL, NULL);
        break;
    case ObjectCodePB::OT_PROC:
        retval = Proc::create(NULL, NULL, NULL, NULL);
        break;
    case ObjectCodePB::OT_PROC_EXPR:
        retval = ProcExpr::create(NULL, NULL, NULL);
        break;
    case ObjectCodePB::OT_QUOTE_EXPR:
        retval = QuoteExpr::create(SYMBOL(ob.quote_expr().expr().symbol().name().c_str()));
        break;
    case ObjectCodePB::OT_RBL_STRING: {
        retval = RBLstring::create((char *)ob.rbl_string().value().c_str());
        break;
    }
    case ObjectCodePB::OT_REFLECTIVE_METHOD_EXPR:
        retval = ReflectiveMethodExpr::create(NIV, NULL,
                    SYMBOL(ob.reflective_method_expr().body().symbol().name().c_str()));
        break;
    case ObjectCodePB::OT_REQUEST_EXPR:
        retval = RequestExpr::create( SYMBOL(ob.request_expr().target().symbol().name().c_str()), NULL);
        break;
    case ObjectCodePB::OT_SEND_EXPR:
        retval = SendExpr::create( SYMBOL(ob.send_expr().target().symbol().name().c_str()), NULL);
        break;
    case ObjectCodePB::OT_SEQ_EXPR:
        retval = SeqExpr::create(NULL);
        break;
    case ObjectCodePB::OT_SET_EXPR:
        retval = SetExpr::create( SYMBOL(ob.set_expr().trgt().symbol().name().c_str()), NULL);
        break;
    case ObjectCodePB::OT_STD_METHOD:
        retval = StdMthd::create(NULL, NULL, NULL);
        break;
    case ObjectCodePB::OT_SYMBOL:
        retval = SYMBOL(ob.symbol().name().c_str());
        break;
    case ObjectCodePB::OT_TBL_OBJECT:
        retval = TblObject::create();
        break;
    case ObjectCodePB::OT_TEMPLATE:
        retval = Template::create(NIL, NILmeta, NILpattern);
        break;
    case ObjectCodePB::OT_TUPLE:
        retval = Tuple::create();
        break;
    case ObjectCodePB::OT_TUPLE_EXPR:
        retval = TupleExpr::create();
        break;
    case ObjectCodePB::OT_RBL_BOOL:
        retval = RBLBOOL(ob.rbl_bool().value());
        break;
    case ObjectCodePB::OT_NIV:
        retval = NIV;
        break;
    case ObjectCodePB::OT_STD_EXTENSION:
        retval = StdExtension::create( ob.std_extension().elements_size());
        break;
    case ObjectCodePB::OT_RBL_ATOM:
        // ???
        break;
    case ObjectCodePB::OT_NULL_EXPR:
        retval = NullExpr::create();
        break;
    case ObjectCodePB::OT_COMPOUND_PATTERN:
        // ???
        break;
    case ObjectCodePB::OT_COMPLEX_PATTERN:
        retval = ComplexPattern::create(NULL);
        break;
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
        retval = IdPattern::create( SYMBOL(ob.id_pattern().symbol().symbol().name().c_str()) );
        break;
    case ObjectCodePB::OT_PRIM:
        retval =  Prim::create(
            (char *)ob.prim().id().symbol().name().c_str(),
            NULL,
            (int)ob.prim().minargs(),
            (int)ob.prim().maxargs());
        break;
    case ObjectCodePB::OT_SYSVAL: {
        std::string val = ob.sysval().value();

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
        retval = ConstPattern::create(FIXNUM(ob.const_pattern().val().fixnum().value()));
        break;
    case ObjectCodePB::OT_RBL_TABLE:
        retval = RblTable::create();
        break;
    default:
        warning("Import object type=%d not yet implemented!", ob.type());
    }

    return retval;
}

pOb linkRosetteObject(ObjectInfo & obi) {

    pOb retval = NULL;

    ObjectCodePB::ObType type = importObjectCode.objects(obi.pbIndex).type();
    switch(type) {
    case ObjectCodePB::OT_ACTOR:
    case ObjectCodePB::OT_BLOCK_EXPR:
    case ObjectCodePB::OT_CHAR:
    case ObjectCodePB::OT_CODE:
    case ObjectCodePB::OT_EXPANDED_LOCATION:
    case ObjectCodePB::OT_FIXNUM:
    case ObjectCodePB::OT_FLOAT:
    case ObjectCodePB::OT_FREE_EXPR:
    case ObjectCodePB::OT_GOTO_EXPR:
    case ObjectCodePB::OT_IF_EXPR:
    case ObjectCodePB::OT_LABEL_EXPR:
    case ObjectCodePB::OT_LET_EXPR:
    case ObjectCodePB::OT_LET_REC_EXPR:
    case ObjectCodePB::OT_METHOD_EXPR:
    case ObjectCodePB::OT_PROC:
    case ObjectCodePB::OT_PROC_EXPR:
    case ObjectCodePB::OT_QUOTE_EXPR:
    case ObjectCodePB::OT_RBL_STRING:
    case ObjectCodePB::OT_REFLECTIVE_METHOD_EXPR:
    case ObjectCodePB::OT_REQUEST_EXPR:
    case ObjectCodePB::OT_SEND_EXPR:
    case ObjectCodePB::OT_SEQ_EXPR:
    case ObjectCodePB::OT_SET_EXPR:
    case ObjectCodePB::OT_STD_METHOD:
    case ObjectCodePB::OT_SYMBOL:
    case ObjectCodePB::OT_TBL_OBJECT:
    case ObjectCodePB::OT_TEMPLATE:
    case ObjectCodePB::OT_TUPLE:
    case ObjectCodePB::OT_TUPLE_EXPR:
    case ObjectCodePB::OT_RBL_BOOL:
    case ObjectCodePB::OT_NIV:
    case ObjectCodePB::OT_STD_EXTENSION:
    case ObjectCodePB::OT_RBL_ATOM:
    case ObjectCodePB::OT_NULL_EXPR:
    case ObjectCodePB::OT_COMPOUND_PATTERN:
    case ObjectCodePB::OT_COMPLEX_PATTERN:
    case ObjectCodePB::OT_STD_META:
    case ObjectCodePB::OT_ID_VEC_PATTERN:
    case ObjectCodePB::OT_ID_AMPR_REST_PATTERN:
    case ObjectCodePB::OT_ID_PATTERN:
    case ObjectCodePB::OT_PRIM:
    case ObjectCodePB::OT_SYSVAL:
    case ObjectCodePB::OT_CONST_PATTERN:
    case ObjectCodePB::OT_RBL_TABLE:
    default:
        warning("Import object type=%d not yet implemented!", type);
    }
    return retval;
}

Code * createRosetteCode(const ObjectCodePB::CodeBlock & cb) {

    return NULL;
}

bool readImportCode() {

    if ('\0' == *ImportFile)
        return false;

fprintf(stderr, "%s", __PRETTY_FUNCTION__);

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

    // Make a map of the objects by id and create the initial top-level objects
    for (int i=0; i<importObjectCode.objects_size(); i++) {
        ObjectInfo obi;
        obi.id = importObjectCode.objects(i).object_id();
        obi.pbIndex = i;
        obi.object = createRosetteObject(importObjectCode.objects(i));
        objects.insert(std::make_pair(obi.id, obi));
    } 

    // Now do a deep dive to link them up
    for (auto obi : objects) {
        linkRosetteObject(obi.second);
    }

    // Process the code blocks and link the litvecs to actual objects
    for (const ObjectCodePB::CodeBlock& cb : importObjectCode.code_block()) {
        Code * code = createRosetteCode(cb);
    } 



    return true;
}


