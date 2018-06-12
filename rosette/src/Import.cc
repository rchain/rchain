#include "Ob.h"
#include "Code.h"
#include "Prim.h"
#include "Number.h"
#include "Expr.h"
#include "RblAtom.h"
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


ObjectCodePB::ObjectCode importObjectCode;

pOb createRosetteObject(const ObjectCodePB::Object & ob) {

    switch(ob.type()) {
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
        warning("Import object type=%d not yet implemented!", ob.type());
    }
    return NULL;
}

Code * createRosetteCode(const ObjectCodePB::CodeBlock & cb) {

    return NULL;
}

bool readImportCode() {

    if ('\0' == *ImportFile)
        return false;

fprintf(stderr, "%s", __PRETTY_FUNCTION__);

    // Read the existing address book.
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

    for (const ObjectCodePB::Object& ob : importObjectCode.objects()) {
        pOb rob = createRosetteObject(ob);
    } 


    for (const ObjectCodePB::CodeBlock& cb : importObjectCode.code_block()) {
        Code * code = createRosetteCode(cb);
    } 



    return true;
}


