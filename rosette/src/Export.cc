#include "Ob.h"
#include "Code.h"
#include "Prim.h"
#include "Number.h"
#include "Expr.h"
#include "RblAtom.h"
#include "Proc.h"
#include "Method.h"

#include "CommandLine.h"

#include <google/protobuf/text_format.h>
#include <Ob.pb.h>

#include <fstream>
#include <string>
#include <map>

// The following functions and table orchestrate the handling of
// Rosette Object export within the litvec portion of the exported object code.
// To add support for an additional object type:
//  - define a protobuf object for it in Ob.proto
//  - write a handler that populates the protobuf from the Rosette Ob
//  - add it to the handlers table.
//

typedef std::string ExportObjectKey;
typedef void (*ExportObjectHandler)(ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype);
typedef ObjectCodePB::ObType ExportObjectType;
extern std::map<ExportObjectKey, std::pair<ExportObjectHandler, ExportObjectType> >  handlers;

// Handlers to populate specific objects
void defaultObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
}

// This function looks up an object in the handlers table based on its type string
// and then calls the appropriate handler to populate it.
void populateObjectByType(pOb ob, ObjectCodePB::Object *exOb) {

    exOb->set_object_id(BASE(ob)->objectId);

    std::string type = BASE(ob)->typestring();
    auto oh = handlers.find(type);
    if (oh != handlers.end()) {
        ExportObjectHandler handler = oh->second.first;
        ExportObjectType obType = oh->second.second;

        exOb->set_type(obType);
        exOb->set_meta_id(BASE(ob)->meta()->objectId);
        exOb->set_parent_id(BASE(ob)->parent()->objectId);

        handler(exOb, ob, obType);
    } else {
        warning("Exporting object type %s not yet implemented!", type.c_str());
        exOb->set_type(ObjectCodePB::OT_UNKNOWN);
        defaultObjectHandler(exOb, ob, ObjectCodePB::OT_UNKNOWN);
    }
}

// The following functions are the handlers that populate object specific fields in the
// protobuf representations of the Rosette objects.

void fixnumObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Fixnum *fn = lvob->mutable_fixnum();
    fn->set_value(FIXVAL(ob));
}

void charObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Char *ch = lvob->mutable_char_();
    ch->set_value(CHARVAL(ob));
}

void codeObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
}

void floatObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Float *fl = lvob->mutable_float_();
    fl->set_value(((Float*)(ob))->val);
}

void symbolObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Symbol *sym = lvob->mutable_symbol();
    sym->set_name(BASE(ob)->asPathname());
}

void rblboolObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::RBLBool *bl = lvob->mutable_rbl_bool();
    bl->set_value(RBLBOOL(ob));
}
void rblstringObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::RBLstring *str = lvob->mutable_rbl_string();
    str->set_value(BASE(ob)->asPathname());
}

void tupleObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype) {
    ObjectCodePB::Tuple *pbTup = lvob->mutable_tuple();
    Tuple * tup = (Tuple *)ob;

    // A tuple is a list of objects.  Retrieve and call the handler for each of them.
    for (int i = 0; i < tup->numberOfElements(); i++) {
        pOb ob = tup->elem(i);
        std::string type = BASE(tup->elem(i))->typestring();
        ObjectCodePB::Object *elOb = pbTup->add_elements();
        populateObjectByType(ob, elOb);
    }
}

void blockExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::BlockExpr *blob = lvob->mutable_block_expr();
    BlockExpr * be = (BlockExpr *)ob;

    ObjectCodePB::Object *subexprs = blob->mutable_subexprs();
    populateObjectByType(be->subExprs, subexprs);

    ObjectCodePB::Object *implicit = blob->mutable_implicit();
    populateObjectByType(be->implicit, implicit);
}

void actorObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Actor *actob = lvob->mutable_actor();
    Actor * act = (Actor *)ob;

    ObjectCodePB::Object *extension = actob->mutable_extension();
    populateObjectByType(act->extension, extension);
}

void nivObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Niv *nivob = lvob->mutable_niv();
}

void stdExtensionObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::StdExtension *sextob = lvob->mutable_std_extension();
}

void expandedLocationObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ExpandedLocation *exlocob = lvob->mutable_expanded_location();
}

void freeExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::FreeExpr *feob = lvob->mutable_free_expr();
    FreeExpr * fe = (FreeExpr *)ob;

    ObjectCodePB::Object *freeids = feob->mutable_freeids();
    populateObjectByType(fe->freeIds, freeids);

    ObjectCodePB::Object *body = feob->mutable_body();
    populateObjectByType(fe->body, body);
}

void gotoExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::GotoExpr *gtob = lvob->mutable_goto_expr();
    GotoExpr * ge = (GotoExpr *)ob;

    ObjectCodePB::Object *label = gtob->mutable_label();
    populateObjectByType(ge->label, label);
}

void ifExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::IfExpr *ifob = lvob->mutable_if_expr();
    IfExpr * ie = (IfExpr *)ob;

    ObjectCodePB::Object *condition = ifob->mutable_condition();
    populateObjectByType(ie->condition, condition);

    ObjectCodePB::Object *trueBranch = ifob->mutable_truebranch();
    populateObjectByType(ie->trueBranch, trueBranch);

    ObjectCodePB::Object *falseBranch = ifob->mutable_falsebranch();
    populateObjectByType(ie->falseBranch, falseBranch);
}

void labelExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::LabelExpr *leob = lvob->mutable_label_expr();
    LabelExpr * le = (LabelExpr *)ob;

    ObjectCodePB::Object *label = leob->mutable_label();
    populateObjectByType(le->label, label);

    ObjectCodePB::Object *body = leob->mutable_body();
    populateObjectByType(le->body, body);
}

void letExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::LetExpr *leob = lvob->mutable_let_expr();
    LetExpr * le = (LetExpr *)ob;

    ObjectCodePB::Object *bindings = leob->mutable_bindings();
    populateObjectByType(le->bindings, bindings);

    ObjectCodePB::Object *body = leob->mutable_body();
    populateObjectByType(le->body, body);
}

void tupleExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::TupleExpr *teob = lvob->mutable_tuple_expr();
    TupleExpr * te = (TupleExpr *)ob;

    // There are cases where the TupleExpr references itself in the "rest" field. This could
    // cause an endless loop resulting in a crash. We catch that case here and set "looped"
    // to true, thus avoiding going down the rabbit hole.

    ObjectCodePB::Object *rest = teob->mutable_rest();
    if (te->rest != ob) {
        populateObjectByType(te->rest, rest);
    } else {
        rest->set_object_id(BASE(te->rest)->objectId);
        rest->set_looped(true);
    }
}

void letRecExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::LetrecExpr *lreob = lvob->mutable_let_rec_expr();
}

void methodExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::MethodExpr *meob = lvob->mutable_method_expr();
    MethodExpr * me = (MethodExpr *)ob;

    ObjectCodePB::Object *identity = meob->mutable_identity();
    populateObjectByType(me->identity, identity);

    ObjectCodePB::Object *formals = meob->mutable_formals();
    populateObjectByType(me->formals, formals);

    ObjectCodePB::Object *body = meob->mutable_body();
    populateObjectByType(me->body, body);
}

void nullExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::NullExpr *neob = lvob->mutable_null_expr();
}

void procObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Proc *pob = lvob->mutable_proc();
    Proc * p = (Proc *)ob;

    ObjectCodePB::Object *env = pob->mutable_env();
    populateObjectByType(p->env, env);

    ObjectCodePB::Object *code = pob->mutable_code();
    populateObjectByType(p->code, code);

    ObjectCodePB::Object *id = pob->mutable_id();
    populateObjectByType(p->id, id);

    ObjectCodePB::Object *source = pob->mutable_source();
    populateObjectByType(p->source, source);

}

void procExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ProcExpr *peob = lvob->mutable_proc_expr();
}

void quoteExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::QuoteExpr *qeob = lvob->mutable_quote_expr();
    QuoteExpr * qe = (QuoteExpr *)ob;

    ObjectCodePB::Object *expr = qeob->mutable_expr();
    populateObjectByType(qe->expr, expr);
}

void rblAtomExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::RBLAtom *raob = lvob->mutable_rbl_atom();
}

void reflectiveMethodExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ReflectiveMethodExpr *rmeob = lvob->mutable_reflective_method_expr();
}

void requestExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::RequestExpr *reob = lvob->mutable_request_expr();
    RequestExpr * re = (RequestExpr *)ob;

    ObjectCodePB::Object *target = reob->mutable_target();
    populateObjectByType(re->target, target);

    ObjectCodePB::Object *msg = reob->mutable_msg();
    populateObjectByType(re->msg, msg);
}

void sendExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::SendExpr *seob = lvob->mutable_send_expr();
}

void seqExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::SeqExpr *seob = lvob->mutable_seq_expr();
    SeqExpr * se = (SeqExpr *)ob;

    ObjectCodePB::Object *subexprs = seob->mutable_subexprs();
    populateObjectByType(se->subExprs, subexprs);
}

void setExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::SetExpr *seob = lvob->mutable_set_expr();
    SetExpr * se = (SetExpr *)ob;

    ObjectCodePB::Object *trgt = seob->mutable_trgt();
    populateObjectByType(se->trgt, trgt);

    ObjectCodePB::Object *val = seob->mutable_val();
    populateObjectByType(se->val, val);
}

void stdMthdObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::StdMthd *smob = lvob->mutable_std_mthd();
    StdMthd * sm = (StdMthd *)ob;

    ObjectCodePB::Object *code = smob->mutable_code();
    populateObjectByType(sm->code, code);

    ObjectCodePB::Object *id = smob->mutable_id();
    populateObjectByType(sm->id, id);

    ObjectCodePB::Object *source = smob->mutable_source();
    populateObjectByType(sm->source, source);

}

void tblObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::TblObject *tblob = lvob->mutable_tbl_object();
    TblObject * to = (TblObject *)ob;

    ObjectCodePB::Object *validextent = tblob->mutable_validextent();
    populateObjectByType(to->validExtent, validextent);

    ObjectCodePB::Object *keyvec = tblob->mutable_keyvec();
    populateObjectByType(to->keyVec, keyvec);
}

void templateExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Template *tob = lvob->mutable_template_();
}




// The handler table that defines which objects are supported, and how they are handled.
// The string type names here MUST match those implemented in Rosette. And yes, there
// is some inconsistency (e.g. RBL vs Rbl).

std::map<ExportObjectKey, std::pair<ExportObjectHandler, ExportObjectType> >  handlers = {
    {"Actor",               {actorObjectHandler,     ObjectCodePB::OT_ACTOR} },
    {"BlockExpr",           {blockExprObjectHandler, ObjectCodePB::OT_BLOCK_EXPR} },
    {"Char",                {charObjectHandler,      ObjectCodePB::OT_CHAR} },
    {"Code",                {codeObjectHandler,      ObjectCodePB::OT_CODE} },
    {"ExpandedLocation",    {expandedLocationObjectHandler, ObjectCodePB::OT_EXPANDED_LOCATION} },
    {"Fixnum",              {fixnumObjectHandler,    ObjectCodePB::OT_FIXNUM} },
    {"Float",               {floatObjectHandler,     ObjectCodePB::OT_FLOAT} },
    {"FreeExpr",            {freeExprObjectHandler,  ObjectCodePB::OT_FREE_EXPR} },
    {"GotoExpr",            {gotoExprObjectHandler,  ObjectCodePB::OT_GOTO_EXPR} },
    {"IfExpr",              {ifExprObjectHandler,    ObjectCodePB::OT_IF_EXPR} },
    {"LabelExpr",           {labelExprObjectHandler, ObjectCodePB::OT_LABEL_EXPR} },
    {"LetExpr",             {letExprObjectHandler,   ObjectCodePB::OT_LET_EXPR} },
    {"LetrecExpr",          {letRecExprObjectHandler, ObjectCodePB::OT_LET_REC_EXPR} },
    {"MethodExpr",          {methodExprObjectHandler, ObjectCodePB::OT_METHOD_EXPR} },
    {"Niv",                 {nivObjectHandler,       ObjectCodePB::OT_NIV}},
    {"NullExpr",            {nullExprObjectHandler,  ObjectCodePB::OT_NULL_EXPR}},
    {"Proc",                {procObjectHandler,      ObjectCodePB::OT_PROC} },
    {"ProcExpr",            {procExprObjectHandler,  ObjectCodePB::OT_PROC_EXPR} },
    {"QuoteExpr",           {quoteExprObjectHandler, ObjectCodePB::OT_QUOTE_EXPR} },
    {"RblAtom",             {rblAtomExprObjectHandler, ObjectCodePB::OT_RBL_ATOM}},
    {"RblBool",             {rblboolObjectHandler,   ObjectCodePB::OT_RBL_BOOL}},
    {"RBLstring",           {rblstringObjectHandler, ObjectCodePB::OT_RBL_STRING} },
    {"ReflectiveMethodExpr",{reflectiveMethodExprObjectHandler, ObjectCodePB::OT_REFLECTIVE_METHOD_EXPR} },
    {"RequestExpr",         {requestExprObjectHandler, ObjectCodePB::OT_REQUEST_EXPR} },
    {"SendExpr",            {sendExprObjectHandler,  ObjectCodePB::OT_SEND_EXPR} },
    {"SeqExpr",             {seqExprObjectHandler,   ObjectCodePB::OT_SEQ_EXPR} },
    {"SetExpr",             {setExprObjectHandler,   ObjectCodePB::OT_SET_EXPR} },
    {"StdExtension",        {stdExtensionObjectHandler, ObjectCodePB::OT_STD_EXTENSION} },
    {"StdMthd",             {stdMthdObjectHandler,   ObjectCodePB::OT_STD_METHOD} },
    {"Symbol",              {symbolObjectHandler,    ObjectCodePB::OT_SYMBOL} },
    {"TblObject",           {tblObjectHandler,       ObjectCodePB::OT_TBL_OBJECT} },
    {"Template",            {templateExprObjectHandler, ObjectCodePB::OT_TEMPLATE} },
    {"Tuple",               {tupleObjectHandler,     ObjectCodePB::OT_TUPLE} },
    {"TupleExpr",           {tupleExprObjectHandler, ObjectCodePB::OT_TUPLE_EXPR} },
};

IdType createObjectId(Code * code) {
    // Rosette internally uses the object pointer. We can't just use the pointer
    // to the object as a unique id when exporting, because when an object goes
    // out of scope its memory is reused. Therefore we create a unique id for each
    // object as it's constructed.

    // Here we will use the newly invented object ID that is generated when the
    // object is created by Rosette

    return BASE(code)->objectId;
}



// This is the protobuf that contains exported object code.
ObjectCodePB::ObjectCode objectCode;


// The collection routine that processes individual Code objects as they are compiled.
void collectExportCode(Code *code) {
    if (VerboseFlag) fprintf(stderr, "\n%s\n", __PRETTY_FUNCTION__);

    IdType codeId = createObjectId( code );

    if (VerboseFlag) {
        fprintf(stderr, "CodeId=%llu\n", codeId);
        code->dumpOn(stderr);
    }

    if ('\0' == *ExportFile)
        return;

    // Build the code block protobuf to export
    ObjectCodePB::CodeBlock * cb = objectCode.add_code_block();

    // Save the code id
   cb->set_object_id( codeId );

    // Get access to the codevec and litvec to be exported.
    CodeVec * codevec = code->codevec;
    Tuple * litvec = code->litvec;

    // Save the binary opCodes from the codevec
    const char * code_binary = (char *)codevec->absolutize(0);
    size_t code_size = codevec->numberOfWords() * sizeof(uint32_t);
    ObjectCodePB::CodeVec * cv = cb->mutable_codevec();
    cv->set_opcodes( std::string(code_binary, code_size) );

    // Create the export litvec
    ObjectCodePB::LitVec * lv = cb->mutable_litvec();

    // Add objects or references to them to the export litvec
    for (int i = 0; i < litvec->numberOfElements(); i++) {
        pOb ob = litvec->elem(i);
        ObjectCodePB::Object *lvob = lv->add_ob();
        populateObjectByType(ob, lvob);
    }
}

// Write the collected object code to disk
void writeExportCode() {
    if ('\0' == *ExportFile)
        return;

    // Text output of the protobuf for debugging
    if (VerboseFlag) {
        std::string s;
        google::protobuf::TextFormat::PrintToString( objectCode, &s );
        fprintf(stderr, "ObjectCode = \n%s\n", s.c_str());
    }

    // Write the object code to disk.
    std::fstream output(ExportFile, std::ios::out | std::ios::trunc | std::ios::binary);
    if (!objectCode.SerializeToOstream(&output)) {
      warning("Failed to write object code to %s.", ExportFile);
    }
}
