#include "Ob.h"
#include "Code.h"
#include "Prim.h"
#include "Number.h"

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

// Handlers to populate specific objects
void defaultObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
}

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

void rblstringObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::RBLstring *str = lvob->mutable_rblstring();
    str->set_value(BASE(ob)->asPathname());
}

// The handler table that defines which objects are supported, and how they are handled.
typedef std::string ExportObjectKey;
typedef void (*ExportObjectHandler)(ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype);
typedef ObjectCodePB::ObType ExportObjectType;

std::map<ExportObjectKey, std::pair<ExportObjectHandler, ExportObjectType> >  handlers = {
    {"Actor",               {defaultObjectHandler,   ObjectCodePB::OT_Actor} },
    {"BlockExpr",           {defaultObjectHandler,   ObjectCodePB::OT_BlockExpr} },
    {"Char",                {charObjectHandler,      ObjectCodePB::OT_Char} },
    {"Code",                {codeObjectHandler,      ObjectCodePB::OT_Code} },
    {"ExpandedLocation",    {defaultObjectHandler,   ObjectCodePB::OT_ExpandedLocation} },
    {"Fixnum",              {fixnumObjectHandler,    ObjectCodePB::OT_Fixnum} },
    {"Float",               {floatObjectHandler,     ObjectCodePB::OT_Float} },
    {"FreeExpr",            {defaultObjectHandler,   ObjectCodePB::OT_FreeExpr} },
    {"GotoExpr",            {defaultObjectHandler,   ObjectCodePB::OT_GotoExpr} },
    {"IfExpr",              {defaultObjectHandler,   ObjectCodePB::OT_IfExpr} },
    {"LabelExpr",           {defaultObjectHandler,   ObjectCodePB::OT_LabelExpr} },
    {"LetExpr",             {defaultObjectHandler,   ObjectCodePB::OT_LetExpr} },
    {"LetrecExpr",          {defaultObjectHandler,   ObjectCodePB::OT_LetrecExpr} },
    {"MethodExpr",          {defaultObjectHandler,   ObjectCodePB::OT_MethodExpr} },
    {"Proc",                {defaultObjectHandler,   ObjectCodePB::OT_Proc} },
    {"ProcExpr",            {defaultObjectHandler,   ObjectCodePB::OT_ProcExpr} },
    {"QuoteExpr",           {defaultObjectHandler,   ObjectCodePB::OT_QuoteExpr} },
    {"RBLstring",           {rblstringObjectHandler, ObjectCodePB::OT_RBLstring} },
    {"ReflectiveMethodExpr",{defaultObjectHandler,   ObjectCodePB::OT_ReflectiveMethodExpr} },
    {"RequestExpr",         {defaultObjectHandler,   ObjectCodePB::OT_RequestExpr} },
    {"SendExpr",            {defaultObjectHandler,   ObjectCodePB::OT_SendExpr} },
    {"SeqExpr",             {defaultObjectHandler,   ObjectCodePB::OT_SeqExpr} },
    {"SetExpr",             {defaultObjectHandler,   ObjectCodePB::OT_SetExpr} },
    {"StdMthd",             {defaultObjectHandler,   ObjectCodePB::OT_StdMthd} },
    {"Symbol",              {symbolObjectHandler,    ObjectCodePB::OT_Symbol} },
    {"TblObject",           {defaultObjectHandler,   ObjectCodePB::OT_TblObject} },
    {"Template",            {defaultObjectHandler,   ObjectCodePB::OT_Template} },
    {"Tuple",               {defaultObjectHandler,   ObjectCodePB::OT_Tuple} },
    {"TupleExpr",           {defaultObjectHandler,   ObjectCodePB::OT_TupleExpr} },
};

uint64_t createObjectId(Code * code) {
    // Rosette internally, uses the object pointer but for export, this results in
    // duplicate ids due to objects being garbage collected, and then the memory
    // being reused.

    // Here we will use the newly invented object ID that is generated when the
    // object is created by Rosette

    return BASE(code)->id;
}



// This is the protobuf that contains exported object code.
ObjectCodePB::ObjectCode objectCode;


// The collection routine that processes individual Code objects as they are compiled.
void collectExportCode(Code *code) {
    if (VerboseFlag) fprintf(stderr, "\n%s\n", __PRETTY_FUNCTION__);

    uint64_t codeId = createObjectId( code );

    if (VerboseFlag) {
        fprintf(stderr, "CodeId=%llu\n", codeId);
        code->dumpOn(stderr);
    }

    if ('\0' == *ExportFile)
        return;

    // Build the code block protobuf to export
    ObjectCodePB::CodeBlock * cb = objectCode.add_codeblock();

    // Save the code id
    cb->set_id( codeId );

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
        std::string type = BASE(litvec->elem(i))->typestring();
        ObjectCodePB::Object *lvob = lv->add_ob();
        lvob->set_id(BASE(ob)->id);

        // Find the handler for this type
        auto oh = handlers.find(type);
        if (oh != handlers.end()) {
            ExportObjectHandler handler = oh->second.first;
            ExportObjectType obType = oh->second.second;

            lvob->set_type(obType);
            handler(lvob, ob, obType);
        } else {
            warning("Exporting object type %s not yet implemented!", type.c_str());

            lvob->set_type(ObjectCodePB::OT_unknown);
            defaultObjectHandler(lvob, ob, ObjectCodePB::OT_unknown);
        }
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
