#include "Ob.h"
#include "Code.h"
#include "Prim.h"
#include "Number.h"

#include "CommandLine.h"

#include <google/protobuf/text_format.h>
#include <Ob.pb.h>
#include <fstream>


#include <string>

ObjectCodePB::ObjectCode objectCode;

void collectExportCode(Code *code) {
    if ('\0' == *ExportFile)
        return;

    if (VerboseFlag) fprintf(stderr, "\n%s\n", __PRETTY_FUNCTION__);

    // Build the code block protobuf to export
    ObjectCodePB::CodeBlocks * cbs = objectCode.mutable_codeblocks();
    ObjectCodePB::CodeBlock * cb = cbs->add_codeblock();

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
        ObjectCodePB::LitVecItem *item = lv->add_item();

        if (type == "Actor") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Actor *act = newOb->mutable_actor();
            newOb->set_type(ObjectCodePB::OT_Actor);
        } else if (type == "BlockExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::BlockExpr *be = newOb->mutable_blockexpr();
            newOb->set_type(ObjectCodePB::OT_BlockExpr);
        } else if (type == "Char") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Char *ch = newOb->mutable_char_();
            newOb->set_type(ObjectCodePB::OT_Char);
            ch->set_value(CHARVAL(ob));
        } else if (type == "Code") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Code *cod = newOb->mutable_code();
            newOb->set_type(ObjectCodePB::OT_Code);
        } else if (type == "ExpandedLocation") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::ExpandedLocation *el = newOb->mutable_expandedlocation();
            newOb->set_type(ObjectCodePB::OT_ExpandedLocation);
        } else if (type == "Fixnum") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Fixnum *fn = newOb->mutable_fixnum();
            newOb->set_type(ObjectCodePB::OT_Fixnum);
            fn->set_value(FIXVAL(ob));
        } else if (type == "Float") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Float *fl = newOb->mutable_float_();
            newOb->set_type(ObjectCodePB::OT_Float);
            fl->set_value(((Float*)(ob))->val);
        } else if (type == "FreeExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::FreeExpr *fe = newOb->mutable_freeexpr();
            newOb->set_type(ObjectCodePB::OT_FreeExpr);
        } else if (type == "GotoExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::GotoExpr *ge = newOb->mutable_gotoexpr();
            newOb->set_type(ObjectCodePB::OT_GotoExpr);
        } else if (type == "IfExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::IfExpr *ie = newOb->mutable_ifexpr();
            newOb->set_type(ObjectCodePB::OT_IfExpr);
        } else if (type == "LabelExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::LabelExpr *le = newOb->mutable_labelexpr();
            newOb->set_type(ObjectCodePB::OT_LabelExpr);
        } else if (type == "LetExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::LetExpr *le = newOb->mutable_letexpr();
            newOb->set_type(ObjectCodePB::OT_LetExpr);
        } else if (type == "LetrecExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::LetrecExpr *lre = newOb->mutable_letrecexpr();
            newOb->set_type(ObjectCodePB::OT_LetrecExpr);
        } else if (type == "MethodExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::MethodExpr *me = newOb->mutable_methodexpr();
            newOb->set_type(ObjectCodePB::OT_MethodExpr);
        } else if (type == "Proc") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Proc *proc = newOb->mutable_proc();
            newOb->set_type(ObjectCodePB::OT_Proc);
        } else if (type == "ProcExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::ProcExpr *pe = newOb->mutable_procexpr();
            newOb->set_type(ObjectCodePB::OT_ProcExpr);
        } else if (type == "QuoteExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::QuoteExpr *qe = newOb->mutable_quoteexpr();
            newOb->set_type(ObjectCodePB::OT_QuoteExpr);
        } else if (type == "RBLstring") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::RBLstring *str = newOb->mutable_rblstring();
            newOb->set_type(ObjectCodePB::OT_RBLstring);
            str->set_value(BASE(ob)->asPathname());
        } else if (type == "ReflectiveMethodExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::ReflectiveMethodExpr *rme = newOb->mutable_reflectivemethodexpr();
            newOb->set_type(ObjectCodePB::OT_ReflectiveMethodExpr);
        } else if (type == "RequestExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::RequestExpr *re = newOb->mutable_requestexpr();
            newOb->set_type(ObjectCodePB::OT_RequestExpr);
        } else if (type == "SendExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::SendExpr *se = newOb->mutable_sendexpr();
            newOb->set_type(ObjectCodePB::OT_SendExpr);
        } else if (type == "SeqExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::SeqExpr *se = newOb->mutable_seqexpr();
            newOb->set_type(ObjectCodePB::OT_SendExpr);
        } else if (type == "SetExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::SetExpr *se = newOb->mutable_setexpr();
            newOb->set_type(ObjectCodePB::OT_SetExpr);
        } else if (type == "StdMthd") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::StdMthd *sm = newOb->mutable_stdmthd();
            newOb->set_type(ObjectCodePB::OT_StdMthd);
        } else if (type == "Symbol") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Symbol *sym = newOb->mutable_symbol();
            newOb->set_type(ObjectCodePB::OT_Symbol);
            sym->set_name(BASE(ob)->asPathname());
        } else if (type == "TblObject") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::TblObject *to = newOb->mutable_tblobject();
            newOb->set_type(ObjectCodePB::OT_TblObject);
        } else if (type == "Template") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Template *t = newOb->mutable_template_();
            newOb->set_type(ObjectCodePB::OT_Template);
        } else if (type == "Tuple") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::Tuple *tup = newOb->mutable_tuple();
            newOb->set_type(ObjectCodePB::OT_Tuple);
        } else if (type == "TupleExpr") {
            ObjectCodePB::Object * newOb = item->mutable_ob();
            ObjectCodePB::TupleExpr *te = newOb->mutable_tupleexpr();
            newOb->set_type(ObjectCodePB::OT_TupleExpr);
        } else {
            warning("Exporting object %s not yet implemented!", type.c_str());
        }
    }
}

void writeExportCode() {

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
