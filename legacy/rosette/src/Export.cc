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
#include "Meta.h"
#include "Operation.h"
#include "MI.h"
#include "Cstruct.h"

#include "CommandLine.h"
#include "Export.h"

#include <google/protobuf/text_format.h>
#include <Ob.pb.h>

#include <fstream>
#include <string>
#include <map>
#include <deque>

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

// This is the protobuf that contains exported object code.
ObjectCodePB::ObjectCode objectCode;


// Handlers to populate specific objects
void defaultObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
}


// This keeps track of which objects have been exported so we don't export them multiple times.
std::set< std::pair<IdType, pOb> > exportedIds;

// There are cases where the Objects reference themselves, either directly or indirectly via referenced
// Objects. This can cause an endless loop during export, resulting in a crash.
// We use a stack of pOb's during the recursive export of objects to catch that case here and set "looped"
// to true, thus avoiding going down the rabbit hole.

std::deque<pOb> populateStack;  // For looped reference detection.
                                //  Using std::deque instead of std::stack because std::stack has no iterators

// This function looks up an object in the handlers table based on its type string
// and then calls the appropriate handler to populate it.

void populateObjectByType(pOb ob, ObjectCodePB::Object *lvOb) {

    // Make sure this isn't a looped reference
    for (const pOb stackOb: populateStack) {
        if (stackOb == ob)
        {
            lvOb->set_object_id(BASE(ob)->objectId);
            lvOb->set_reference(true);
            lvOb->set_looped(true);
            break;
        }
    }
    // Note that we are working on this object
    populateStack.push_front(ob);

    std::string type = BASE(ob)->typestring();
    auto oh = handlers.find(type);
    if (oh != handlers.end()) {
        ExportObjectHandler handler = oh->second.first;
        ExportObjectType obType = oh->second.second;
        lvOb->set_type(obType);

        if (lvOb->looped()) {
            // Done with this object
            populateStack.pop_front();
            return;
        }
        if (TAG(ob) == OTptr) {
            // It's a real Rosette object (not RblAtom derived)
            lvOb->set_object_id(BASE(ob)->objectId);
            lvOb->set_reference(true);

            // See if already exported
            auto id = exportedIds.find(std::make_pair(BASE(ob)->objectId, ob));
            if (id == exportedIds.end()) {
                // Object not yet exported.  Add it to the objects list and populate it.
                ObjectCodePB::Object * storeOb = objectCode.add_objects();
                storeOb->set_type(obType);
                storeOb->set_object_id(BASE(ob)->objectId);
                storeOb->set_reference(false);

                // Call the handler to populate this particular object type
                handler(storeOb, ob, obType);

                // Remember that we handled this object
                exportedIds.insert(std::make_pair(BASE(ob)->objectId, ob));

                // Handle the meta and parent object pointers included in the Base class from
                // which all object types are derived.
                ObjectCodePB::Object *meta = storeOb->mutable_meta();
                populateObjectByType( BASE(ob)->meta(), meta );

                ObjectCodePB::Object *parent = storeOb->mutable_parent();
                populateObjectByType( BASE(ob)->parent(), parent );
            }

        } else {
            // This is an RblAtom object. They have no object id, only a value,
            //  and are always exported by value.
            handler(lvOb, ob, obType);
        }
    } else {
        // Oops.  We don't know how to handle this object type...yet.
        warning("Exporting object type %s not yet implemented!", type.c_str());
        lvOb->set_type(ObjectCodePB::OT_UNKNOWN);
        defaultObjectHandler(lvOb, ob, ObjectCodePB::OT_UNKNOWN);
    }

    // Done with this object
    populateStack.pop_front();
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
    sym->set_name(BASE(ob)->asCstring());
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
        pOb el = tup->nth(i);
        ObjectCodePB::Object *elOb = pbTup->add_elements();
        populateObjectByType(el, elOb);
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
    StdExtension * se = (StdExtension *)ob;

    // A StdExtension is similar to a tuple which is a list of objects.
    //  Retrieve and call the handler for each of them.
    for (int i = 0; i < se->numberOfSlots(); i++) {
        pOb el = se->slot(i);
        ObjectCodePB::Object *elOb = sextob->add_elements();
        populateObjectByType(el, elOb);
    }
}

void populateLocation(Location loc, ObjectCodePB::Location * locob) {
    locob->set_value((uint32_t)loc.locfields);

    char buf[256];
    printRep(loc, buf);
    locob->set_text( buf );

    ObjectCodePB::Location_PBLocationType type = (ObjectCodePB::Location_PBLocationType)GET_GENERIC_TYPE(loc);
    locob->set_type( type );
    switch(type) {
    case LT_CtxtRegister: {
        ObjectCodePB::Location::LocCtxt * pbloc = locob->mutable_ctxt();
        pbloc->set_reg( (ObjectCodePB::Location_LocCtxt_PBLocationCtxt)GET_CTXTREG_INDEX(loc) );
        break;
    }

    case LT_ArgRegister: {
        ObjectCodePB::Location::LocArg * pbloc = locob->mutable_arg();
        pbloc->set_arg( GET_ARGREG_INDEX(loc) );
        break;
    }

    case LT_LexVariable: {
        ObjectCodePB::Location::LocLexVar * pbloc = locob->mutable_lexvar();
        pbloc->set_indirect( GET_LEXVAR_IND(loc) );
        pbloc->set_level( GET_LEXVAR_LEVEL(loc) );
        pbloc->set_offset( GET_LEXVAR_OFFSET(loc) );
        break;
    }

    case LT_AddrVariable: {
        ObjectCodePB::Location::LocAddrVar * pbloc = locob->mutable_addrvar();
        pbloc->set_indirect( GET_ADDRVAR_IND(loc) );
        pbloc->set_level( GET_ADDRVAR_LEVEL(loc) );
        pbloc->set_offset( GET_ADDRVAR_OFFSET(loc) );
        break;
    }

    case LT_GlobalVariable: {
        ObjectCodePB::Location::LocGlobalVar * pbloc = locob->mutable_globalvar();
        pbloc->set_offset( GET_GLOBALVAR_OFFSET(loc) );
        break;
    }

    case LT_BitField: {
        ObjectCodePB::Location::LocBitField * pbloc = locob->mutable_bitfield();
        pbloc->set_indirect( GET_BITFIELD_IND(loc) );
        pbloc->set_signed_( GET_BITFIELD_SIGN(loc) );
        pbloc->set_level( GET_BITFIELD_LEVEL(loc) );
        pbloc->set_span( GET_BITFIELD_SPAN(loc) );
        break;
    }

    case LT_BitField00: {
        ObjectCodePB::Location::LocBitField00 * pbloc = locob->mutable_bitfield00();
        pbloc->set_signed_( GET_BITFIELD00_SIGN(loc) );
        pbloc->set_offset( GET_BITFIELD00_OFFSET(loc) );
        pbloc->set_span( GET_BITFIELD00_SPAN(loc) );
        break;
    }

    case LT_Limbo: {
        ObjectCodePB::Location::LocLimbo * pbloc = locob->mutable_limbo();
        break;
    }

    default: {
        warning("Unknown Location Type during export.");
    }
    }
}

void expandedLocationObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ExpandedLocation *exlocob = lvob->mutable_expanded_location();
    ObjectCodePB::Location *locob = exlocob->mutable_loc();

    ExpandedLocation *el = (ExpandedLocation *)ob;
    Location loc;
    loc.atom = el;
    populateLocation(loc, locob);
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
    populateObjectByType(te->rest, rest);

    // A tupleExpr has a list of objects.  Retrieve and call the handler for each of them.
    for (int i = 0; i < te->numberOfElements(); i++) {
        pOb el = te->nth(i);
        ObjectCodePB::Object *elOb = teob->add_elements();
        populateObjectByType(el, elOb);
    }

}

void letRecExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::LetrecExpr *lreob = lvob->mutable_let_rec_expr();
    LetrecExpr * lre = (LetrecExpr *)ob;

    ObjectCodePB::Object *bindings = lreob->mutable_bindings();
    populateObjectByType(lre->bindings, bindings);

    ObjectCodePB::Object *body = lreob->mutable_body();
    populateObjectByType(lre->body, body);
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
    ProcExpr * pe = (ProcExpr *)ob;

    ObjectCodePB::Object *identity = peob->mutable_identity();
    populateObjectByType(pe->identity, identity);

    ObjectCodePB::Object *formals = peob->mutable_formals();
    populateObjectByType(pe->formals, formals);

    ObjectCodePB::Object *body = peob->mutable_body();
    populateObjectByType(pe->body, body);

}

void quoteExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::QuoteExpr *qeob = lvob->mutable_quote_expr();
    QuoteExpr * qe = (QuoteExpr *)ob;

    ObjectCodePB::Object *expr = qeob->mutable_expr();
    populateObjectByType(qe->expr, expr);
}

void reflectiveMethodExprObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ReflectiveMethodExpr *rmeob = lvob->mutable_reflective_method_expr();
    ReflectiveMethodExpr * rme = (ReflectiveMethodExpr *)ob;

    ObjectCodePB::Object *identity = rmeob->mutable_identity();
    populateObjectByType(rme->identity, identity);

    ObjectCodePB::Object *formals = rmeob->mutable_formals();
    populateObjectByType(rme->formals, formals);

    ObjectCodePB::Object *body = rmeob->mutable_body();
    populateObjectByType(rme->body, body);
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
    SendExpr * se = (SendExpr *)ob;

    ObjectCodePB::Object *target = seob->mutable_target();
    populateObjectByType(se->target, target);

    ObjectCodePB::Object *msg = seob->mutable_msg();
    populateObjectByType(se->msg, msg);
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

void templateObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Template *tob = lvob->mutable_template_();
    Template * t = (Template *)ob;

    ObjectCodePB::Object *keytuple = tob->mutable_keytuple();
    populateObjectByType(t->keytuple, keytuple);

    ObjectCodePB::Object *pat = tob->mutable_pat();
    populateObjectByType(t->pat, pat);

    ObjectCodePB::Object *keymeta = tob->mutable_keymeta();
    populateObjectByType(t->keymeta, keymeta);
}

void complexPatternObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ComplexPattern *cpob = lvob->mutable_complex_pattern();
    ComplexPattern * cp = (ComplexPattern *)ob;

    ObjectCodePB::Object *expr = cpob->mutable_expr();
    populateObjectByType(cp->expr, expr);

    ObjectCodePB::Object *patvec = cpob->mutable_patvec();
    populateObjectByType(cp->patvec, patvec);

    ObjectCodePB::Object *offsetvec = cpob->mutable_offsetvec();
    populateObjectByType(cp->offsetvec, offsetvec);
}

void stdMetaObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::StdMeta *smob = lvob->mutable_std_meta();
    StdMeta * sm = (StdMeta *)ob;

    ObjectCodePB::Object *extension = smob->mutable_extension();
    populateObjectByType(sm->extension, extension);
}

void idVecPatternObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::IdVecPattern *ivpob = lvob->mutable_id_vec_pattern();
    IdVecPattern * ivp = (IdVecPattern *)ob;

    ObjectCodePB::Object *expr = ivpob->mutable_expr();
    populateObjectByType(ivp->expr, expr);
}

void idAmprRestPatternObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::IdAmprRestPattern *iarpob = lvob->mutable_id_ampr_rest_pattern();
    IdAmperRestPattern * iarp = (IdAmperRestPattern *)ob;

    ObjectCodePB::Object *expr = iarpob->mutable_expr();
    populateObjectByType(iarp->expr, expr);
}

void idPatternObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::IdPattern *ipob = lvob->mutable_id_pattern();
    IdPattern * ip = (IdPattern *)ob;

    ObjectCodePB::Object *symbol = ipob->mutable_symbol();
    populateObjectByType(ip->symbol, symbol);
}

void primObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Prim *pob = lvob->mutable_prim();
    Prim * p = (Prim *)ob;

    ObjectCodePB::Object *id = pob->mutable_id();
    populateObjectByType(p->id, id);

    // ObjectCodePB::Object *fn = pob->mutable_fn();
    // populateObjectByType(p->fn, fn);

    pob->set_minargs(p->minargs);
    pob->set_maxargs(p->maxargs);
    pob->set_primnum(p->primnum);
}

void sysvalObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::Sysval *sv = lvob->mutable_sysval();

    sv->set_value(BASE(ob)->asCstring());
}

void constPatternObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ConstPattern *cpob = lvob->mutable_const_pattern();
    ConstPattern * cp = (ConstPattern *)ob;

    ObjectCodePB::Object *val = cpob->mutable_val();
    populateObjectByType(cp->val, val);
}

void rblTableObjectHandler( ObjectCodePB::Object * lvob, pOb ob, ObjectCodePB::ObType pbtype) {
    ObjectCodePB::RblTable *rtob = lvob->mutable_rbl_table();
    RblTable * rt = (RblTable *)ob;

    ObjectCodePB::Object *tbl = rtob->mutable_tbl();
    populateObjectByType(rt->tbl, tbl);
}

void indexedMetaObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype) {
    ObjectCodePB::IndexedMeta *imob = pbob->mutable_indexed_meta();
    IndexedMeta * im = (IndexedMeta *)ob;

    ObjectCodePB::Object *extension = imob->mutable_extension();
    populateObjectByType(im->extension, extension);
}

void stdOprnObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::StdOprn *soob = pbob->mutable_std_oprn();
    StdOprn * so = (StdOprn *)ob;

    ObjectCodePB::Object *extension = soob->mutable_extension();
    populateObjectByType(so->extension, extension);
}

void topEnvObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::TopEnv *teob = pbob->mutable_top_env();
    RBLtopenv * teo = (RBLtopenv *)ob;

}

void miActorObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::MIActor *maob = pbob->mutable_mi_actor();
    MIActor * mao = (MIActor *)ob;

    ObjectCodePB::Object *extension = maob->mutable_extension();
    populateObjectByType(mao->extension, extension);
}

void atomicDescriptorObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::AtomicDescriptor *adob = pbob->mutable_atomic_descriptor();
    AtomicDescriptor * ado = (AtomicDescriptor *)ob;

    adob->set__offset(ado->_offset);
    adob->set__align_to(ado->_align_to);
    adob->set__size(ado->_size);

    ObjectCodePB::Object *mnemonic = adob->mutable_mnemonic();
    populateObjectByType(ado->mnemonic, mnemonic);

    ObjectCodePB::Object *imported = adob->mutable_imported();
    populateObjectByType(ado->imported, imported);

    ObjectCodePB::Object *freestructongc = adob->mutable_freestructongc();
    populateObjectByType(ado->freeStructOnGC, freestructongc);

    ObjectCodePB::Object *_signed = adob->mutable__signed();
    populateObjectByType(ado->_signed, _signed);
}

void reflectiveMthdObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ReflectiveMthd *rmob = pbob->mutable_reflective_mthd();
    ReflectiveMthd * rmo = (ReflectiveMthd *)ob;

    ObjectCodePB::Object *code = rmob->mutable_code();
    populateObjectByType(rmo->code, code);

    ObjectCodePB::Object *id = rmob->mutable_id();
    populateObjectByType(rmo->id, id);

    ObjectCodePB::Object *source = rmob->mutable_source();
    populateObjectByType(rmo->source, source);
}

void productTypeObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::ProductType *ptob = pbob->mutable_product_type();
    ProductType * pto = (ProductType *)ob;

    // A ProductType is similar to a tuple which is a list of objects.
    //  Retrieve and call the handler for each of them.
    for (int i = 0; i < pto->numberOfElements(); i++) {
        pOb el = pto->elem(i);
        ObjectCodePB::Object *elOb = ptob->add_elements();
        populateObjectByType(el, elOb);
    }
}

void sumTypeObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::SumType *stob = pbob->mutable_sum_type();
    SumType * sto = (SumType *)ob;

    // A SumType is similar to a tuple which is a list of objects.
    //  Retrieve and call the handler for each of them.
    for (int i = 0; i < sto->types()->numberOfElements(); i++) {
        pOb el = sto->elem(i);
        ObjectCodePB::Object *elOb = stob->add_elements();
        populateObjectByType(el, elOb);
    }
}

void multiMethodObjectHandler( ObjectCodePB::Object * pbob, pOb ob, ObjectCodePB::ObType pbtype ) {
    ObjectCodePB::MultiMethod *mmob = pbob->mutable_multi_method();
    MultiMethod * mmo = (MultiMethod *)ob;

    // A MultiMethod is similar to a tuple which is a list of objects.
    //  Retrieve and call the handler for each of them.
    for (int i = 0; i < mmo->numberOfElements(); i++) {
        pOb el = mmo->elem(i);
        ObjectCodePB::Object *elOb = mmob->add_elements();
        populateObjectByType(el, elOb);
    }
}

// The handler table that defines which objects are supported, and how they are handled.
// The string type names here MUST match those implemented in Rosette. And yes, there
// is some inconsistency (e.g. RBL vs Rbl).

std::map<ExportObjectKey, std::pair<ExportObjectHandler, ExportObjectType> >  handlers = {
    {"Actor",               {actorObjectHandler,     ObjectCodePB::OT_ACTOR} },
    {"AtomicDescriptor",    {atomicDescriptorObjectHandler, ObjectCodePB::OT_ATOMIC_DESCRIPTOR} },
    {"BlockExpr",           {blockExprObjectHandler, ObjectCodePB::OT_BLOCK_EXPR} },
    {"Char",                {charObjectHandler,      ObjectCodePB::OT_CHAR} },
    {"Code",                {codeObjectHandler,      ObjectCodePB::OT_CODE} },
    {"ComplexPattern",      {complexPatternObjectHandler, ObjectCodePB::OT_COMPLEX_PATTERN} },
    {"ConstPattern",        {constPatternObjectHandler, ObjectCodePB::OT_CONST_PATTERN} },
    {"ExpandedLocation",    {expandedLocationObjectHandler, ObjectCodePB::OT_EXPANDED_LOCATION} },
    {"Fixnum",              {fixnumObjectHandler,    ObjectCodePB::OT_FIXNUM} },
    {"Float",               {floatObjectHandler,     ObjectCodePB::OT_FLOAT} },
    {"FreeExpr",            {freeExprObjectHandler,  ObjectCodePB::OT_FREE_EXPR} },
    {"GotoExpr",            {gotoExprObjectHandler,  ObjectCodePB::OT_GOTO_EXPR} },
    {"IdAmperRestPattern",  {idAmprRestPatternObjectHandler, ObjectCodePB::OT_ID_AMPR_REST_PATTERN} },
    {"IdPattern",           {idPatternObjectHandler, ObjectCodePB::OT_ID_PATTERN} },
    {"IdVecPattern",        {idVecPatternObjectHandler, ObjectCodePB::OT_ID_VEC_PATTERN} },
    {"IfExpr",              {ifExprObjectHandler,    ObjectCodePB::OT_IF_EXPR} },
    {"IndexedMeta",         {indexedMetaObjectHandler, ObjectCodePB::OT_INDEXED_META} },
    {"LabelExpr",           {labelExprObjectHandler, ObjectCodePB::OT_LABEL_EXPR} },
    {"LetExpr",             {letExprObjectHandler,   ObjectCodePB::OT_LET_EXPR} },
    {"LetrecExpr",          {letRecExprObjectHandler, ObjectCodePB::OT_LET_REC_EXPR} },
    {"MethodExpr",          {methodExprObjectHandler, ObjectCodePB::OT_METHOD_EXPR} },
    {"MIActor",             {miActorObjectHandler,   ObjectCodePB::OT_MI_ACTOR} },
    {"MultiMethod",         {multiMethodObjectHandler, ObjectCodePB::OT_MULTI_METHOD} },
    {"Niv",                 {nivObjectHandler,       ObjectCodePB::OT_NIV} },
    {"NullExpr",            {nullExprObjectHandler,  ObjectCodePB::OT_NULL_EXPR} },
    {"Prim",                {primObjectHandler,      ObjectCodePB::OT_PRIM} },
    {"Proc",                {procObjectHandler,      ObjectCodePB::OT_PROC} },
    {"ProcExpr",            {procExprObjectHandler,  ObjectCodePB::OT_PROC_EXPR} },
    {"ProductType",         {productTypeObjectHandler, ObjectCodePB::OT_PRODUCT_TYPE} },
    {"QuoteExpr",           {quoteExprObjectHandler, ObjectCodePB::OT_QUOTE_EXPR} },
    {"RblBool",             {rblboolObjectHandler,   ObjectCodePB::OT_RBL_BOOL} },
    {"RBLstring",           {rblstringObjectHandler, ObjectCodePB::OT_RBL_STRING} },
    {"RblTable",            {rblTableObjectHandler,  ObjectCodePB::OT_RBL_TABLE} },
    {"ReflectiveMthd",      {reflectiveMthdObjectHandler, ObjectCodePB::OT_REFLECTIVE_MTHD} },
    {"ReflectiveMethodExpr",{reflectiveMethodExprObjectHandler, ObjectCodePB::OT_REFLECTIVE_METHOD_EXPR} },
    {"RequestExpr",         {requestExprObjectHandler, ObjectCodePB::OT_REQUEST_EXPR} },
    {"SendExpr",            {sendExprObjectHandler,  ObjectCodePB::OT_SEND_EXPR} },
    {"SeqExpr",             {seqExprObjectHandler,   ObjectCodePB::OT_SEQ_EXPR} },
    {"SetExpr",             {setExprObjectHandler,   ObjectCodePB::OT_SET_EXPR} },
    {"StdExtension",        {stdExtensionObjectHandler, ObjectCodePB::OT_STD_EXTENSION} },
    {"StdMeta",             {stdMetaObjectHandler,   ObjectCodePB::OT_STD_META} },
    {"StdMthd",             {stdMthdObjectHandler,   ObjectCodePB::OT_STD_METHOD} },
    {"StdOprn",             {stdOprnObjectHandler,   ObjectCodePB::OT_STD_OPRN} },
    {"SumType",             {sumTypeObjectHandler,   ObjectCodePB::OT_SUM_TYPE} },
    {"Symbol",              {symbolObjectHandler,    ObjectCodePB::OT_SYMBOL} },
    {"Sysval",              {sysvalObjectHandler,    ObjectCodePB::OT_SYSVAL} },
    {"TblObject",           {tblObjectHandler,       ObjectCodePB::OT_TBL_OBJECT} },
    {"Template",            {templateObjectHandler,  ObjectCodePB::OT_TEMPLATE} },
    {"TopEnv",              {topEnvObjectHandler,    ObjectCodePB::OT_TOP_ENV} },
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

// The collection routine that processes individual Code objects as they are compiled.
void collectExportCode(Code *code) {
    if ('\0' == *ExportFile)
        return;

    if (VerboseFlag) fprintf(stderr, "\n%s\n", __PRETTY_FUNCTION__);

    IdType codeId = createObjectId( code );

    if (VerboseFlag) {
        fprintf(stderr, "CodeId=%llu\n", codeId);
        code->dumpOn(stderr);
    }

    // Build the code block protobuf to export
    ObjectCodePB::CodeBlock * cb = objectCode.add_code_block();

    // Save the code id
   cb->set_object_id( codeId );

    // Get access to the codevec and litvec to be exported.
    CodeVec * codevec = code->codevec;
    Tuple * litvec = code->litvec;

    // Export the opCodes from the codevec
    size_t codesize = codevec->numberOfWords();
    ObjectCodePB::CodeVec * cv = cb->mutable_codevec();
    for(size_t i=0; i<codesize; i++) {
        cv->add_opcodes( codevec->instr(i) );
    }

    // Create the export litvec
    ObjectCodePB::LitVec * lv = cb->mutable_litvec();

    // Add objects to the object list and references to them to the export litvec,
    //  or in the case of RblAtom objects, add the value to the litvec.
    for (int i = 0; i < litvec->numberOfElements(); i++) {
        pOb ob = litvec->nth(i);
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

        fprintf(stderr, "*** Exported ObjectCode = \n%s\n", s.c_str());
        fprintf(stderr, "object count = %d\n", objectCode.objects_size());
        fprintf(stderr, "code count = %d\n", objectCode.code_block_size());
    }

    // Write the object code to disk.
    std::fstream output(ExportFile, std::ios::out | std::ios::trunc | std::ios::binary);
    if (!objectCode.SerializeToOstream(&output)) {
      warning("Failed to write object code to %s.", ExportFile);
    }
}

