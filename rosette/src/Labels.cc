/* Mode: -*- C++ -*- */
// vim: set ai ts=4 sw=4 expandtab
/* @BC
 *		                Copyright (c) 1993
 *	    by Microelectronics and Computer Technology Corporation (MCC)
 *				All Rights Reserved
 *
 *	Permission to use, copy, modify, and distribute this software and its
 *	documentation for any purpose and without fee is hereby granted,
 *	provided that this notice be retained unaltered, and that the name of
 *	MCC and its shareholders and participants shall not be used in
 *	advertising or publicity pertaining to distribution of the software
 *	without specific written prior permission.
 *
 *	THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 *	WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#include "Labels.h"
#include "Code.h"
#include "Compile.h"
#include "Expr.h"
#include "Table.h"
#include "BuiltinClass.h"

#include <assert.h>
#include <memory.h>


static int min(int m, int n) { return m < n ? m : n; }


BUILTIN_CLASS(FixupVec) {}


FixupVec::FixupVec(int n)
    : BinaryOb(sizeof(FixupVec) + n * sizeof(FixupEntry), CLASS_META(FixupVec),
               CLASS_SBO(FixupVec)),
      count(0) {
    FixupVec::updateCnt();
}


FixupVec::FixupVec(FixupVec* oldvec, int newsize)
    : BinaryOb(sizeof(FixupVec) + newsize * sizeof(FixupEntry), oldvec->meta(),
               oldvec->parent()),
      count(oldvec->count) {
    memcpy(&entry(0), &oldvec->entry(0),
           min(oldvec->count, newsize) * sizeof(FixupEntry));
}


FixupVec* FixupVec::create(int n) {
    void* loc = PALLOC(sizeof(FixupVec) + n * sizeof(FixupEntry));
    return new (loc) FixupVec(n);
}


FixupVec* FixupVec::create(FixupVec* oldvec, int newsize) {
    void* loc =
        PALLOC1(sizeof(FixupVec) + newsize * sizeof(FixupEntry), oldvec);
    return new (loc) FixupVec(oldvec, newsize);
}


void FixupVec::addEntry(int loc, Label label) {
    int n = count++;
    entry(n).loc = loc;
    entry(n).label = label;
}


BUILTIN_CLASS(LabelTable) {
    OB_FIELD("next-label", LabelTable, nextLabel);
    OB_FIELD("label-values", LabelTable, labelValues);
    OB_FIELD("fixup-values", LabelTable, fixupValues);
    OB_FIELD("external-labels", LabelTable, externalLabels);
}


LabelTable::LabelTable(Word16Vec* lv, FixupVec* fv, RblTable* tbl)
    : Ob(sizeof(LabelTable), CLASS_META(LabelTable), CLASS_SBO(LabelTable)),
      nextLabel(FIXNUM(0)),
      labelValues(lv),
      fixupValues(fv),
      externalLabels(tbl) {
    LabelTable::updateCnt();
}


LabelTable* LabelTable::create(int size) {
    Word16Vec* lv = Word16Vec::create(size);
    PROTECT(lv);
    FixupVec* fv = FixupVec::create(size);
    PROTECT(fv);
    RblTable* tbl = RblTable::create();
    void* loc = PALLOC1(sizeof(LabelTable), tbl);
    return new (loc) LabelTable(lv, fv, tbl);
}


static const int EXTERN_LABEL_ENTRY_NODE = 0;
static const int EXTERN_LABEL_ENTRY_LABEL = 1;
static const int EXTERN_LABEL_ENTRIES = 2;


Label LabelTable::newLabel() {
    int n = labelValues->numberOfWords();
    int next = FIXVAL(nextLabel);

    if (next >= n) {
        PROTECT_THIS(LabelTable);
        Word16Vec* newvec = Word16Vec::create(labelValues, 2 * n);
        ASSIGN(SELF, labelValues, newvec);
        FIXNUM_INC(SELF->nextLabel);
    } else {
        FIXNUM_INC(nextLabel);
    }

    return (Label)next;
}


Label LabelTable::newExternalLabel(LabelNode* label_node) {
    PROTECT_THIS(LabelTable);
    PROTECT(label_node);

    Label new_label = SELF->newLabel();
    Tuple* pair = Tuple::create(EXTERN_LABEL_ENTRIES, NIV);
    pair->elem(EXTERN_LABEL_ENTRY_NODE) = label_node;
    pair->elem(EXTERN_LABEL_ENTRY_LABEL) = FIXNUM(new_label);
    SELF->externalLabels->addKey(label_node->expr->label, pair);

    return new_label;
}


void LabelTable::addFixup(int loc, Label label) {
    int n = fixupValues->numberOfEntries();
    if (n >= fixupValues->capacity()) {
        PROTECT_THIS(LabelTable);
        FixupVec* newvec = FixupVec::create(fixupValues, 2 * n);
        ASSIGN(SELF, fixupValues, newvec);
        SELF->fixupValues->addEntry(loc, label);
    } else {
        fixupValues->addEntry(loc, label);
    }
}


void LabelTable::bindLabel(Label label, int loc) {
    labelValues->word(label) = loc;
}


Label LabelTable::getLabel(Ob* label_name) {
    Tuple* pair = (Tuple*)externalLabels->getKey(label_name);

    if (pair == ABSENT) {
        return MissingLabel;
    } else {
        return (Label)FIXVAL(pair->elem(EXTERN_LABEL_ENTRY_LABEL));
    }
}


LabelNode* LabelTable::getLabelNode(Ob* label_name) {
    Tuple* pair = (Tuple*)externalLabels->getKey(label_name);

    if (pair == ABSENT) {
        return (LabelNode*)INVALID;
    } else {
        return (LabelNode*)pair->elem(EXTERN_LABEL_ENTRY_NODE);
    }
}


int LabelTable::resolveLabel(Label label) {
    int val = (int)labelValues->word((int)label);
    assert(val != 0);
    return val;
}


void LabelTable::resolveLabels(CodeBuf* cb) {
    int n = fixupValues->numberOfEntries();
    for (int i = 0; i < n; i++) {
        FixupEntry temp = fixupValues->entry(i);
        cb->patchAddress(temp.loc, resolveLabel(temp.label));
    }
}
