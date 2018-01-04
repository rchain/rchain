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

#if !defined(_RBL_Labels_h)
#define _RBL_Labels_h

#include "rosette.h"
#include "BinaryOb.h"
#include "Ob.h"


typedef int Label;
static const Label NoneRemaining = -1;
static const Label NoParticularLabel = -2;
static const Label MissingLabel = -3;


struct FixupEntry {
    int loc;
    Label label;
};


class FixupVec : public BinaryOb {
    STD_DECLS(FixupVec);

   protected:
    int count;

    /*
     * If more member variables are added after count, be sure to modify
     * the code for entry(), since it needs to know where the fixed part
     * of the object ends.
     */

    FixupVec(int);
    FixupVec(FixupVec*, int);

    static FixupVec* create(int);
    static FixupVec* create(FixupVec*, int);

    FixupEntry& entry(int n) {
        FixupEntry* p = (FixupEntry*)(((char*)&count) + sizeof(count));
        return p[n];
    }

    friend class LabelTable;

   public:
    int numberOfEntries() { return (count); }
    void addEntry(int, Label);
    int capacity() {
        return ((SIZE(this) - sizeof(FixupVec)) / sizeof(FixupEntry));
    }
};


static const int DefaultLabelTableSize = 32;


class LabelNode;


class LabelTable : public Ob {
    STD_DECLS(LabelTable);

   protected:
    LabelTable(Word16Vec*, FixupVec*, RblTable*);

   public:
    Ob* nextLabel;
    Word16Vec* labelValues;
    FixupVec* fixupValues;
    RblTable* externalLabels;

    static LabelTable* create(int = DefaultLabelTableSize);

    Label newLabel();
    Label newExternalLabel(LabelNode*);
    void addFixup(int, Label);
    void bindLabel(Label, int);
    Label getLabel(Ob*);
    LabelNode* getLabelNode(Ob*);
    int resolveLabel(Label);
    void resolveLabels(CodeBuf*);
};

#endif
