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

#include "rosette.h"
#include "RblAtom.h"
#include "Ctxt.h"
#include "Interrupt.h"
#include "Meta.h"
#include "Prim.h"
#include "Table.h"
#include "Tuple.h"
#include "BuiltinClass.h"

#include <memory.h>

BUILTIN_CLASS(StdMeta) {
    OB_FIELD_INDIRECT("map", STDMETA_MAP_SLOT);
    OB_FIELD_INDIRECT("ref-count", STDMETA_REFCOUNT_SLOT);
    OB_FIELD_INDIRECT("extensible", STDMETA_EXTENSIBLE_SLOT);
}


extern pOb emptyMbox;

StdMeta::StdMeta(pExt ext)
    : Actor(sizeof(StdMeta), CLASS_META(StdMeta), CLASS_SBO(StdMeta), emptyMbox,
            ext) {
    StdMeta::updateCnt();
}


pMeta StdMeta::create(pTuple map, pOb ref_count, pOb extensible) {
    PROTECT(ref_count);
    PROTECT(extensible);
    RblTable* tbl = (RblTable*)INVALID;
    PROTECT(tbl);

    /*
     * During the boot phase, we use map == NULL to avoid prematurely
     * allocating RblTables whose parents and metas can't yet be properly
     * initialized.
     */

    if (map != NULL) {
        PROTECT(map);
        tbl = RblTable::create();
        const int n = map->numberOfElements();
        const bool indirect = extensible == RBLTRUE;
        for (int i = 0; i < n; i++) {
            tbl->addKey(map->elem(i), LexVar(0, i, indirect).atom);
        }
    }

    pExt ext = StdExtension::create(BUILTIN_STDMETA_SLOTS);
    ext->slot(STDMETA_MAP_SLOT) = tbl;
    ext->slot(STDMETA_REFCOUNT_SLOT) = ref_count;
    ext->slot(STDMETA_EXTENSIBLE_SLOT) = extensible;

    void* loc = PALLOC1(sizeof(StdMeta), ext);
    return new (loc) StdMeta(ext);
}


pOb StdMeta::cloneTo(pOb new_meta, pOb new_parent) {
    extern pOb NILmeta;

    if (this == NILmeta) {
        return this;
    }

    PROTECT_THIS(StdMeta);
    pMeta ob = (pMeta)SELF->Actor::cloneTo(new_meta, new_parent);
    PROTECT(ob);
    ob->extension->slot(STDMETA_REFCOUNT_SLOT) = FIXNUM(0);
    pOb new_map = ob->extension->slot(STDMETA_MAP_SLOT)->clone();
    ASSIGN(ob->extension, slot(STDMETA_MAP_SLOT), new_map);
    return ob;
}


pTuple StdMeta::keys(pOb) { return map()->dumpKeys(); }


pTuple StdMeta::locs(pOb) { return NIL; }


Location StdMeta::keyLoc(pOb key, pOb) {
    pOb atom = map()->getKey(key);
    if (atom == ABSENT) {
        return LocLimbo;
    } else {
        Location loc;
        loc.atom = atom;
        return loc;
    }
}


pTuple StdMeta::locContour(pOb) { return map()->dumpPairs(); }


pTuple StdMeta::contour(pOb client) {
    PROTECT_THIS(StdMeta);
    PROTECT(client);
    pTuple result = SELF->locContour(client);
    int limit = result->numberOfElements();
    for (int i = 1; i < limit; i += 2) {
        Location descriptor;
        descriptor.atom = result->elem(i);
        result->elem(i) = valWrt(descriptor, client);
    }
    return result;
}


pOb StdMeta::get(pOb client, pOb key, pCtxt) {
    Location loc = keyLoc(key, client);
    pOb container = BASE(client);
    /*
     * This unfolding of the valWrt function pays big dividends during
     * method lookup.
     */

    switch (GET_GENERIC_TYPE(loc)) {
    case LT_LexVariable:
        if (GET_LEXVAR_IND(loc)) {
            container = ((Actor*)client)->extension;

            // TODO: This prevents a crash caused by a malformed expander. See
            // Jira ticket ROS-304.
            //
            //       This is a blatant BANDAID and a HACK, and needs to be fixed
            //       in the compiler
            //       to not generate malformed objects.
            //       The comparison agains 0x100 is because, while the pointer
            //       is "NULL", it really
            //       isn't zero because of the additional "Tag" data that in
            //       this implementation gets
            //       overlayed on pOb pointers.
            //       Here I arbitrarily used a value that is smaller than any
            //       normal pointer, and greater
            //       than tag bit stuff.
            //
            //       See the use of TAG, TagExtract, TagSize, EscTagSize,
            //       WordSize, GET_*Tagged_*, GET_LF
            //       in Ob.h, Ob.cc, Bits.h and elsewhere for additional clues.

            TagExtract te;
            te.ptr = container;
            if (te.locfields < 0x100) {
                warning("Malformed Meta. Actor has NULL extension");
                return ABSENT;
            }
        }

        return container->slot(GET_LEXVAR_OFFSET(loc));
    case LT_Limbo:
        return ABSENT;
    default:
        return valWrt(loc, client);
    }
}


pOb StdMeta::add(pOb client, pOb key, pOb val, pCtxt ctxt) {
    Location descriptor = keyLoc(key, client);

    if (descriptor == LocLimbo) {
        if (clientsAreExtensible()) {
            PROTECT_THIS(StdMeta);
            PROTECT(client);
            PROTECT(key);
            PROTECT(val);

            const int offset = client->addSlot(key, val);
            const int INDIRECT = 1;

            pMeta new_meta = SELF;

            if (SELF->isShared()) {
                new_meta = StdMeta::create(NIL);
                PROTECT(new_meta);
                pOb new_map = SELF->map()->clone();

                ASSIGN(new_meta, parent(), SELF);
                ASSIGN(new_meta->extension, slot(STDMETA_MAP_SLOT), new_map);

                client->meta()->deleteRef();
                new_meta->addRef();

                /*
                 * At this point, we know that client must be an
                 * extensible object with an extension field, so we cast
                 * it appropriately.
                 */
                ASSIGN(client, meta(), new_meta);
                ASSIGN(((Actor*)client)->extension, meta(), new_meta);
            }

            new_meta->map()->addKey(key, LexVar(0, offset, INDIRECT).atom);
        } else {
            return BASE(ctxt->trgt)
                ->runtimeError(ctxt, "can't add slot to non-extensible object");
        }
    } else {
        setValWrt(descriptor, client, val);
    }

    return client;
}


pOb StdMeta::set(pOb client, pOb key, pOb val, pCtxt ctxt) {
    Location descriptor = keyLoc(key, client);
    if (descriptor != LocLimbo) {
        setValWrt(descriptor, client, val);
        return client;
    } else {
        return ctxt->missingBindingError(key);
    }
}


void StdMeta::addRef(void) {
    pOb& ref_count = extension->slot(STDMETA_REFCOUNT_SLOT);
    if (ref_count != MAX_FIXNUM) {
        FIXNUM_INC(ref_count);
    }
}


void StdMeta::deleteRef(void) {
    pOb& ref_count = extension->slot(STDMETA_REFCOUNT_SLOT);
    if (ref_count != MAX_FIXNUM) {
        FIXNUM_DEC(ref_count);
    }
}


pOb StdMeta::lookupOBO(pOb client, pOb key, pCtxt ctxt) {
    if (interruptPending) {
        return ABSENT;
    }

    pOb result = get(client, key, ctxt);

    if (result == ABSENT) {
        return BASE(BASE(client)->parent())->lookup(key, ctxt);
    } else {
        return result;
    }
}


void StdMeta::allocateMap() {
    PROTECT_THIS(StdMeta);
    RblTable* map = RblTable::create();
    ASSIGN(SELF->extension, slot(STDMETA_MAP_SLOT), map);
}


void StdMeta::becomeIndexed(int start_indexed_part) {
    new (this) IndexedMeta(this, start_indexed_part);
}


BUILTIN_CLASS(IndexedMeta) {
    OB_FIELD_INDIRECT("map", STDMETA_MAP_SLOT);
    OB_FIELD_INDIRECT("ref-count", STDMETA_REFCOUNT_SLOT);
    OB_FIELD_INDIRECT("extensible", STDMETA_EXTENSIBLE_SLOT);
    OB_FIELD_INDIRECT("indexed-part", INDEXEDMETA_START_INDEXED_PART_SLOT);
}


IndexedMeta::IndexedMeta(pMeta meta, int indexed_part_start)
    : StdMeta(BuildInPlace) {
    PROTECT(meta);
    pOb old_ext = meta->extension;
    pExt new_ext = (pExt)old_ext->rcons(FIXNUM(indexed_part_start));
    new_ext->meta() = CLASS_META(IndexedMeta);
    new_ext->parent() = CLASS_SBO(IndexedMeta);
    ASSIGN(meta, meta(), CLASS_META(IndexedMeta));
    ASSIGN(meta, parent(), CLASS_SBO(IndexedMeta));
    ASSIGN(meta, extension, new_ext);
}


pTuple IndexedMeta::keys(pOb ob) {
    PROTECT_THIS(IndexedMeta);
    PROTECT(ob);

    int nslots = BASE(ob)->numberOfSlots();
    int start_slot =
        FIXVAL(extension->slot(INDEXEDMETA_START_INDEXED_PART_SLOT));
    int N = nslots - start_slot;

    pTuple base_keys = StdMeta::keys(ob);
    PROTECT(base_keys);

    int base_offset = base_keys->numberOfElements();
    pTuple result = Tuple::create(base_offset + N, NIV);
    memcpy(&result->elem(0), &base_keys->elem(0), base_offset * sizeof(pOb));
    for (int i = 0; i < N; i++) {
        result->elem(base_offset + i) = FIXNUM(i);
    }

    return result;
}


Location IndexedMeta::keyLoc(pOb key, pOb client) {
    if (IS_FIXNUM(key)) {
        if ((client == ABSENT) ||
            ((0 <= FIXVAL(key)) && (FIXVAL(key) < client->numberOfSlots()))) {
            return LexVar(
                0,
                FIXVAL(extension->slot(INDEXEDMETA_START_INDEXED_PART_SLOT)) +
                    FIXVAL(key),
                clientsAreExtensible());
        } else {
            return LocLimbo;
        }
    } else {
        return StdMeta::keyLoc(key, client);
    }
}


pTuple IndexedMeta::locContour(pOb ob) {
    pOb pob = BASE(ob);
    PROTECT_THIS(IndexedMeta);
    PROTECT(pob);

    const int nslots = pob->numberOfSlots();
    const int start_slot =
        FIXVAL(extension->slot(INDEXEDMETA_START_INDEXED_PART_SLOT));
    const int N = nslots - start_slot;

    pTuple base_contour = StdMeta::locContour(pob);
    PROTECT(base_contour);

    const int base_offset = base_contour->numberOfElements();
    const bool indirect = SELF->clientsAreExtensible();
    pTuple result = Tuple::create(base_offset + 2 * N, NIV);
    memcpy(&result->elem(0), &base_contour->elem(0), base_offset * sizeof(pOb));
    for (int i = 0; i < N; i++) {
        result->elem(base_offset + 2 * i) = FIXNUM(i);
        result->elem(base_offset + 2 * i + 1) =
            LexVar(0, start_slot + i, indirect).atom;
    }

    return result;
}


#define ENFORCE_META(prim, self, client)                       \
    {                                                          \
        if (self != BASE(client)->meta())                      \
            return PRIM_ERROR("inconsistent meta and client"); \
    }


DEF("keys", metaKeys, 2, 2) {
    ENFORCE_META(metaKeys, ARG(0), ARG(1));
    return BASE(ARG(0))->keys(ARG(1));
}


DEF("loc-contour", metaLocContour, 2, 2) {
    ENFORCE_META(metaLocContour, ARG(0), ARG(1));
    return BASE(ARG(0))->locContour(ARG(1));
}


DEF("contour", metaContour, 2, 2) {
    ENFORCE_META(metaContour, ARG(0), ARG(1));
    return BASE(ARG(0))->contour(ARG(1));
}


DEF("lookup-obo", metaLookupOBO, 3, 3) {
    ENFORCE_META(metaLookupOBO, ARG(0), ARG(1));
    return BASE(ARG(0))->lookupOBO(ARG(1), ARG(2), __CTXT__);
}


DEF("get-obo", metaGetOBO, 3, 3) {
    ENFORCE_META(metaGetOBO, ARG(0), ARG(1));
    return BASE(ARG(0))->get(ARG(1), ARG(2), __CTXT__);
}


DEF("add-obo", metaAddOBO, 4, 4) {
    ENFORCE_META(metaAddOBO, ARG(0), ARG(1));
    return BASE(ARG(0))->add(ARG(1), ARG(2), ARG(3), __CTXT__);
}


DEF("set-obo", metaSetOBO, 4, 4) {
    ENFORCE_META(metaSetOBO, ARG(0), ARG(1));
    return BASE(ARG(0))->set(ARG(1), ARG(2), ARG(3), __CTXT__);
}


DEF("lexvar", locLexvar, 3, 3) {
    CHECK_FIXNUM(0, level);
    CHECK_FIXNUM(1, offset);
    CHECK_NOVAR(2, RblBool);
    return LexVar(level, offset, ARG(2) == RBLTRUE).atom;
}


DEF("bitfield", locBitfield, 5, 5) {
    CHECK_FIXNUM(0, level);
    CHECK_FIXNUM(1, offset);
    CHECK_FIXNUM(2, span);
    CHECK_NOVAR(3, RblBool);
    CHECK_NOVAR(4, RblBool);
    return BitField(level, offset, span, ARG(3) == RBLTRUE, ARG(4) == RBLTRUE)
        .atom;
}

DEF("bitfield00", locBitField00, 3, 3) {
    CHECK_FIXNUM(0, offset);
    CHECK_FIXNUM(1, span);
    CHECK(2, RblBool, sign);
    return BitField00(offset, span, sign == RBLTRUE).atom;
}
