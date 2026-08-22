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
IS_IMPLEMENTATION

#define NeedFunctionPrototypes 0

#include "Intrinsic.h"
#include "StringDefs.h"
#include "Xm/Xm.h"
#include "Xm/Label.h"
#include "Xm/PushBG.h"
#include "Xm/RowColumn.h"
#include "Xm/CascadeB.h"
#include "Xm/Scale.h"
#include "Xm/ScrollBar.h"
#include "Xm/ScrolledW.h"
#include "Xm/SelectioB.h"
#include "Xm/SeparatoG.h"
#include "Xm/Separator.h"

WidgetClass rbl_xmLabelWidgetClass = xmLabelWidgetClass;
WidgetClass rbl_xmRowColumnWidgetClass = xmRowColumnWidgetClass;
WidgetClass rbl_xmScrollBarWidgetClass = xmScrollBarWidgetClass;
WidgetClass rbl_xmScaleWidgetClass = xmScaleWidgetClass;
WidgetClass rbl_xmCascadeButtonWidgetClass = xmCascadeButtonWidgetClass;
/* WidgetClass rbl_xmPulldownMenuWidgetClass = xmPulldownMenuWidgetClass; */

extern "C" {
void force_motif_load() {
    /* X Window manipulation */
    XDefaultRootWindow();
    XRootWindow();
    XRootWindowOfScreen();
    XCreateSimpleWindow();
    XMapWindow();
    XMoveWindow();
    XNextEvent();
    XEventsQueued();
    XCreateGC();
    XCheckTypedEvent();
    XSelectInput();
    XClearWindow();
    XResizeWindow();

    /* X Drawing functions */
    XDrawLine();
    XDrawLines();
    XDrawString();
    XDrawRectangle();
    XDrawRectangles();
    XDrawArcs();
    XDrawText();


    XmStringCreate("A", XmSTRING_DEFAULT_CHARSET);
    XmStringCreateLtoR("A", XmSTRING_DEFAULT_CHARSET);
    XmStringCreateSimple("A");

    /* Xtoolkit functions */
    XtInitialize();
    XtToolkitInitialize();
    XtGetValues();
    XtSetValues();
    /*   XtSetArg(); turns out this is a macro? (unni) */
    XtCreateManagedWidget();
    XtCreateWidget();
    XtOpenDisplay();
    XtAppCreateShell();
    XtCreateApplicationShell();
    XtRealizeWidget();
    XtNextEvent();
    XtDispatchEvent();
    XtMainLoop();
}
}
