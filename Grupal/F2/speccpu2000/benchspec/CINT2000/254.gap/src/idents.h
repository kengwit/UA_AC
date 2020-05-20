/****************************************************************************
**
*A  idents.h                    GAP source                   Martin Schoenert
**
*A  @(#)$Id: idents.h,v 3.1 1991/04/30 16:12:23 martin Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
**  This file contains the functions for mapping  identifiers  to  variables.
**
*H  $Log: idents.h,v $
*H  Revision 3.1  1991/04/30  16:12:23  martin
*H  initial revision under RCS
*H
*H  Revision 3.0  1991/01/15  12:00:00  martin
*H  fixed 'x ->' yields 'warning, undefined variable'
*H
*/
#ifdef SPEC_CPU2000_P64
#define long __int64
#endif /* SPEC_CPU2000_P64 */


/****************************************************************************
**
*F  PushFunction( <hdFun> ) . . . . . . . . add another function to the stack
**
**  'PushFunction' adds another function to the  function  definition  stack.
**  It is called from the reader when the reader starts to parse a  function.
**  It makes the local variables known to 'FindIdent'.
*/
extern  void            PushFunction P(( TypHandle hdFun ));


/****************************************************************************
**
*F  PopFunction() . . . . . . . . . .  remove another function from the stack
**
**  'PopFunction'  removes  the  most  recent  function  from  the   function
**  definition stack.  It is called from the reader when the reader  finished
**  parsing a function.  It makes the local variables again unknown.
*/
extern  void            PopFunction P(( void ));


/****************************************************************************
**
*V  IsUndefinedGlobal . . . . .  true if a variable is an new global variable
**
**  'IsUndefinedGlobal' is set by 'FindIdent'  if  <name>  is  a  new  global
**  variable inside a function definition.  'RdVar' tests 'IsUndefinedGlobal'
**  and complains about such variables.
*/
extern  unsigned long   IsUndefinedGlobal;


/****************************************************************************
**
*F  FindIdent( <name> ) . . . . . . . . . . . find variable for an identifier
**
**  'FindIdent' returns the handle of  the  variable  bag  for  the  variable
**  with the identifier  <name>.  'FindIdent'  first  searches  the  function
**  definition bags made added to the stack by 'PushFunction'.  If  no  local
**  variable has this identifier, 'FindIdent'  looks in the global identifier
**  table.  If the identifier is also not found in the  global  table  a  new
**  global variable is created.
*/
extern  TypHandle       FindIdent P(( char name [] ));


/****************************************************************************
**
*F  FindRecname( <name> ) . . . .  find the record name bag for a record name
**
**  'FindRecname' returns the record name bag for  the  record  name  <name>.
**  Note that record names are always stored unique, i.e., for  every  string
**  there is a unique record name bag for that string.  This makes it  easier
**  to find a record element for a given record  name:  We  do  not  have  to
**  compare strings, it is enough to compare handles.
*/
extern  TypHandle       FindRecname P(( char name [] ));


/****************************************************************************
**
*F  InitIdents()  . . . . . . . . . . . . . . . initialize identifier package
**
**  'InitIdents' initializes the identifier package. This must be done before
**  the  first  call  to  'FindIdent'  or  'FindRecname',  i.e.,  before  the
**  evaluator packages are initialized.
*/
extern  void            InitIdents P(( void ));



