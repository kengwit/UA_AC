/****************************************************************************
**
*A  blister.h                   GAP source                   Martin Schoenert
**
*H  @(#)$Id: blister.h,v 3.4 1993/03/19 17:28:24 martin Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
**  This  file  defines the functions  that mainly operate  on boolean lists.
**  Because boolean lists are  just a special case  of lists many  things are
**  done in the list package.
**
**  A *boolean list* is a list that has no holes and contains only 'true' and
**  'false'.  For  the full definition of  boolean list  see chapter "Boolean
**  Lists" in the  \GAP\ Manual.  Read  also the section "More  about Boolean
**  Lists" about the different internal representations of such lists.
**
**  This package consists of three parts.
**
**  The  first  part  consists  of  the  macros  'BIPEB',  'SIZE_PLEN_BLIST',
**  'PLEN_SIZE_BLIST',   'LEN_BLIST',   'SET_LEN_BLIST',   'ELM_BLIST',   and
**  'SET_ELM_BLIST'.   They  determine the  representation of boolean  lists.
**  The  rest  of the {\GAP} kernel  uses those macros  to access and  modify
**  boolean lists.
**
**  The  second  part  consists  of  the  functions  'LenBlist',  'ElmBlist',
**  'ElmsBlist',   'AssBlist',    'AsssBlist',   'PosBlist',    'PlainBlist',
**  'IsDenseBlist',  'IsPossBlist', 'EqBlist', and  'LtBlist'.  They  are the
**  functions required by the  generic lists  package.  Using these functions
**  the other parts of  the {\GAP} kernel can access and modify boolean lists
**  without actually being aware that they are dealing with a boolean list.
**
**  The  third  part  consists  of  the  functions  'IsBlist',  'FunIsBlist',
**  'FunBlistList',   'FunListBlist',   'FunSizeBlist',   'FunIsSubsetBlist',
**  'FunUniteBlist',  'FunIntersectBlist',   and  'FunSubtractBlist'.   These
**  functions make it possible to make  boolean lists, either by converting a
**  list to a boolean list,  or  by computing the characteristic boolean list
**  of  a sublist, or  by computing the union, intersection or difference  of
**  two boolean lists.
**
*H  $Log: blister.h,v $
*H  Revision 3.4  1993/03/19  17:28:24  martin
*H  added 'EqBlist'
*H
*H  Revision 3.3  1993/02/04  10:51:10  martin
*H  changed to the new list interface
*H
*H  Revision 3.2  1992/12/08  11:50:26  martin
*H  added '<list>{<positions>}'
*H
*H  Revision 3.1  1991/05/02  09:17:29  martin
*H  initial revision under RCS
*H
*H  Revision 3.0  1990/12/20  12:00:00  martin
*H  added the boolean list package
*H
*/
#ifdef SPEC_CPU2000_P64
#define long __int64
#endif /* SPEC_CPU2000_P64 */


/****************************************************************************
**
*V  BIPEB . . . . . . . . . . . . . . . . . . . . . . . . . .  bits per block
**
**  'BIPEB' is the number of bits per block, usually 32.
*/
#define BIPEB                           (sizeof(unsigned long) * 8L)


/****************************************************************************
**
*F  PLEN_SIZE_BLIST(<size>) . .  physical length from size for a boolean list
**
**  'PLEN_SIZE_BLIST'  computes  the  physical  length  (e.g.  the  number of
**  elements that could be stored  in a list) from the <size> (as reported by
**  'SIZE') for a boolean list.
**
**  Note that 'PLEN_SIZE_BLIST' is a macro, so  do not call it with arguments
**  that have sideeffects.
*/
#define PLEN_SIZE_BLIST(SIZE)           ((((SIZE)-SIZE_HD)/SIZE_HD) * BIPEB)


/****************************************************************************
**
*F  SIZE_PLEN_BLIST(<plen>)size for a boolean list with given physical length
**
**  'SIZE_PLEN_BLIST' returns  the size  that a boolean list  with  room  for
**  <plen> elements must at least have.
**
**  Note that 'SIZE_PLEN_BLIST' is a macro, so do not call it with  arguments
**  that have sideeffects.
*/
#define SIZE_PLEN_BLIST(PLEN)        (SIZE_HD+((PLEN)+BIPEB-1)/BIPEB*SIZE_HD)


/****************************************************************************
**
*F  LEN_BLIST(<hdList>) . . . . . . . . . . . . . .  length of a boolean list
**
**  'LEN_BLIST' returns the logical length of the boolean list <hdBlist>,  as
**  a C integer.
**
**  Note that 'LEN_BLIST' is a macro, so do not call it  with  arguments that
**  have sideeffects.
*/
#define LEN_BLIST(LIST)                 (HD_TO_INT(PTR(LIST)[0]))


/****************************************************************************
**
*F  SET_LEN_BLIST(<hdList>,<len>) . . . . .  set the length of a boolean list
**
**  'SET_LEN_BLIST' sets the length of the boolean list <hdList> to the value
**  <len>, which must be a positive C integer.
**
**  Note that 'SET_LEN_BLIST' is a macro, so do  not  call it with  arguments
**  that have sideeffects.
*/
#define SET_LEN_BLIST(LIST,LEN)         (PTR(LIST)[0] = INT_TO_HD(LEN))


/****************************************************************************
**
*F  ELM_BLIST(<hdList>,<pos>) . . . . . . . . . . . element of a boolean list
**
**  'ELM_BLIST'  return the  <pos>-th element of the  boolean  list <hdList>,
**  which is either 'true' or 'false'.  <pos> must be a positive integer less
**  than or equal to the length of <hdList>.
**
**  Note that 'ELM_BLIST' is a macro, so do not call it  with arguments  that
**  have sideeffects.
**
**  'ELM_BLIST' is defined in the declaration part of the package as follows:
*/

#define ELM_BLIST(LIST,POS)             \
  (((unsigned long*)(PTR(LIST)+1))[((POS)-1)/BIPEB]&((size_t)1<<((POS)-1)%BIPEB) ? \
   HdTrue : HdFalse)


/****************************************************************************
**
*F  SET_ELM_BLIST(<hdList>,<pos>,<val>) . .  set an element of a boolean list
**
**  'SET_ELM_BLIST' sets  the element  at position <pos>  in the boolean list
**  <hdList> to the  value <val>.  <pos> must be a positive integer less than
**  or equal to the  length of <hdList>.  <val>  must be either  'HdTrue'  or
**  'HdFalse'.
**
**  Note that  'SET_ELM_BLIST' is  a macro, so do not  call it with arguments
**  that have sideeffects.
*/

#define SET_ELM_BLIST(LIST,POS,VAL)     \
 ((VAL) == HdTrue ?                     \
  (((unsigned long*)(PTR(LIST)+1))[((POS)-1)/BIPEB]|=((size_t)1<<((POS)-1)%BIPEB)):\
  (((unsigned long*)(PTR(LIST)+1))[((POS)-1)/BIPEB]&=~((size_t)1<<((POS)-1)%BIPEB)))


/****************************************************************************
**
*F  LenBlist(<hdList>)  . . . . . . . . . . . . . .  length of a boolean list
**
**  'LenBlist' returns  the length  of  the  boolean  list  <hdList>  as a  C
**  integer.
**
**  'LenBlist' is the function in 'TabLenList' for boolean lists.
*/
extern  long            LenBlist P((
            TypHandle           hdList ));


/****************************************************************************
**
*F  ElmBlist(<hdList>,<pos>)  . . . . . . select an element of a boolean list
**
**  'ElmBlist'  selects  the  element at position <pos> of  the  boolean list
**  <hdList>.  It is the responsibility of the caller to ensure that <pos> is
**  a  positive integer.  An error is signalled if  <pos> is  larger than the
**  length of <hdList>.
**
**  'ElmfBlist' does  the same thing than 'ElmBlist', but need not check that
**  <pos>  is  less than or equal  to  the  length of <hdList>, this  is  the
**  responsibility of the caller.
**
**  'ElmBlist'  is  the   function   in  'TabElmBlist'  for   boolean  lists.
**  'ElmfBlist'  is  the  function  in  'TabElmfBlist',  'TabElmlBlist',  and
**  'TabElmrBlist' for boolean lists.
*/
extern  TypHandle       ElmBlist P((
            TypHandle           hdList,
            long                pos ));

extern  TypHandle       ElmfBlist P((
            TypHandle           hdList,
            long                pos ));


/****************************************************************************
**
*F  ElmsBlist(<hdList>,<hdPoss>)  . . .  select a sublist from a boolean list
**
**  'ElmsBlist'  returns a new list containing the elements at  the positions
**  given  in the list  <hdPoss> from  the boolean  list <hdList>.  It is the
**  responsibility  of the  caller  to  ensure  that  <hdPoss>  is  dense and
**  contains only positive integers.  An error is signalled  if an element of
**  <hdPoss> is larger than the length of <hdList>.
**
**  'ElmsBlist' is the function in 'TabElmsList' for boolean lists.
*/
extern  TypHandle       ElmsBlist P((
            TypHandle           hdList,
            TypHandle           hdPoss ));


/****************************************************************************
**
*F  AssBlist(<hdList>,<pos>,<hdVal>)  . . . . . . .  assign to a boolean list
**
**  'AssBlist' assigns the  value <hdVal> to the boolean list <hdList> at the
**  position <pos>.  It  is the  responsibility of the caller  to ensure that
**  <pos> is positive, and that <hdVal> is not 'HdVoid'.
**
**  'AssBlist' is the function in 'TabAssList' for boolean lists.
**
**  If <pos>  is less than or equal to the logical length of the boolean list
**  and <hdVal> is 'true' or 'false' the assignment  is  done by  setting the
**  corresponding bit.  If <pos> is one more  than the  logical length of the
**  boolean list  the  assignment  is done  by resizing  the boolean  list if
**  necessary,  setting the corresponding  bit  and incrementing the  logical
**  length  by one.  Otherwise the  boolean list is  converted to an ordinary
**  list and the assignment is performed the ordinary way.
*/
extern  TypHandle       AssBlist P((
            TypHandle           hdList,
            long                pos,
            TypHandle           hdVal ));


/****************************************************************************
**
*F  AsssBlist(<hdList>,<hdPoss>,<hdVals>)  assign several elements to a blist
**
**  'AsssBlist' assignes the values from  the list <hdVals>  at the positions
**  given in  the  list  <hdPoss> to the boolean list <hdList>.   It  is  the
**  responsibility  of  the  caller to  ensure  that  <hdPoss>  is  dense and
**  contains only positive integers, that <hdPoss> and <hdVals> have the same
**  length, and that <hdVals> is dense.
**
**  'AsssBlist' is the function in 'TabAsssList' for boolean lists.
**
**  'AsssBlist' simply  converts  the boolean list to a  plain  list and then
**  does the same  stuff as  'AsssPlist'.   This  is because a boolean is not
**  very likely to stay a boolean list after the assignment.
*/
extern  TypHandle       AsssBlist P((
            TypHandle           hdList,
            TypHandle           hdPoss,
            TypHandle           hdVals ));


/****************************************************************************
**
*F  PosBlist(<hdList>,<hdVal>,<start>) . position of an elm in a boolean list
**
**  'PosBlist'  returns  the  position  of  the first occurence of  the value
**  <hdVal>, which  may be an object of arbitrary  type, in the  boolean list
**  <hdList> after <start> as  a  C  integer.   If <hdVal> does not  occur in
**  <hdList> after <start>, then 0 is returned.
**
**  'PosBlist' is the function in 'TabPosList' for boolean lists.
*/
extern  long            PosBlist P((
            TypHandle           hdBlist,
            TypHandle           hdVal,
            long                start ));


/****************************************************************************
**
*F  PlainBlist(<hdList>)  . . .  convert a boolean list into an ordinary list
**
**  'PlainBlist' converts the boolean list <hdList> to a plain list.
**
**  'PlainBlist' is the function in 'TabPlainList' for boolean lists.
*/
extern  void            PlainBlist P((
            TypHandle           hdList ));


/****************************************************************************
**
*F  IsDenseBlist(<hdList>)  . . .  dense list test function for boolean lists
**
**  'IsDenseBlist' returns 1, since boolean lists are always dense.
**
**  'IsDenseBlist' is the function in 'TabIsDenseBlist' for boolean lists.
*/
extern  long            IsDenseBlist P((
            TypHandle           hdList ));


/****************************************************************************
**
*F  IsPossBlist(<hdList>) . .  positions list test function for boolean lists
**
**  'IsPossBlist' returns  1 if  <hdList> is  empty, and 0 otherwise, since a
**  boolean list is a positions list if and only if it is empty.
*/
extern  long            IsPossBlist P((
            TypHandle           hdList ));


/****************************************************************************
**
*F  EqBlist(<hdL>,<hdR>)  . . . . . . . . test if two boolean lists are equal
**
**  'EqBlist'  returns 'true'  if  the two boolean lists <hdL> and  <hdR> are
**  equal and 'false' otherwise.
**
**  Is called from the 'EQ' binop so both  operands  are  already  evaluated.
*/
extern  TypHandle       EqBlist P((
            TypHandle           hdL,
            TypHandle           hdR ));


/****************************************************************************
**
*F  IsBlist(<hdList>) . . . . . . . . . test whether a list is a boolean list
**
**  'IsBlist' returns 1 if the list  <hdList> is a boolean list, i.e., a list
**  that has no holes and contains only 'true'  and 'false', and 0 otherwise.
**  As a sideeffect 'IsBlist' changes the  representation  of  boolean  lists
**  into the compact representation of type 'T_BLIST' described above.
*/
extern  long            IsBlist P((
            TypHandle           hdList ));


/****************************************************************************
**
*F  FunIsBlist(<hdCall>)  . . . . . . . . test if an object is a boolean list
**
**  'FunIsBlist' implements the internal function 'IsBlist'.
**
**  'IsBlist( <obj> )'
**
**  'IsBlist' returns 'true' if the  object  <obj>  is  a  boolean  list  and
**  'false' otherwise.  An object is a boolean list if it is a lists  without
**  holes containing only 'true' and 'false'.  Will cause an  error if  <obj>
**  <obj> is an unbound variable.
*/
extern  TypHandle       FunIsBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunBlistList(<hdCall>)  . . . . . . .  make a boolean list from a sublist
**
**  'FunBlistList' implements the internal function 'BlistList'.
**
**  'BlistList( <list>, <sub> )'
**
**  'BlistList'  creates a boolean  list   that describes the  list <sub>  as
**  sublist of the  list <list>.  The  result is a  new boolean list <blist>,
**  which has the same  length as <list>, such  that '<blist>[<i>]' is 'true'
**  if '<list>[<i>]' is an element of <sub> and 'false' otherwise.
**
**  'BlistList' is most effective if <list> is a set, but can be used with an
**  arbitrary list that has no holes.
*/
extern  TypHandle       FunBlistList P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunListBlist(<hdCall>)  . . . . . . .  make a sublist from a boolean list
**
**  'FunListBlist' implements the internal function 'ListBlist'.
**
**  'ListBlist( <list>, <blist> )'
**
**  'ListBlist' returns the  sublist of the  elements of the list  <list> for
**  which the boolean list   <blist>, which must   have  the same  length  as
**  <list>, contains 'true'.  The order of the elements in the result is  the
**  same as in <list>.
*/
extern  TypHandle       FunListBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunSizeBlist(<hdCall>)  . . .  number of 'true' entries in a boolean list
**
**  'FunSizeBlist' implements the internal function 'SizeBlist'
**
**  'SizeBlist( <blist> )'
**
**  'SizeBlist' returns the  number of entries  of the boolean  list  <blist>
**  that are 'true'.
*/
extern  TypHandle       FunSizeBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunIsSubsetBlist(<hdCall>)  . test if a boolean list is subset of another
**
**  'FunIsSubsetBlist' implements the internal function 'IsSubsetBlist'.
**
**  'IsSubsetBlist( <blist1>, <blist2> )'
**
**  'IsSubsetBlist' returns 'true' if  the boolean list <blist2> is  a subset
**  of the boolean list <list1>, which must have equal length.  <blist2> is a
**  subset if <blist1> if '<blist2>[<i>] >= <blist1>[<i>]' for all <i>.
*/
extern  TypHandle       FunIsSubsetBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunUniteBlist(<hdCall>) . . . . . . . unite one boolean list with another
**
**  'FunUniteBlist' implements the internal function 'UniteBlist'.
**
**  'UniteBlist( <blist1>, <blist2> )'
**
**  'UniteBlist'  unites  the  boolean list  <blist1>  with  the boolean list
**  <blist2>,  which  must  have the   same  length.  This  is  equivalent to
**  assigning '<blist1>[<i>] := <blist1>[<i>] or <blist2>[<i>]' for all <i>.
*/
extern  TypHandle       FunUniteBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunIntersectBlist(<hdCall>) . . . intersect one boolean list with another
**
**  'FunIntersectBlist' implements the function 'IntersectBlist'.
**
**  'IntersectBlist( <blist1>, <blist2> )'
**
**  'IntersectBlist' intersects the boolean list   <blist1> with the  boolean
**  list <blist2>, which must  have the same  length.  This is equivalent  to
**  assigning '<blist1>[<i>] := <blist1>[<i>] and <blist2>[<i>]' for all <i>.
*/
extern  TypHandle       FunIntersectBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  FunSubtractBlist(<hdCall>)  . . .  subtract one boolean list from another
**
**  'FunSubtractBlist' implements the internal function 'SubtractBlist'.
**
**  'SubtractBlist( <blist1>, <blist2> )'
**
**  'SubtractBlist' subtracts the boolean list <blist2> from the boolean list
**  <blist1>, which must have the same  length.  This is equivalent assigning
**  '<blist1>[<i>] := <blist1>[<i>] and not <blist2>[<i>]' for all <i>.
*/
extern  TypHandle       FunSubtractBlist P((
            TypHandle           hdCall ));


/****************************************************************************
**
*F  InitBlist() . . . . . . . . . . . . . initialize the boolean list package
**
**  'InitBlist' initializes the boolean list package.
*/
extern  void            InitBlist P(( void ));


/****************************************************************************
**
*E  Emacs . . . . . . . . . . . . . . . . . . . . . . . local emacs variables
**
**  Local Variables:
**  mode:               outline
**  outline-regexp:     "*A\\|*F\\|*V\\|*T\\|*E"
**  fill-column:        73
**  fill-prefix:        "**  "
**  eval:               (local-set-key "\t" 'c-indent-command)
**  eval:               (local-set-key ";"  'electric-c-semi )
**  eval:               (local-set-key "{"  'electric-c-brace)
**  eval:               (local-set-key "}"  'electric-c-brace)
**  eval:               (hide-body)
**  End:
*/



