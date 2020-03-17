/****************************************************************************
**
*A  range.c                     GAP source                   Martin Schoenert
**
*H  @(#)$Id: range.c,v 3.10 1994/07/04 08:42:32 mschoene Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
**  This file contains the functions that mainly deal with ranges.  As ranges
**  are a special case of lists many things are done in the list package.
**
**  A *range* is  a list without  holes  consisting  of consecutive integers.
**  For the full definition of ranges see chapter "Ranges" in the GAP Manual.
**  Read  also   "More about Ranges"  about  the different  representation of
**  ranges.
**
**  A list that is  known to be  a  range is  represented  by a  bag of  type
**  'T_RANGE', which has the following format:
**
**      +-------+-------+-------+
**      |logical| first | incr- |
**      | length|element| ement |
**      +-------+-------+-------+
**
**  The first entry is the handle of the logical length.  The second entry is
**  the first element of the range.  The last  entry  is the  increment.  All
**  three are represented as immediate GAP integers.
**
**  The element at position <pos> is thus simply <first> + (<pos>-1) * <inc>.
**
**  Note  that  a list  represented by a   bag of type   'T_LIST', 'T_SET' or
**  'T_VECTOR' might still  be a range.  It is  just that the kernel does not
**  know this.
**
**  This package consists of three parts.
**
**  The  first  part  consists  of the  macros  'LEN_RANGE', 'SET_LEN_RANGE',
**  'LOW_RANGE',   'SET_FIRST_RANGE',   'INC_RANGE',   'SET_INC_RANGE',   and
**  'ELM_RANGE'.   They determine the  representation of  ranges.  Everything
**  else in this file and the rest of the {\GAP} kernel  uses those macros to
**  access and modify ranges.
**
**  The  second  part  consists  of  the  functions  'LenRange',  'ElmRange',
**  'ElmsRange',    'AssRange',   'AsssRange',   'PosRange',    'PlainRange',
**  'IsDenseRange', 'IsPossRange', 'PrRange', 'EqRange', and 'LtRange'.  They
**  are the functions required by the  generic  lists package.   Using  these
**  functions  the other parts  of the {\GAP}  kernel  can  access or  modify
**  ranges without actually being aware that they are dealing with a range.
**
**  The  third part consists  of the  functions 'EvMakeRange', 'PrMakeRange',
**  'IsRange',  and 'FunIsRange'.  These functions make it possible  to  make
**  ranges, either by  evaluating  a range  literal, or by converting another
**  list to a range.
**
*H  $Log: range.c,v $
*H  Revision 3.10  1994/07/04  08:42:32  mschoene
*H  changed 'IsRange' to avoid converting lists of length 2
*H
*H  Revision 3.9  1993/03/19  10:04:35  martin
*H  fixed a bug in '<range>{ <range> }'
*H
*H  Revision 3.8  1993/03/11  15:52:47  fceller
*H  changed "divisable" into "divisible" (thanx Michel)
*H
*H  Revision 3.7  1993/02/04  10:51:10  martin
*H  changed to the new list interface
*H
*H  Revision 3.6  1992/12/08  11:40:54  martin
*H  added '<list>{<positions>}'
*H
*H  Revision 3.5  1992/04/27  14:24:36  martin
*H  fixed 'PosRange' (and 'in <range>') for negative values
*H
*H  Revision 3.4  1991/05/16  16:42:46  martin
*H  improved 'PosRange' for large integers
*H
*H  Revision 3.3  1991/04/30  16:12:34  martin
*H  initial revision under RCS
*H
*H  Revision 3.2  1990/12/19  12:00:00  martin
*H  improved 'Position' to accept a starting position
*H
*H  Revision 3.1  1990/12/19  12:00:00  martin
*H  improved the list like objects package interface
*H
*H  Revision 3.0  1990/12/06  12:00:00  martin
*H  added yet another list package
*H
*/

#include        "system.h"              /* system dependent functions      */
#include        "gasman.h"              /* dynamic storage manager         */
#include        "scanner.h"             /* reading of tokens and printing  */
#include        "eval.h"                /* evaluator main dispatcher       */
#include        "integer.h"             /* arbitrary size integers         */

#include        "list.h"                /* generic list package            */
#include        "plist.h"               /* 'LEN_PLIST', 'SET_LEN_PLIST',.. */

#include        "range.h"               /* declaration part of the package */


/****************************************************************************
**
*F  SIZE_PLEN_RANGE(<plen>) . . . . . . size from physical length for a range
**
**  'SIZE_PLEN_RANGE' returns the size that the bag for a range with room for
**  <plen> elements must have.
**
**  Note that 'SIZE_PLEN_RANGE' is a macro, so do not call it with  arguments
**  that have sideeffects.
**
**  'SIZE_PLEN_RANGE'  is defined in the declaration  part of this package as
**  follows:
**
#define SIZE_PLEN_RANGE(PLEN)           (3 * SIZE_HD)
*/


/****************************************************************************
**
*F  LEN_RANGE(<hdRange>)  . . . . . . . . . . . . . . . . . length of a range
**
**  'LEN_RANGE' returns the logical length  of  the  range <hdRange>,  as a C
**  integer.
**
**  Note that 'LEN_RANGE' is a macro, so do not call it with  arguments  that
**  have sideeffects.
**
**  'LEN_RANGE' is defined in the declaration part of the package as follows:
**
#define LEN_RANGE(LIST)                 HD_TO_INT(PTR(LIST)[0])
*/


/****************************************************************************
**
*F  SET_LEN_RANGE(<hdRange>,<len>)  . . . . . . . . set the length of a range
**
**  'SET_LEN_RANGE'  sets  the  length  of the range <hdRange>  to  the value
**  <len>, which must be a C integer larger than 1.
**
**  Note that 'SET_LEN_RANGE' is a macro,  so  do not  call it with arguments
**  that have sideeffects.
**
**  'SET_LEN_RANGE' is  defined in the  declaration part of  this  package as
**  follows:
**
#define SET_LEN_RANGE(LIST,LEN)         (PTR(LIST)[0] = INT_TO_HD(LEN))
*/


/****************************************************************************
**
*F  LOW_RANGE(<hdRange>)  . . . . . . . . . . . . .  first element of a range
**
**  'LOW_RANGE'  returns the  first element of  the  range  <hdRange>  as a C
**  integer.
**
**  Note that 'LOW_RANGE' is a macro,  so do not call it  with arguments that
**  have sideeffects.
**
**  'LOW_RANGE' is  defined  in  the  declaration  part  of this  package  as
**  follows:
**
#define LOW_RANGE(LIST)                 HD_TO_INT(PTR(LIST)[1])
*/


/****************************************************************************
**
*F  SET_LOW_RANGE(<hdRange>,<low>)  . . . .  set the first element of a range
**
**  'SET_LOW_RANGE' sets the  first  element  of  the range <hdRange>  to the
**  value <low>, which must be a C integer.
**
**  Note  that 'SET_LOW_RANGE' is a macro, so do not call  it with  arguments
**  that have sideeffects.
**
**  'SET_LOW_RANGE' is defined  in  the  declaration  part of this package as
**  follows:
**
#define SET_LOW_RANGE(LIST,LOW)         (PTR(LIST)[1] = INT_TO_HD(LOW))
*/


/****************************************************************************
**
*F  INC_RANCE(<hdRange>)  . . . . . . . . . . . . . . .  increment of a range
**
**  'INC_RANGE' returns the increment of the range <hdRange> as a C integer.
**
**  Note that 'INC_RANGE' is a macros,  so do not call it with arguments that
**  have sideeffects.
**
**  'INC_RANGE'  is  defined  in  the declaration  part  of  this  package as
**  follows:
**
#define INC_RANGE(LIST)                 HD_TO_INT(PTR(LIST)[2])
*/


/****************************************************************************
**
*F  SET_INC_RANGE(<hdRange>,<inc>)  . . . . . .  set the increment of a range
**
**  'SET_INC_RANGE'  sets the  increment of the range <hdRange> to  the value
**  <inc>, which must be a C integer.
**
**  Note that  'SET_INC_RANGE' is a macro,  so do  not call it with arguments
**  that have sideeffects.
**
**  'SET_INC_RANGE'  is defined in  the declaration part  of this  package as
**  follows:
**
#define SET_INC_RANGE(LIST,INC)         (PTR(LIST)[2] = INT_TO_HD(INC))
*/


/****************************************************************************
**
*F  ELM_RANGE(<hdRange>,<i>)  . . . . . . . . . . . . . .  element of a range
**
**  'ELM_RANGE' return the <i>-th element of the  range <hdRange>.  <i>  must
**  be a positive integer less than or equal to the length of <hdRange>.
**
**  Note that 'ELM_RANGE' is a macro, so do not call it with  arguments  that
**  have sideeffects.
**
**  'ELM_RANGE' is defined in the declaration part of the package as follows:
**
#define ELM_RANGE(L,POS)        INT_TO_HD(LOW_RANGE(L)+(POS-1)*INC_RANGE(L))
*/


/****************************************************************************
**
*F  LenRange(<hdList>)  . . . . . . . . . . . . . . . . . . length of a range
**
**  'LenRange' returns the length of the range <hdList> as a C integer.
**
**  'LenRange' is the function in 'TabLenList' for ranges.
*/
long            LenRange ( hdList )
    TypHandle           hdList;
{
    return LEN_RANGE( hdList );
}


/****************************************************************************
**
*F  ElmRange(<hdList>,<pos>)  . . . . . . . . .  select an element of a range
**
**  'ElmRange' selects the element at position <pos>  of the range  <hdList>.
**  It is the responsibility of the caller to ensure that <pos> is a positive
**  integer.  An error is  signaller if <pos>  is larger than  the length  of
**  <hdList>.
**
**  'ElmfRange' does  the same thing than 'ElmRange', but need not check that
**  <pos> is  less than or  equal to  the  length of  <hdList>,  this is  the
**  responsibility of the caller.
**
**  'ElmRange' is the  function in  'TabElmList' for ranges.   'ElmfRange' is
**  the  function in  'TabElmfList',  'TabElmlList',  and  'TabElmrList'  for
**  ranges.
*/
TypHandle       ElmRange ( hdList, pos )
    TypHandle           hdList;
    long                pos;
{
    /* check the position                                                  */
    if ( LEN_RANGE( hdList ) < pos ) {
        return Error(
          "List Element: <list>[%d] must have a value",
                     pos, 0L );
    }

    /* return the selected element                                         */
    return ELM_RANGE( hdList, pos );
}

TypHandle       ElmfRange ( hdList, pos )
    TypHandle           hdList;
    long                pos;
{
    return ELM_RANGE( hdList, pos );
}


/****************************************************************************
**
*F  ElmsRange(<hdList>,<hdPoss>)  . . . . . . . select a sublist from a range
**
**  'ElmsRange' returns a new list containing  the elements  at the positions
**  given  in  the  list  <hdPoss>  from  the  range  <hdList>.   It  is  the
**  responsibility of  the  caller  to ensure  that  <hdPoss>  is  dense  and
**  contains only  positive integers.  An error is signalled if an element of
**  <hdPoss> is larger than the length of <hdList>.
**
**  'ElmsRange' is the function in 'TabElmsList' for ranges.
*/
TypHandle       ElmsRange ( hdList, hdPoss )
    TypHandle           hdList;
    TypHandle           hdPoss;
{
    TypHandle           hdElms;         /* selected sublist, result        */
    long                lenList;        /* length of <list>                */
    TypHandle           hdElm;          /* one element from <list>         */
    long                lenPoss;        /* length of <positions>           */
    long                pos;            /* <position> as integer           */
    long                inc;            /* increment in a range            */
    long                i;              /* loop variable                   */

    /* general code                                                        */
    if ( TYPE(hdPoss) != T_RANGE ) {

        /* get the length of <list>                                        */
        lenList = LEN_RANGE( hdList );

        /* get the length of <positions>                                   */
        lenPoss = LEN_LIST( hdPoss );

        /* make the result list                                            */
        hdElms = NewBag( T_LIST, SIZE_PLEN_PLIST( lenPoss ) );
        SET_LEN_PLIST( hdElms, lenPoss );

        /* loop over the entries of <positions> and select                 */
        for ( i = 1; i <= lenPoss; i++ ) {

            /* get <position>                                              */
            pos = HD_TO_INT( ELMF_LIST( hdPoss, i ) );
            if ( lenList < pos ) {
                return Error(
                  "List Elements: <list>[%d] must have a value",
                             pos, 0L );
            }

            /* select the element                                          */
            hdElm = ELM_RANGE( hdList, pos );

            /* assign the element into <elms>                              */
            SET_ELM_PLIST( hdElms, i, hdElm );

        }

    }

    /* special code for ranges                                             */
    else {

        /* get the length of <list>                                        */
        lenList = LEN_RANGE( hdList );

        /* get the length of <positions>, the first elements, and the inc. */
        lenPoss = LEN_RANGE( hdPoss );
        pos = LOW_RANGE( hdPoss );
        inc = INC_RANGE( hdPoss );

        /* check that no <position> is larger than 'LEN_LIST(<list>)'      */
        if ( lenList < pos ) {
            return Error(
              "List Elements: <list>[%d] must have a value",
                         pos, 0L );
        }
        if ( lenList < pos + (lenPoss-1) * inc ) {
            return Error(
              "List Elements: <list>[%d] must have a value",
                         pos + (lenPoss-1) * inc, 0L );
        }

        /* make the result range                                           */
        hdElms = NewBag( T_RANGE, SIZE_PLEN_RANGE( lenPoss ) );
        SET_LEN_RANGE( hdElms, lenPoss );
        SET_LOW_RANGE( hdElms, HD_TO_INT( ELM_RANGE( hdList, pos ) ) );
        SET_INC_RANGE( hdElms, inc * INC_RANGE( hdList ) );

    }

    /* return the result                                                   */
    return hdElms;
}


/****************************************************************************
**
*F  AssRange(<hdList>,<pos>,<hdVal>)  . . . . . . . . . . . assign to a range
**
**  'AssRange'  assigns  the value  <hdVal> to  the  range  <hdList>  at  the
**  position <pos>.  It is the  responsibility  of the caller to ensure  that
**  <pos> is positive, and that <hdVal> is not 'HdVoid'.
**
**  'AssRange' is the function in 'TabAssList' for ranges.
**
**  'AssRange' simply converts the range into a plain list, and then does the
**  same stuff as 'AssPlist'.   This is because a range is not very likely to
**  stay a range after the assignment.
*/
TypHandle       AssRange ( hdList, pos, hdVal )
    TypHandle           hdList;
    long                pos;
    TypHandle           hdVal;
{
    long                plen;           /* physical length of <list>       */

    /* convert the range into a plain list                                 */
    PLAIN_LIST( hdList );
    Retype( hdList, T_LIST );

    /* resize the list if necessary                                        */
    if ( LEN_PLIST( hdList ) < pos ) {
        plen = PLEN_SIZE_PLIST( SIZE(hdList) );
        if ( plen + plen/8 + 4 < pos )
            Resize( hdList, SIZE_PLEN_PLIST( pos ) );
        else if ( plen < pos )
            Resize( hdList, SIZE_PLEN_PLIST( plen + plen/8 + 4 ) );
        SET_LEN_PLIST( hdList, pos );
    }

    /* now perform the assignment and return the assigned value            */
    SET_ELM_PLIST( hdList, pos, hdVal );
    return hdVal;
}


/****************************************************************************
**
*F  AsssRange(<hdList>,<hdPoss>,<hdVals>)  assign several elements to a range
**
**  'AsssRange'  assignes the values  from the list <hdVals> at the positions
**  given   in   the  list   <hdPoss>  to  the  range <hdList>.   It  is  the
**  responsibility  of the caller  to  ensure  that  <hdPoss>  is  dense  and
**  contains only positive integers, that <hdPoss> and <hdVals> have the same
**  length, and that <hdVals> is dense.
**
**  'AsssRange' is the function in 'TabAsssList' for ranges.
**
**  'AsssRange' simply converts the range to a plain  list  and then does the
**  same stuff as 'AsssPlist'.  This is because a range is not very likely to
**  stay a range after the assignment.
*/
TypHandle       AsssRange ( hdList, hdPoss, hdVals )
    TypHandle           hdList;
    TypHandle           hdPoss;
    TypHandle           hdVals;
{
    /* convert <list> to a plain list                                      */
    PLAIN_LIST( hdList );
    Retype( hdList, T_LIST );

    /* and delegate                                                        */
    return ASSS_LIST( hdList, hdPoss, hdVals );
}


/****************************************************************************
**
*F  PosRange(<hdRange>,<hdVal>,<start>) . . position of an element in a range
**
**  'PosRange' returns the  position  of  the  value  <hdVal>  in  the  range
**  <hdList> after the first position <start> as a  C integer.  0 is returned
**  if <hdVal> is not in the list.
**
**  'PosRange' is the function in 'TabPosList' for ranges.
*/
long            PosRange ( hdList, hdVal, start )
    TypHandle           hdList;
    TypHandle           hdVal;
    long                start;
{
    long                k;              /* position, result                */
    long                lenList;        /* length of <list>                */
    long                low;            /* first element of <list>         */
    long                inc;            /* increment of <list>             */
    long                val;            /* numerical value of <val>        */

    /* get the length, the first element, and the increment of <list>      */
    lenList = LEN_RANGE(hdList);
    low     = LOW_RANGE(hdList);
    inc     = INC_RANGE(hdList);

    /* look just beyond the end                                            */
    if ( start == lenList ) {
        k = 0;
    }

    /* look for an integer                                                 */
    else if ( TYPE(hdVal) == T_INT ) {
        val = HD_TO_INT(hdVal);
        if ( 0 < inc
          && low + start * inc <= val && val <= low + (lenList-1) * inc
          && (val - low) % inc == 0 ) {
            k = (val - low) / inc + 1;
        }
        else if ( inc < 0
          && low + (lenList-1) * inc <= val && val <= low + start * inc
          && (val - low) % inc == 0 ) {
            k = (val - low) / inc + 1;
        }
        else {
            k = 0;
        }
    }

    /* for a record compare every entry                                    */
    else if ( TYPE(hdVal) == T_REC ) {
        for ( k = start+1; k <= lenList; k++ ) {
            if ( EQ( INT_TO_HD( low + (k-1) * inc ), hdVal ) == HdTrue )
                break;
        }
        if ( lenList < k ) {
            k = 0;
        }
    }

    /* otherwise it can not be an element of the range                     */
    else {
        k = 0;
    }

    /* return the position                                                 */
    return k;
}


/****************************************************************************
**
*F  PlainRange(<hdList>)  . . . . . . . . . . convert a range to a plain list
**
**  'PlainRange' converts the range <hdList> to a plain list.
**
**  'PlainRange' is the function in 'TabPlainList' for ranges.
*/
void            PlainRange ( hdList )
    TypHandle           hdList;
{
    long                lenList;        /* length of <list>                */
    long                low;            /* first element of <list>         */
    long                inc;            /* increment of <list>             */
    long                i;              /* loop variable                   */

    /* get the length, the first element, and the increment of <list>      */
    lenList = LEN_RANGE( hdList );
    low     = LOW_RANGE( hdList );
    inc     = INC_RANGE( hdList );

    /* change the type of the list, and allocate enough space              */
    Retype( hdList, T_LIST );
    Resize( hdList, SIZE_PLEN_PLIST( lenList ) );
    SET_LEN_PLIST( hdList, lenList );

    /* enter the values in <list>                                          */
    for ( i = 1; i <= lenList; i++ ) {
        SET_ELM_PLIST( hdList, i, INT_TO_HD( low + (i-1) * inc ) );
    }

}


/****************************************************************************
**
*F  IsDenseRange(<hdList>)  . . . . . . . dense list test function for ranges
**
**  'IsDenseRange' returns 1, since ranges are always dense.
**
**  'IsDenseRange' is the function in 'TabIsDenseList' for ranges.
*/
long            IsDenseRange ( hdList )
    TypHandle           hdList;
{
    return 1;
}


/****************************************************************************
**
*F  IsPossRange(<hdList>) . . . . . . positions list test function for ranges
**
**  'IsPossRange' returns 1 if  the range <hdList> is a dense list containing
**  only positive integers, and 0 otherwise.
**
**  'IsPossRange' is the function in 'TabIsPossList' for ranges.
*/
long            IsPossRange ( hdList )
    TypHandle           hdList;
{
    /* test if the first element is positive                               */
    if ( LOW_RANGE( hdList ) <= 0 )
        return 0;

    /* test if the last element is positive                                */
    if ( HD_TO_INT( ELM_RANGE( hdList, LEN_RANGE(hdList) ) ) <= 0 )
        return 0;

    /* otherwise <list> is a positions list                                */
    return 1;
}


/****************************************************************************
**
*F  PrRange(<hdRange>)  . . . . . . . . . . . . . . . . . . . . print a range
**
**  'PrRange' prints the range <hdRange>.
**
**  'PrRange' handles bags of type 'T_RANGE' and 'T_MAKERANGE'.
*/
void            PrRange ( hdRange )
    TypHandle           hdRange;
{
    Pr( "%2>[ %2>%d",
        LOW_RANGE(hdRange), 0L );
    if ( INC_RANGE(hdRange) != 1 )
        Pr( "%<,%< %2>%d",
            LOW_RANGE(hdRange)+INC_RANGE(hdRange), 0L );
    Pr( "%2< .. %2>%d%4< ]",
        LOW_RANGE(hdRange)+(LEN_RANGE(hdRange)-1)*INC_RANGE(hdRange),0L);
}


/****************************************************************************
**
*F  EqRange(<hdL>,<hdR>)  . . . . . . . . . . .  test if two ranges are equal
**
**  'EqRange' returns 'true' if the two  ranges <hdL> and <hdR> are equal and
**  'false' otherwise.
**
**  Is  called from the 'EQ' binop  so both  operands are  already evaluated.
*/
TypHandle       EqRange ( hdL, hdR )
    TypHandle           hdL;
    TypHandle           hdR;
{
    if ( LEN_RANGE(hdL) == LEN_RANGE(hdR)
      && LOW_RANGE(hdL) == LOW_RANGE(hdR)
      && INC_RANGE(hdL) == INC_RANGE(hdR) ) {
        return HdTrue;
    }
    else {
        return HdFalse;
    }
}


/****************************************************************************
**
*F  LtRange(<hdL>,<hdR>)  . . . . . . . . . . .  test if two ranges are equal
**
**  'LtRange' returns 'true' if the range  <hdL> is less than the range <hdR>
**  and 'false' otherwise.
**
**  Is called from the 'LT' binop so both operands are already evaluated.
*/
TypHandle       LtRange ( hdL, hdR )
    TypHandle           hdL;
    TypHandle           hdR;
{
    /* first compare the first elements                                    */
    if ( LOW_RANGE(hdL) < LOW_RANGE(hdR) )
        return HdTrue;
    else if ( LOW_RANGE(hdR) < LOW_RANGE(hdL) )
        return HdFalse;

    /* next compare the increments (or the second elements)                */
    if ( INC_RANGE(hdL) < INC_RANGE(hdR) )
        return HdTrue;
    else if ( INC_RANGE(hdR) < INC_RANGE(hdL) )
        return HdFalse;

    /* finally compare the lengths                                         */
    if ( LEN_RANGE(hdL) < LEN_RANGE(hdR) )
        return HdTrue;
    else if ( LEN_RANGE(hdR) < LEN_RANGE(hdL) )
        return HdFalse;

    /* the two ranges are equal                                            */
    return HdFalse;
}


/****************************************************************************
**
*F  EvMakeRange(<hdMake>) . . .  convert a variable range into a constant one
**
**  'EvMakeRange' turns the literal  range  <hdMake>  into  a  constant  one.
*/
TypHandle       EvMakeRange ( hdMake )
    TypHandle           hdMake;
{
    TypHandle           hdRange;        /* handle of the result            */
    TypHandle           hdL;            /* handle of the first element     */
    long                low;            /* low value                       */
    TypHandle           hdH;            /* handle of the last element      */
    long                high;           /* high value                      */
    long                inc;            /* increment                       */

    /* evaluate the low value                                              */
    hdL = EVAL( PTR(hdMake)[0] );
    if ( TYPE(hdL) != T_INT )
        return Error("Range: <low> must be an integer",0L,0L);
    low = HD_TO_INT( hdL );

    /* evaluate the second value (if present)                              */
    if ( SIZE( hdMake ) == 3 * SIZE_HD ) {
        hdH = EVAL( PTR(hdMake)[1] );
        if ( TYPE(hdH) != T_INT )
            return Error("Range: <second> must be an integer",0L,0L);
        if ( HD_TO_INT( hdH ) == low )
            return Error("Range: <second> must not be equal to <low>",0L,0L);
        inc = HD_TO_INT( hdH ) - low;
    }
    else {
        inc = 1;
    }

    /* evaluate the high value                                             */
    hdH = EVAL( PTR(hdMake)[SIZE(hdMake)/SIZE_HD-1] );
    if ( TYPE( hdH ) != T_INT )
        return Error("Range: <high> must be an integer",0L,0L);
    high = HD_TO_INT( hdH );

    /* check that <high>-<low> is divisable by <inc>                       */
    if ( (high - low) % inc != 0 )
        return Error("Range: <high>-<low> must be divisible by <inc>",0L,0L);

    /* if <low> is larger than <high> the range is empty                   */
    if ( (0 < inc && high < low) || (inc < 0 && low < high) ) {
        hdRange = NewBag( T_LIST, SIZE_PLEN_PLIST( 0 ) );
        SET_LEN_PLIST( hdRange, 0 );
    }

    /* if <low> is equal to <high> the range is a singleton list           */
    else if ( low == high ) {
        hdRange = NewBag( T_LIST, SIZE_PLEN_PLIST( 1 ) );
        SET_LEN_PLIST( hdRange, 1 );
        SET_ELM_PLIST( hdRange, 1, INT_TO_HD( low ) );
    }

    /* else make the range                                                 */
    else {
        hdRange = NewBag( T_RANGE, SIZE_PLEN_RANGE( (high-low) / inc + 1 ) );
        SET_LEN_RANGE( hdRange, (high-low) / inc + 1 );
        SET_LOW_RANGE( hdRange, low );
        SET_INC_RANGE( hdRange, inc );
    }

    /* return the range                                                    */
    return hdRange;
}


/****************************************************************************
**
*F  PrMakeRange(<hdMake>) . . . . . . . . . . . . . . . print a range literal
**
**  'PrMakeRange' prints the range literal  <hdMake> in the form '[  <low> ..
**  <high> ]'.
*/
void            PrMakeRange ( hdMake )
    TypHandle           hdMake;
{
    if ( SIZE( hdMake ) == 2 * SIZE_HD ) {
        Pr("%2>[ %2>",0L,0L);    Print( PTR(hdMake)[0] );
        Pr("%2< .. %2>",0L,0L);  Print( PTR(hdMake)[1] );
        Pr(" %4<]",0L,0L);
    }
    else {
        Pr("%2>[ %2>",0L,0L);    Print( PTR(hdMake)[0] );
        Pr("%<,%< %2>",0L,0L);   Print( PTR(hdMake)[1] );
        Pr("%2< .. %2>",0L,0L);  Print( PTR(hdMake)[2] );
        Pr(" %4<]",0L,0L);
    }
}


/****************************************************************************
**
*F  IsRange(<hdList>) . . . . . . . . . . . . . . . test if a list is a range
**
**  'IsRange' returns 1 if the list with the handle <hdList> is a range and 0
**  otherwise.  As a  sideeffect 'IsRange' converts proper ranges represented
**  the ordinary way to the compact representation.
*/
long            IsRange ( hdList )
    TypHandle           hdList;
{
    long                isRange;        /* result of the test              */
    long                len;            /* logical length of list          */
    long                low;            /* value of first element of range */
    long                inc;            /* increment                       */
    long                i;              /* loop variable                   */

    /* if <hdList> is represented as a range, it is of course a range      */
    if ( TYPE(hdList) == T_RANGE ) {
        isRange = 1;
    }

    /* if <hdList> is not a list, it is not a range                        */
    else if ( ! IS_LIST( hdList ) ) {
        isRange = 0;
    }

    /* if <hdList> is the empty list, it is a range by definition          */
    else if ( LEN_LIST(hdList) == 0 ) {
        isRange = 1;
    }

    /* if <hdList> is a list with just one integer, it is also a range     */
    else if ( LEN_LIST(hdList)==1 && TYPE(ELMF_LIST(hdList,1))==T_INT ) {
        isRange = 1;
    }

    /* if the first element is not an integer, it is not a range           */
    else if ( ELMF_LIST(hdList,1)==0 || TYPE(ELMF_LIST(hdList,1))!=T_INT ) {
        isRange = 0;
    }

    /* if the second element is not an integer, it is not a range          */
    else if ( ELMF_LIST(hdList,2)==0 || TYPE(ELMF_LIST(hdList,2))!=T_INT ) {
        isRange = 0;
    }

    /* if the first and the second element are equal it is also not a range*/
    else if ( ELMF_LIST(hdList,1) == ELMF_LIST(hdList,2) ) {
        isRange = 0;
    }

    /* otherwise, test if the elements are consecutive integers            */
    else {

        /* get the logical length of the list                              */
        len = LEN_LIST(hdList);
        low = HD_TO_INT( ELMF_LIST( hdList, 1 ) );
        inc = HD_TO_INT( ELMF_LIST( hdList, 2 ) ) - low;

        /* test all entries against the first one                          */
        for ( i = 3;  i <= len;  i++ ) {
            if ( ELMF_LIST(hdList,i) != INT_TO_HD( low + (i-1) * inc ) )
                break;
        }

        /* if <hdList> is a range, convert to the compact representation   */
        isRange = (len < i);
        if ( isRange && 2 < len ) {
            Retype( hdList, T_RANGE );
            Resize( hdList, SIZE_PLEN_RANGE( len ) );
            SET_LEN_RANGE( hdList, len );
            SET_LOW_RANGE( hdList, low );
            SET_INC_RANGE( hdList, inc );
        }

    }

    /* return the result of the test                                       */
    return isRange;
}


/****************************************************************************
**
*F  FunIsRange(<hdCall>)  . . . . . . . . . . . . . . . . .  test for a range
**
**  'FunIsRange' implements the internal function 'IsRange'.
**
**  'IsRange( <obj> )'
**
**  'IsRange' returns 'true' if <obj>, which may be an object of any type, is
**  a range and 'false' otherwise.  A range is a list without holes such that
**  the elements are  consecutive integers.  Will cause an  error if <obj> is
**  an unassigned variable.
*/
TypHandle       FunIsRange ( hdCall )
    TypHandle           hdCall;
{
    TypHandle           hdObj;          /* handle of the argument          */

    /* get and check the argument                                          */
    if ( SIZE(hdCall) != 2 * SIZE_HD )
        return Error("usage: IsRange( <obj> )",0L,0L);
    hdObj = EVAL( PTR(hdCall)[1] );
    if ( hdObj == HdVoid )
        return Error("IsRange: function must return a value",0L,0L);

    /* let 'IsRange' do the work for lists                                 */
    return IsRange(hdObj) ? HdTrue : HdFalse;
}


/****************************************************************************
**
*F  InitRange() . . . . . . . . . . . . . . . .  initialize the range package
**
**  'InitRange' initializes the range package.
*/
void            InitRange ()
{

    /* install the list functions in the tables                            */
    TabIsList[T_RANGE]      = 1;
    TabLenList[T_RANGE]     = LenRange;
    TabElmList[T_RANGE]     = ElmRange;
    TabElmfList[T_RANGE]    = ElmfRange;
    TabElmlList[T_RANGE]    = ElmfRange;
    TabElmrList[T_RANGE]    = ElmfRange;
    TabElmsList[T_RANGE]    = ElmsRange;
    TabAssList[T_RANGE]     = AssRange;
    TabAsssList[T_RANGE]    = AsssRange;
    TabPosList[T_RANGE]     = PosRange;
    TabPlainList[T_RANGE]   = PlainRange;
    TabIsDenseList[T_RANGE] = IsDenseRange;
    TabIsPossList[T_RANGE]  = IsPossRange;
    EvTab[T_RANGE]          = EvList;
    PrTab[T_RANGE]          = PrRange;
    TabLt[T_RANGE][T_RANGE] = LtRange;

    /* install the functions to make a range                               */
    EvTab[T_MAKERANGE]      = EvMakeRange;
    PrTab[T_MAKERANGE]      = PrMakeRange;

    /* install the internal function                                       */
    InstIntFunc( "IsRange", FunIsRange );

}


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



