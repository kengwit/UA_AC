/****************************************************************************
**
*A  integer.h                   GAP source                   Martin Schoenert
**                                                           & Alice Niemeyer
**                                                           & Werner  Nickel
**
*A  @(#)$Id: integer.h,v 3.2 1992/02/29 14:12:55 fceller Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
**  This file  declares  the  functions  handling  arbitrary  size  integers.
**
**  There are three integer types in GAP: 'T_INT', 'T_INTPOS' and 'T_INTNEG'.
**  Each integer has a unique representation, e.g., an integer  that  can  be
**  represented as 'T_INT' is never  represented as 'T_INTPOS' or 'T_INTNEG'.
**
**  'T_INT' is the type of those integers small enough to fit into  29  bits.
**  Therefor the value range of this small integers is: $-2^{28}...2^{28}-1$.
**  This range contains about 99\% of all integers that usually occur in GAP.
**  (I just made up this number, obviously it depends on the application  :-)
**  Only these small integers can be used as index expression into sequences.
**
**  Small integers are represented by an immediate integer handle, containing
**  the value instead of pointing  to  it,  which  has  the  following  form:
**
**      +-------+-------+-------+-------+- - - -+-------+-------+-------+
**      | guard | sign  | bit   | bit   |       | bit   | tag   | tag   |
**      | bit   | bit   | 27    | 26    |       | 0     | 0     | 1     |
**      +-------+-------+-------+-------+- - - -+-------+-------+-------+
**
**  Immediate integers handles carry the tag 'T_INT', i.e. the last bit is 1.
**  This distuingishes immediate integers from other handles which  point  to
**  structures aligned on 4 byte boundaries and therefor have last bit  zero.
**  (The second bit is reserved as tag to allow extensions of  this  scheme.)
**  Using immediates as pointers and dereferencing them gives address errors.
**
**  To aid overflow check the most significant two bits must always be equal,
**  that is to say that the sign bit of immediate integers has a  guard  bit.
**
**  The macros 'INT_TO_HD' and 'HD_TO_INT' should be used to convert  between
**  a small integer value and its representation as immediate integer handle.
**
**  'T_INTPOS' and 'T_INTPOS' are the types of positive  respective  negative
**  integer values  that  can  not  be  represented  by  immediate  integers.
**
**  This large integers values are represented in signed base 65536 notation.
**  That means that the bag of  a  large  integer  has  the  following  form:
**
**      +-------+-------+-------+-------+- - - -+-------+-------+-------+
**      | digit | digit | digit | digit |       | digit | digit | digit |
**      | 0     | 1     | 2     | 3     |       | <n>-2 | <n>-1 | <n>   |
**      +-------+-------+-------+-------+- - - -+-------+-------+-------+
**
**  The value of this  is:  $d0 + d1 65536 + d2 65536^2 + ... + d_n 65536^n$,
**  respectivly the negative of this if the type of this object is T_INTNEG'.
**
**  Each digit is  of  course  stored  as  a  16  bit  wide  unsigned  short.
**  Note that base 65536 allows us to multiply 2 digits and add a carry digit
**  without overflow in 32 bit long arithmetic, available on most processors.
**
**  The number of digits in every  large  integer  is  a  multiple  of  four.
**  Therefor the leading three digits of some values will actually  be  zero.
**  Note that the uniqueness of representation implies that not four or  more
**  leading digits may be zero, since |d0|d1|d2|d3| and |d0|d1|d2|d3|0|0|0|0|
**  have the same value only one, the first, can be a  legal  representation.
**
**  Because of this it is possible to do a  little  bit  of  loop  unrolling.
**  Thus instead of looping <n> times, handling one digit in each  iteration,
**  we can loop <n>/4 times, handling  four  digits  during  each  iteration.
**  This reduces the overhead of the loop by a factor of  approximatly  four.
**
**  Using base 65536 representation has advantages over  using  other  bases.
**  Integers in base 65536 representation can be packed  dense  and  therefor
**  use roughly 20\% less space than integers in base  10000  representation.
**  'SumInt' is 20\% and 'ProdInt' is 40\% faster for 65536 than  for  10000,
**  as their runtime is linear respectivly quadratic in the number of digits.
**  Dividing by 65536 and computing the remainder mod 65536 can be done  fast
**  by shifting 16 bit to  the  right  and  by  taking  the  lower  16  bits.
**  Larger bases are difficult because the product of two digits will not fit
**  into 32 bit, which is the word size  of  most  modern  micro  processors.
**  Base 10000 would have the advantage that printing is  very  much  easier,
**  but 'PrInt' keeps a terminal at 9600 baud busy for almost  all  integers.
**
*H  $Log: integer.h,v $
*H  Revision 3.2  1992/02/29  14:12:55  fceller
*H  'TypDigit' is now defined in "integer.h".
*H
*H  Revision 3.1  1991/04/30  16:12:26  martin
*H  initial revision under RCS
*H
*H  Revision 3.0  1990/12/07  12:00:00  martin
*H  changed shifts to please TurboC
*H
*/


/****************************************************************************
**
*F  INT_TO_HD( <INT> )  . . .  convert a small integer to an immediate handle
**
**  'INT_TO_HD' converts the integer <INT> which should be  small  enough  to
**  fit into 29  bits,  into  the  corresponding  immediate  integer  handle.
**
**  Applying this to an  integer  outside  $-2^{28}...2^{28}-1$  gives random
**  results.
**
**  'INT_TO_HD' is defined in the declaration file of the package as follows:
*/
#define INT_TO_HD(INT)  ((TypHandle) (((long)(INT) << 2) + T_INT))


/****************************************************************************
**
*F  HD_TO_INT( <HD> ) . . . .  convert an immediate handle to a small integer
**
**  'HD_TO_INT' converts the handle <HD> which should be an immediate integer
**  handle into the value of the integer constant represented by this handle.
**
**  Applying this to a non immediate integer  handle  gives  random  results.
**
**  'HD_TO_INT' is defined in the declaration file of the package as follows:
*/
#define HD_TO_INT(HD)   (((long)HD) >> 2)


/****************************************************************************
**
*T  TypDigit  . . . . . . . . . . . . . . . . . . . .  type of a single digit
**
**  'TypDigit' is the type of a single digit of an  arbitrary  size  integer.
**  This is of course unsigned short int, which gives us the 16 bits we want.
*/
typedef unsigned short  TypDigit;


/****************************************************************************
**
*F  EvInt( <hdInt> )  . . . . . . . . . . . . .  evaluate an integer constant
**
**  'EvInt' returns  the value  of the  integer  <hdInt>.  Since integers are
**  constants and thus  selfevaluating this simply  returns <hdInt>.  This is
**  the evaluation function for the types 'T_INT', 'T_INTPOS', 'T_INTNEG'.
*/
TypHandle       EvInt P(( TypHandle hdInt ));


/****************************************************************************
**
*F  SumInt( <intL>, <intR> )  . . . . . . . . . . . . . . sum of two integers
**
**  'SumInt' returns the sum of the two integer arguments <intL> and  <intR>.
**  'SumInt' handles operands of type 'T_INT', 'T_INTPOS' and 'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**  This case is usually already handled in 'EvSum' for a better  efficiency.
**
**  Is called from the 'EvSum'  binop so both operands are already evaluated.
*/
TypHandle       SumInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  DiffInt( <intL>, <intR> ) . . . . . . . . . .  difference of two integers
**
**  'DiffInt' returns the difference of the two integer arguments <intL>  and
**  <intR>.  'DiffInt' handles  operands  of  type  'T_INT',  'T_INTPOS'  and
**  'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**  This case is usually already handled in 'EvDiff' for a better efficiency.
**
**  Is called from the 'EvDiff' binop so both operands are already evaluated.
*/
TypHandle       DiffInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  ProdInt( <intL>, <intR> ) . . . . . . . . . . . . product of two integers
**
**  'ProdInt' returns the product of the two  integer  arguments  <intL>  and
**  <intR>.  'ProdInt' handles  operands  of  type  'T_INT',  'T_INTPOS'  and
**  'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**  This case is usually already handled in 'EvProd' for a better efficiency.
**
**  Is called from the 'EvProd' binop so both operands are already evaluated.
*/
TypHandle       ProdInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  ModInt( <intL>, <intR> )  . . representant of residue class of an integer
**
**  'ModInt' returns the smallest positive representant of the residue  class
**  of the  integer  <intL>  modulo  the  integer  <intR>.  'ModInt'  handles
**  operands of type 'T_INT', 'T_INTPOS', 'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**  This case is usually already handled in 'EvMod' for a better efficiency.
**
**  Is called from the 'EvMod'  binop so both operands are already evaluated.
*/
TypHandle       ModInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  PowInt( <intL>, <intR> )  . . . . . . . . . . . . . . power of an integer
**
**  'PowInt' returns the <intR>-th (an integer) power of the integer  <intL>.
**  'PowInt' is handles operands of type 'T_INT', 'T_INTPOS' and 'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**  This case is usually already handled in 'EvPow' for a better  efficiency.
**
**  Is called from the 'EvPow'  binop so both operands are already evaluated.
*/
TypHandle       PowInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  EqInt( <intL>, <intR> ) . . . . . . . . .  test if two integers are equal
**
**  'EqInt' returns 'HdTrue' if the two integer arguments <intL>  and  <intR>
**  are equal and 'HdFalse' otherwise.
**
**  Is called from the  'EvEq'  binop so both operands are already evaluated.
*/
TypHandle       EqInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  LtInt( <intL>, <intR> ) . . . . . test if an integer is less than another
**
**  'LtInt' return 'HdTrue' if the integer <intL> is strictly less  than  the
**  integer <intR> and 'HdFalse' otherwise.
**
**  Is called from the 'EvLt'   binop so both operands are already evaluated.
*/
TypHandle       LtInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  PrInteger( <hdInt> )  . . . . . . . . . . . . . print an integer constant
**
**  'PrInteger' prints the integer <hdInt> in  the  usual  decimal  notation.
**  'PrInteger' handles objects of type 'T_INT', 'T_INTPOS' and 'T_INTNEG'.
**
**  The name is choosen to avoid possible conflicts with  the  name  'Print'.
*/
void            PrInteger P(( TypHandle hdInt ));


/****************************************************************************
**
*F  FunIsInt( <hdCall> )  . . . . . . . . . . . . . internal function 'IsInt'
**
**  'FunIsInt' implements the internal function 'IsInt'.
**
**  'IsInt( <obj> )'
**
**  'IsInt'  returns 'true' if the object <obj> is  an  integer  and  'false'
**  otherwise.  May cause an error if <obj> is an unbound variable.
*/
TypHandle       FunIsInt P(( TypHandle hdCall ));


/****************************************************************************
**
*F  QuoInt( <intL>, <intR> )  . . . . . . . . . . . quotient of two integers
**
**  'QuoInt' returns the integer part of the two integers <intL> and  <intR>.
**  'QuoInt' handles operands of type  'T_INT',  'T_INTPOS'  and  'T_INTNEG'.
**
**  It can also be used in the cases that both operands  are  small  integers
**  and the result is a small integer too,  i.e., that  no  overflow  occurs.
**
**  Note that this routine is not called from 'EvQuo', the  division  of  two
**  integers yields  a  rational  and  is  therefor  performed  in  'QuoRat'.
**  This operation is however available through the internal function 'Quo'.
*/
TypHandle       QuoInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  FunQuoInt( <hdCall> ) . . . . . . . . . . . .  internal function 'QuoInt'
**
**  'FunQuo' implements the internal function 'QuoInt'.
**
**  'QuoInt( <i>, <k> )'
**
**  'Quo' returns the  integer part of the quotient  of its integer operands.
**  If <i>  and <k> are  positive 'Quo( <i>,  <k> )' is  the largest positive
**  integer <q>  such that '<q> * <k>  \<= <i>'.  If  <i> or  <k> or both are
**  negative we define 'Abs( Quo(<i>,<k>) ) = Quo( Abs(<i>), Abs(<k>) )'  and
**  'Sign( Quo(<i>,<k>) ) = Sign(<i>) * Sign(<k>)'.  Dividing by 0  causes an
**  error.  'Rem' (see "Rem") can be used to compute the remainder.
*/
TypHandle       FunQuo P(( TypHandle hdCall ));


/****************************************************************************
**
*F  RemInt( <intL>, <intR> )  . . . . . . . . . . . remainder of two integers
**
**  'RemInt' returns the remainder of the quotient  of  the  integers  <intL>
**  and <intR>.  'RemInt' handles operands of type  'T_INT',  'T_INTPOS'  and
**  'T_INTNEG'.
**
**  Note that the remainder is different from the value returned by the 'mod'
**  operator which is always positive.
**
**  'RemInt' is called from 'FunRemInt'.
*/
TypHandle       RemInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  FunRemInt( <hdCall> ) . . . . . . . . . . . .  internal function 'RemInt'
**
**  'FunRem' implements the internal function 'RemInt'.
**
**  'RemInt( <i>, <k> )'
**
**  'Rem' returns the remainder of its two integer operands,  i.e., if <k> is
**  not equal to zero 'Rem( <i>, <k> ) = <i> - <k> *  Quo( <i>, <k> )'.  Note
**  that the rules given  for 'Quo' (see "Quo") imply  that 'Rem( <i>, <k> )'
**  has the same sign as <i> and its absolute value is strictly less than the
**  absolute value of <k>.  Dividing by 0 causes an error.
*/
TypHandle       FunRem P(( TypHandle hdCall ));


/****************************************************************************
**
*F  GcdInt( <hdL>, <hdR> )  . . . . . . . . . . . . . . . gcd of two integers
**
**  'GcdInt' returns the gcd of the two integers <hdL> and <hdR>.
**
**  It is called from 'FunGcdInt' and the rational package.
*/
TypHandle       GcdInt P(( TypHandle hdL, TypHandle hdR ));


/****************************************************************************
**
*F  FunGcdInt( <hdCall> ) . . . . . . . . . . . .  internal function 'GcdInt'
**
**  'FunGcd' implements the internal function 'GcdInt'.
**
**  'GcdInt( <i>, <k> )'
**
**  'Gcd'  returns the greatest common divisor   of the two  integers <m> and
**  <n>, i.e.,  the  greatest integer that  divides  both <m>  and  <n>.  The
**  greatest common divisor is never negative, even if the arguments are.  We
**  define $gcd( m, 0 ) = gcd( 0, m ) = abs( m )$ and $gcd( 0, 0 ) = 0$.
*/
TypHandle       FunGcdInt P(( TypHandle hdCall ));


/****************************************************************************
**
*F  InitInt() . . . . . . . . . . . . . . . . initializes the integer package
**
**  'InitInt' initializes the arbitrary size integer package.
*/
void            InitInt P(( void ));



