/****************************************************************************
**
*A  aggroup.h                   GAP source                    Thomas Bischops
*A                                                             & Frank Celler
**
*A  @(#)$Id: aggroup.h,v 3.7 1992/04/07 20:24:44 martin Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
*H  $Log: aggroup.h,v $
*H  Revision 3.7  1992/04/07  20:24:44  martin
*H  changed the author line
*H
*H  Revision 3.6  1992/02/07  13:17:37  fceller
*H  Initial GAP 3.1 release.
*H
*H  Revision 3.1  1990/07/28  12:00:00  fceller
*H  Initial release under RCS.
*/

#ifdef SPEC_CPU2000_P64
#define long __int64
#endif /* SPEC_CPU2000_P64 */

/****************************************************************************
**
*F  EqAg( <hdL>, <hdR> )  . . . . . . . . . .  tests if two agwords are equal
*/
extern TypHandle    EqAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  LtAg( <hdL>, <hdR> )  . . . . . . . . . . . . . .  tests if <hdL> < <hdR>
*/
extern TypHandle    LtAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  EvAg( <hdAgWord> )  . . . . .  evaluates a normed word in a soluble group
*/
extern TypHandle    EvAg P(( TypHandle ));


/****************************************************************************
**
*F  ProdAg( <hdL>, <hdR> )  . . . . . . . . . . . . . evaluates <hdL> * <hdR>
*/
extern TypHandle    ProdAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  PowAgI( <hdL>, <hdR> )  . . . . . . . . . . . . . evaluates <hdL> ^ <hdR>
*/
extern TypHandle    PowAgI P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  QuoAg( <hdL>, <hdR> ) . . . . . . . . . . . . . . evaluates <hdL> / <hdR>
*/
extern TypHandle    QuoAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  ModAg( <hdL>, <hdR> ) . . . . . . . . . . . . . evaluates <hdL> mod <hdR>
*/
extern TypHandle    ModAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  PowAgAg( <hdL>, <hdR> ) . . . . . . . . . . . . . evaluates <hdL> ^ <hdR>
*/
extern TypHandle    PowAgAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  CommAg( <hdL>, <hdR> )  . . . . . evaluates the commutator of two agwords
*/
extern TypHandle    CommAg P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  FactorAgGroup( <hdG>, <n> ) . . . . . .  factor group of the group of <g>
*F  FunFactorAgGroup( <hdCall> )  . . . . . . . . .  internal 'FactorAgGroup'
*/
extern TypHandle    FactorAgGroup P(( TypHandle, long ));
extern TypHandle    FunFactorAgGroup P(( TypHandle ));


/****************************************************************************
**
*V  HdRnDepthAgWord . . . . . . . . . . . . . . 'DepthAgWord' record name bag
*F  FunDepthAgWord( <hdCall> )  . . . . . . . . . . .  internal 'DepthAgWord'
*/
extern TypHandle    FunDepthAgWord P(( TypHandle ));


/****************************************************************************
**
*V  HdRnCentralWeightAgWord . . . . . . 'CentralWeightAgWord' record name bag
*F  FunCentralWeightAgWord( <hdCall>  ) . . .  internal 'CentralWeightAgWord'
*/
extern TypHandle       FunCentralWeightAgWord P(( TypHandle ));


/****************************************************************************
**
*V  HdRnLeadingExponentAgWord . . . . 'LeadingExponentAgWord' record name bag
*F  FunLeadingExponentAgWord( <hdCall> )  .  internal 'LeadingExponentAgWord'
*/
extern TypHandle       FunLeadingExp P(( TypHandle ));


/****************************************************************************
**
*F  FunIsAgWord( <hdCall> ) . . . . . . . . . .  internal function 'IsAgWord'
*/
extern TypHandle    FunIsAgWord P(( TypHandle ));


/****************************************************************************
**
*V  HdRnSumAgWord . . . . . . . . . . . handle of 'SumAgWord' record name bag
*F  SumAgWord( <P>, <v>, <w> )  . . . . . . . . . . sum of <v> and <w> in <P>
*F  FunSumAgWord( <hdCall> )  . . . . . . . . . . . . .  internal 'SumAgWord'
*/
extern TypHandle    HdRnSumAgWord;
extern TypHandle    SumAgWord P(( TypHandle, TypHandle, TypHandle ));
extern TypHandle    FunSumAgWord P(( TypHandle ));
    

/****************************************************************************
**
*V  HdRnDifferenceAgWord  . . . . . . . .  'DifferenceAgWord' record name bag
*F  DifferenceAgWord( <P>, <v>, <w> ) . . .  difference of <v> and <w> in <P>
*F  FunDifferenceAgWord( <hdCall> ) . . . . . . . internal 'DifferenceAgWord'
*/
extern TypHandle    HdRnDifferenceAgWord;
extern TypHandle    DifferenceAgWord P(( TypHandle, TypHandle, TypHandle ));
extern TypHandle    FunDifferenceAgWord P(( TypHandle ));
    

/****************************************************************************
**
*V  HdRnExponentsAgWord . . . . . . . . . . 'ExponentsAgWord' record name bag
*F  FFExponentsAgWord( <g>, <s>, <e>, <z> ) . . . . conversion into ff-vector
*F  IntExponentsAgWord( <g>, <s>, <e> ) . . . . .  conversion into int-vector
*F  FunExponentsAgWord( <hdCall> )  . . . . . . .  internal 'ExponentsAgWord'
*/
extern TypHandle    HdRnExponentsAgWord;
extern TypHandle    FFExponentsAgWord  P((TypHandle, long, long, TypHandle));
extern TypHandle    IntExponentsAgWord P((TypHandle, long, long));
extern TypHandle    FunExponentsAgWord P((TypHandle));


/****************************************************************************
**
*V  HdIdAgWord  . . . . . . . . . . . . . . . . . . . . . .  general identity
*F  InitAg()  . . . . . . . . . . . . . . . initializes the collection module
*/
extern TypHandle    HdIdAgWord;
extern void         InitAg P(( void ));
