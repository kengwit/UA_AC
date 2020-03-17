/****************************************************************************
**
*A  polynom.h                    GAP source                      Frank Celler
**
*A  @(#)$Id: polynom.h,v 3.5 1994/02/07 14:11:32 fceller Rel $
**
*Y  Copyright 1990-1992,  Lehrstuhl D fuer Mathematik,  RWTH Aachen,  Germany
**
*H  $Log: polynom.h,v $
*H  Revision 3.5  1994/02/07  14:11:32  fceller
*H  changed interface slightly,  the functions now accept list with
*H  mixed entries and they will not automatically shorten the result
*H
*H  Revision 3.4  1993/03/01  10:33:13  fceller
*H  adapted to new list concept
*H
*H  Revision 3.3  1992/11/25  11:29:30  fceller
*H  added prototypes
*H
*H  Revision 3.2  1992/05/25  08:56:08  fceller
*H  Initial GAP 3.2 release
*/
#ifdef SPEC_CPU2000_P64
#define long __int64
#endif /* SPEC_CPU2000_P64 */


/****************************************************************************
**
*F  UnifiedFieldVecFFE( <hdL>, <hdR> )	. . . unify fields of <hdL> and <hdR>
**
**  Convert two finite field vectors into finite field vectors over  the same
**  finite field.  Signal an error if this conversion fails.
*/
extern TypHandle UnifiedFieldVecFFE P((
    TypHandle, TypHandle ));


/****************************************************************************
**
*F  FunShiftedCoeffs( <hdCall> )  . . . . . internal function 'ShiftedCoeffs'
**
**  'FunShiftedCoeffs' implements 'ShiftedCoeffs( <l>, <n> )'
*/
extern TypHandle (*TabShiftedCoeffs[T_VAR]) P(( TypHandle, long ));
extern TypHandle FunShiftedCoeffs    P(( TypHandle ));
extern TypHandle CantShiftedCoeffs   P(( TypHandle, long ));
extern TypHandle ShiftedCoeffsListx  P(( TypHandle, long ));
extern TypHandle ShiftedCoeffsVecFFE P(( TypHandle, long ));


/****************************************************************************
**
*F  FunNormalizeCoeffs( <hdCall> )  . . . internal function 'NormalizeCoeffs'
**
**  'FunNormalizeCoeffs' implements 'NormalizeCoeffs( <c> )'
*/
extern TypHandle (*TabNormalizeCoeffs[T_VAR]) P(( TypHandle ));
extern TypHandle FunNormalizeCoeffs    P(( TypHandle ));
extern TypHandle CantNormalizeCoeffs   P(( TypHandle ));
extern TypHandle NormalizeCoeffsVecFFE P(( TypHandle ));
extern TypHandle NormalizeCoeffsListx  P(( TypHandle ));


/****************************************************************************
**
*F  FunShrinkCoeffs( <hdCall> )  . . . . . . internal function 'ShrinkCoeffs'
**
**  'FunShrinkCoeffs' implements 'ShrinkCoeffs( <c> )'
*/
extern void (*TabShrinkCoeffs[T_VAR]) P(( TypHandle ));

extern TypHandle FunShrinkCoeffs P(( TypHandle ));
extern void CantShrinkCoeffs   P(( TypHandle ));
extern void ShrinkCoeffsVecFFE P(( TypHandle ));
extern void ShrinkCoeffsListx  P(( TypHandle ));


/****************************************************************************
**
*F  ADD_COEFFS( <hdL>, <hdR>, <hdM> ) . . . . . <hdL>+<hdM>*<hdR> into <hdL>
*/
#define ADD_COEFFS( hdL, hdR, hdM ) \
    (TabAddCoeffs[XType(hdL)][XType(hdR)]( hdL, hdR, hdM ))

extern void (*TabAddCoeffs[T_VAR][T_VAR]) P((
    TypHandle, TypHandle, TypHandle ));

extern void CantAddCoeffs         P(( TypHandle, TypHandle, TypHandle ));
extern void AddCoeffsListxListx   P(( TypHandle, TypHandle, TypHandle ));
extern void AddCoeffsVecFFEVecFFE P(( TypHandle, TypHandle, TypHandle ));
extern void AddCoeffsListxVecFFE  P(( TypHandle, TypHandle, TypHandle ));


/****************************************************************************
**
*F  FunAddCoeffs( <hdCall> )  . . . . . . . . . internal function 'AddCoeffs'
**
**  'FunAddCoeffs' implements 'AddCoeffs( <l>, <r> )'
*/
extern TypHandle FunAddCoeffs P(( TypHandle ));


/****************************************************************************
**
*F  FunSumCoeffs( <hdCall> )  . . . . . . . . . internal function 'SumCoeffs'
**
**  'FunSumCoeffs' implements 'SumCoeffs( <l>, <r> )'
*/
extern TypHandle FunSumCoeffs P(( TypHandle ));


/****************************************************************************
**
*F  MULTIPLY_COEFFS( <hdP>, <hdL>, <l>, <hdR>, <r> )   <hdL>*<hdR> into <hdP>
*/
#define MULTIPLY_COEFFS(hdP,hdL,l,hdR,r) \
    (TabMultiplyCoeffs[XType(hdL)][XType(hdR)](hdP,hdL,l,hdR,r))

extern long (*TabMultiplyCoeffs[T_VAR][T_VAR]) P((
    TypHandle, TypHandle, long, TypHandle, long ));

extern long CantMultiplyCoeffs P((
    TypHandle, TypHandle, long, TypHandle, long ));

extern long MultiplyCoeffsListxListx P((
    TypHandle, TypHandle, long, TypHandle, long ));

extern long MultiplyCoeffsVecFFEVecFFE P((
    TypHandle, TypHandle, long, TypHandle, long ));


/****************************************************************************
**
*F  FunProductCoeffs( <hdCall> )  . . . . . internal function 'ProductCoeffs'
**
**  'FunProductCoeffs' implements 'ProductCoeffs( <l>, <r> )'
*/
extern TypHandle (*TabProductCoeffs[T_VAR][T_VAR]) P((
    TypHandle, TypHandle ));

extern TypHandle FunProductCoeffs P(( TypHandle ));
extern TypHandle CantProductCoeffs         P(( TypHandle, TypHandle ));
extern TypHandle ProductCoeffsListxListx   P(( TypHandle, TypHandle ));
extern TypHandle ProductCoeffsVecFFEVecFFE P(( TypHandle, TypHandle ));


/****************************************************************************
**
*F  FunProductCoeffsMod( <hdCall> ) . .  internal function 'ProductCoeffsMod'
**
**  'FunProductCoeffsMod' implements 'ProductCoeffsMod( <l>, <r>, <p> )'
*/
extern TypHandle (*TabProductCoeffsMod[T_VAR][T_VAR]) P((
    TypHandle, TypHandle, TypHandle ));

extern TypHandle FunProductCoeffsMod P(( TypHandle ));

extern TypHandle CantProductCoeffsMod P((
    TypHandle, TypHandle, TypHandle ));

extern TypHandle ProductCoeffsModListxListx P((
    TypHandle, TypHandle, TypHandle ));


/****************************************************************************
**
*F  REDUCE_COEFFS( <hdL>, <l>, <hdR>, <r> ) . . . . . . reduce <hdL> by <hdR>
*/
#define REDUCE_COEFFS( hdL, l, hdR, r ) \
    (TabReduceCoeffs[XType(hdL)][XType(hdR)]( hdL, l, hdR, r ))

extern long (*TabReduceCoeffs[T_VAR][T_VAR]) P((
    TypHandle, long, TypHandle, long ));

extern long CantReduceCoeffs P((
    TypHandle, long, TypHandle, long ));

extern long ReduceCoeffsListxListx P((
    TypHandle, long, TypHandle, long ));

extern long ReduceCoeffsVecFFEVecFFE P((
    TypHandle, long, TypHandle, long ));


/****************************************************************************
**
*F  FunReduceCoeffs( <hdCall> ) . . . . . .  internal function 'ReduceCoeffs'
**
**  'FunReduceCoeffs' implements 'ReduceCoeffs( <l>, <r> )'
*/
extern TypHandle FunReduceCoeffs P(( TypHandle ));


/****************************************************************************
**
*F  FunRemainderCoeffs( <hdCall> )  . . . internal function 'RemainderCoeffs'
**
**  'FunRemainderCoeffs' implements 'RemainderCoeffs( <l>, <r> )'
*/
extern TypHandle FunRemainderCoeffs P(( TypHandle ));


/****************************************************************************
**
*F  REDUCE_COEFFS_MOD( <hdL>, <l>, <hdR>, <r>, <hdN> )  reduce <hdL> by <hdR>
*/
#define REDUCE_COEFFS_MOD( hdL, l, hdR, r ) \
    (TabReduceCoeffsMod[XType(hdL)][XType(hdR)]( hdL, l, hdR, r, hdN ))

extern long (*TabReduceCoeffsMod[T_VAR][T_VAR]) P((
    TypHandle, long, TypHandle, long, TypHandle ));

extern long CantReduceCoeffsMod P((
    TypHandle, long, TypHandle, long, TypHandle ));

extern long ReduceCoeffsModListxListx P((
    TypHandle, long, TypHandle, long, TypHandle ));

extern long ReduceCoeffsModListx P((
    TypHandle, long, TypHandle, long, TypHandle ));


/****************************************************************************
**
*F  FunReduceCoeffsMod( <hdCall> )  . . . internal function 'ReduceCoeffsMod'
**
**  'FunReduceCoeffsMod' implements 'ReduceCoeffsMod( <l>, <r>, <p> )'
*/
extern TypHandle FunReduceCoeffsMod P(( TypHandle ));


/****************************************************************************
**
*F  FunPowerModCoeffs( <hdCall> ) . . . .  internal function 'PowerModCoeffs'
**
**  'FunPowerModCoeffs' implements 'PowerModCoeffs( <g>, <n>, <r> )'
*/
extern TypHandle (*TabPowerModCoeffsInt[T_VAR][T_VAR]) P((
    TypHandle, TypHandle, TypHandle ));

extern TypHandle (*TabPowerModCoeffsLInt[T_VAR][T_VAR]) P((
    TypHandle, TypHandle, TypHandle ));

extern TypHandle FunPowerModCoeffs P(( TypHandle ));
extern TypHandle PowerModListxIntListx    P((TypHandle,TypHandle,TypHandle));
extern TypHandle PowerModVecFFEIntVecFFE  P((TypHandle,TypHandle,TypHandle));
extern TypHandle PowerModListxLIntListx   P((TypHandle,TypHandle,TypHandle));
extern TypHandle PowerModVecFFELIntVecFFE P((TypHandle,TypHandle,TypHandle));
extern TypHandle CantPowerModCoeffs       P((TypHandle,TypHandle,TypHandle));


/****************************************************************************
**
*F  InitPolynom() . . . . . . . . . . . . . .  initialize the polynom package
*/
extern void InitPolynom P(( void ));
