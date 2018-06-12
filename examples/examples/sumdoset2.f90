! Extended version of sumdoset.f90.  This version includes evaluation of second derivatives, 
! and can be used for any method, including LAPLACE.
! sumdoset.f90 is from Appendix 5 of 
! Jun Shen, Alison Boeckmann & Andrew Vick
! Journal of Pharmacokinetics and Pharmacodynamics
! ISSN 1567-567X Volume 39 Number 3
! J Pharmacokinet Pharmacodyn (2012) 39:251-262 DOI 10.1007/s10928-012-9247-3
! Computes Mathematical absorption function for "Transit" compartment
! AJB 1/2011
! For Jun Shen
! Computes partial derivatives of funca w.r.t. X(2), X(4), X(5) (t, nn, ktr)
! Does not compute derivatives of funca w.r.t. X(3) (amt) because BIO is used
! outside this function and F1=0, so amt cannot have a partial derivative.
! The partial derivatives are needed so that DES can compute d(DADT(i))/d(eta).
! Algorithm: DES knows how FUNCA contributes to DADT(i). 
! DES needs the partials of FUNCA w.r.t. other variables defined in DES.
! The X(i) (which are elements of VECTR) are defined in DES.
! By the chain rule:
!   dDADT/deta=dDADT/dFUNCA * dFUNCA/deta + ... etc. (for each variable or FUNC used to compute DADT)
!   dFUNCA/deta=dFUNCA/dx(i) * dx(i)/deta + ... (for each element of X)
!  (See PREDPP Guide - Section VI. Other User Subroutines - C. DES)
!
      FUNCTION FUNCA(X,X1,X2)
      USE SIZES, ONLY: DPSIZE,ISIZE
      USE NMPRD_INT, ONLY: MSEC=>ISECDER
      REAL(KIND=DPSIZE) :: EVTREC
      INTEGER(KIND=ISIZE) :: FIRSTEM
      REAL(KIND=DPSIZE):: X,X1,X2,FUNCA
      DIMENSION X(9),X1(9),X2(9,9)
! 9 is a fixed dimension in NMTRAN and PREDPP. It may not be changed.
! Input: X
!   X(1)=Flag (0 - intialize; 1 - dose from PK ; 2 - call from DES)
!   X(2)=DOSE TIME (from PK) or T (from DES)
!   X(3)=DOSE AMOUNT
!   x(4)=NN
!   x(5)=KTR

! Output: X1(9), X1(i) = first partial derivative of FUNCA w.r.t. X(i)
! Does not compute derivatives of funca w.r.t. X(3) (amt) because BIO
! is used outside this function and F1=0, so amt cannot have a partial
! derivative.

! Output: X2(9,9),X2(i,j) = 2nd. partial derivative of FUNCA 
! w.r.t. X(i), X(j)
      integer ndos,i,maxndos
      PARAMETER (MAXNDOS=10000)
      real(kind=dpsize) :: amt(maxndos),dosetime(maxndos)
      real(kind=dpsize) :: ipt,ktr,t,nn,deltatime
      real(kind=dpsize) :: logdt,ndt
      save
      funca=0
      x1=0
      x2=0
      SELECT CASE (int(X(1)))
       CASE (0)   ! From PK at ICALL=0 or ICALL=1
! Initialization
       ndos=0
       CASE (1)   ! From PK at ICALL=2 with a dose
! save dose information if amt > 0
         if (x(3) > 0) then
          ndos=ndos+1
         if (ndos > maxndos) then
! Because of a design error, no error return to DES is possible.
! The following is too drastic, but is more informative than an error caused by
! exceeding an array bound.
          WRITE (6,*) 'ERROR in FUNCA: No. of doses > maxndos:',maxndos
          STOP
         endif
!
         dosetime(ndos)=x(2)
         amt(ndos)=x(3)
         endif
       CASE (2)   ! From DES
! compute superimposed (total) input at time T
        if (ndos > 0) then
        t=X(2)
        nn=x(4)
        ktr=x(5)
        do i=1,ndos
        deltatime=t-dosetime(i)
        if (deltatime > 0) then
          IPT=amt(i)*deltatime**NN*EXP(-KTR*deltatime)
          funca=funca+ipt
! x1(2) = partial of FUNCA w.r.t. T
           x1(2)=x1(2)+ipt*(NN/deltatime-ktr)
! x1(4) = partial of FUNCA w.r.t. NN
           x1(4)=x1(4)+ipt*log(deltatime)
! x1(5) = partial of FUNCA w.r.t. KTR
           x1(5)=x1(5)-IPT*deltatime
! compute second derivatives if NONMEM asks for them (for the Laplacian method)
          if (msec == 1) then
          logdt=log(deltatime)
          ndt=nn/deltatime
! x2(2,2) = 2nd partial of FUNCA w.r.t. T
            x2(2,2)=x2(2,2)-ipt*(ndt/deltatime-(ndt-ktr)*(ndt-ktr))
! x2(2,4) = 2nd partial of FUNCA w.r.t.T, w.r.t. NN
            x2(2,4)=x2(2,4)+ipt*(1/deltatime + ndt*logdt - ktr*logdt)
! x2(2,5) = 2nd partial of FUNCA w.r.t. T, w.r.t. Ktr
            x2(2,5)=x2(2,5)+ipt*(ktr*deltatime-NN-1)
! x2(4,2) = 2nd partial of FUNCA w.r.t. NN, w.r.t. T
            x2(4,2)=x2(2,4)
! x2(4,4) = 2nd partial of FUNCA w.r.t. NN
            x2(4,4)=x2(4,4)+ipt*logdt*logdt
! x2(4,5) = 2nd partial of FUNCA w.r.t. NN, w.r.t. Ktr
            x2(4,5)=x2(4,5)-ipt*logdt*deltatime
! x2(5,2) = 2nd partial of FUNCA w.r.t.Ktr, w.r.t. T
            x2(5,2)=x2(2,5)
! x2(5,4) = 2nd partial of FUNCA w.r.t.Ktr, w.r.t. NN
            x2(5,4)=x2(4,5)
! x2(5,5) = 2nd partial of FUNCA w.r.t. Ktr
            x2(5,5)=x2(5,5)+ipt*deltatime*deltatime
        endif   ! msec
        endif   ! deltatime>0
        enddo
        endif
      END SELECT       
      return
      end
