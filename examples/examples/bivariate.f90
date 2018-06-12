! Integral of Bivariate normal distribution of two correlated data points, DV1, DV2, correlation RHO
! BV1, BV2 and GAUSS FROM Drezner and Wesolowsky, J. Statistical Computat. Simul. 35, pp. 101-107, 1990.
! BV2 is more accurate for extreme values of rho
! Partial derivatives worked out by Robert Bauer.
!
! How to use in NONMEM 7.4
! VBI is a suggested vector name.  Could be any name.  But the function name must match what is in bivariate.f90
! $ABBR FUNCTION BIVARIATE(VBI,5)
! $SUBROUTINES ... OTHER=bivariate.f90
! $PK
! ...
! DV1 AND DV2 SHOULD BE TWO SEPARATE DATA ITEMS ON EACH RECORD.
!   VBI(1)=RHO
!   VBI(2)=DV1 ! H
!   VBI(3)=DV2 !K 
!   VBI(4)=DTYPE !=0 FOR H TO INF, K TO INF, OR 1 FOR -INF TO H, -INF TO K
!   VBI(5)=BVTYPE !BVTYPE=0 USES SIMPLE INTEGRATOR, BVTYPE=1 USES MORE ACCURATE ONE FOR LARGE VALUES OF RHO
!   F_FLAG=1
! IT IS SAFER TO AVOID BV BEING EXACTLY 0, JUST MAKE SURE IT IS REALLY SMALL, TO AVOID LOG() ERRORS
!    BV=BIVARIATE(VBI)+1.0D-30
!    IF YOU USE -2LL FORMAT (DEFAULT):
!    Y=-2.0D+00*LOG(BV)

      FUNCTION BIVARIATE(X,X1,X2,NDIM)
! NDIM SHOULD EQUAL 5      
!      IMPLICIT REAL*8 (A-H,O-Z)
      IMPLICIT NONE
      INTEGER NDIM
      REAL*8 X(NDIM),X1(NDIM),X2(NDIM,NDIM)
      REAL*8 R,H,K,ITYPE,PI,PI2,SPI2,R1,RSQR,HRK,KRH,PHRK,PKRH,XH,XK,XHRK,XKRH,XC,BV,BV2,BIVARIATE,GAUSS,BV1
      EXTERNAL GAUSS,BV1,BV2
      INTEGER BVTYPE
! unload the arguments
      R=X(1)
      H=X(2)
      K=X(3)
      ITYPE=X(4)
      BVTYPE=X(5)
! ITYPE=0 INTGRATE H TO INF, K TO INF
! ITYPE=1 INTGRATE -INF TO H -INF TO K
! BVTYPE=0 USES THE SIMPLER BV1
! BVTYPE=1 USES THE MORE ACCURATE BV2
! SET UP SOME USEFUL CALCUATIONS FOR FIRST AND SECOND DERIVATIVES OF THE ARGUMENTS
! DERIVATIVES ARE NOT NEEDED IF YOU USE IMP, SAEM, BAYES, 
! OR IF YOU USE ITS OR LAPLACE WITH NUMERICAL 1ST AND 2ND DERIVATIVE EVALUATIONS WITH OPTMAP=1 ETADER=3 (NONMEM 7.3).
! DERIVATIVES ARE NEEDED IF YOU USE STANDARD LAPLACE OR STADNARD ITS
      PI=3.141592653589793238D+00
      PI2=2.0D+00*pi
      spi2=sqrt(pi2)
      R1=1.0D+00-R*R
      RSQR=SQRT(ABS(R1))
      IF(RSQR.LE.0.0D+00) RSQR=1.0D-100
      IF(R1.LE.0.0D+00) R1=1.0D-100
      HRK=(H-R*K)/RSQR
      KRH=(K-R*H)/RSQR
      PHRK=1.0D+00-GAUSS(HRK)
      PKRH=1.0D+00-GAUSS(KRH)
      XH=EXP(-H*H/2.0D+00)/SPI2
      XK=EXP(-K*K/2.0D+00)/SPI2
      XHRK=EXP(-HRK*HRK/2.0D+00)/SPI2/RSQR
      XKRH=EXP(-KRH*KRH/2.0D+00)/SPI2/RSQR    
      XC=EXP(-(H*H-2.0D+00*R*H*K+K*K)/2.0D+00/R1)/RSQR/PI2

! partial F WRT RHO, from Drezner and Wesolowsky, equation (4)
      X1(1)=XC
! parital F WRT H
      X1(2)=XH*PKRH+(ITYPE-1.0D+00)*XH
! parital F WRT K
      X1(3)=XK*PHRK+(ITYPE-1.0D+00)*XK
! 2ND parital F WRT RHO,RHO
      X2(1,1)=R/R1*XC+XC*(H*K*(1.0D+00-R)*(1.0D+00-R)-R*(H-K)*(H-K))/R1/R1
! 2ND parital F WRT RHO,H
      X2(1,2)=XC*(R*K-H)/R1     
! 2ND parital F WRT RHO,K
      X2(1,3)=XC*(R*H-K)/R1
      X2(2,1)=X2(1,2)
      X2(3,1)=X2(1,3)
! 2ND parital F WRT H,K
      X2(2,3)=XC
      X2(3,2)=X2(2,3)
! 2ND parital F WRT H,H
      X2(2,2)=-X1(2)*H-XC*R
! 2ND parital F WRT K,K
      X2(3,3)=-X1(3)*K-XC*R
!      write(50,*) 'A ',x1(1),x1(2),x1(3)
!      write(50,*) 'B ',x2(1,1),x2(1,2),x2(1,3),x2(2,1),x2(2,2),x2(2,3),x2(3,1),x2(3,2),x2(3,3)

! bV2 is more accurate for extreme rho values.
      IF(BVTYPE==0) THEN
      BV=BV1(H,K,R)
      ELSE
      BV=BV2(H,K,R)
      ENDIF

      IF(ITYPE==1.0D+00) BV=BV-GAUSS(H)-GAUSS(K)+1.0D+00
      BIVARIATE=BV

      RETURN
      END

      FUNCTION BV1(H1,H2,R)
      IMPLICIT NONE
      INTEGER I
!      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 H1,H2,R,H12,BV,BV1,RR,RR2,H3,GAUSS
      EXTERNAL GAUSS
      REAL*8 X(5),W(5),DVAL
      DATA X/.04691008,.23076534,.5,.76923466,.95308992/
      DATA W/.018854042,.038088059,.0452707394,.038088059,.018854042/
      H12 =(H1*H1+ H2*H2)/2.0D+00
      H3 = H1*H2
      BV = 0.0D+00
      DO 1 I= 1,5
      RR = R*X(I)
      RR2= 1.0D+00-RR*RR
      IF(RR2.LE.0.0D+00) RR2=1.0D-100
      DVAL=(RR*H3 - H12)/RR2
      IF(DVAL.GT.300.0D+00) DVAL=300.0D+00
      BV = BV + W(I)*EXP(DVAL)/SQRT(RR2)
    1 CONTINUE
      BV = BV*R + GAUSS(H1)*GAUSS(H2)
      BV1=BV
      RETURN
      END

      FUNCTION BV2(H1,HK,R)
      IMPLICIT NONE
!      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 X(5),W(5)
      REAL*8 H1,HK,R,H2,H12,BV,BV2,R2,R3,H3,H7,H6,H5,AA,AB,R1,RR,GAUSS,RR2
      REAL*8 DVAL,DVAL2
      EXTERNAL GAUSS
      INTEGER I
      DATA X/.04691008,.23076534,.5,.76923466,.95308992/
      DATA W/.018854042,.038088059,.0452707394,.038088059,.018854042/
      H2 = HK
      H12 = (H1*H1+ H2*H2)/2.0D+00
      BV = 0.0D+00
      IF(ABS(R).LT.0.7D+00)GO TO 4
      R2= 1.0D+00-R*R
      R3 = SQRT(R2)
      IF(R.LT.0.0D+00)H2 = -H2
      H3=H1*H2
      H7 = EXP( -H3/2.0D+00)
      IF(R2.EQ.0.0D+00) GO TO 3
      H6 = ABS(H1 - H2)
      H5 = H6*H6/2.0D+00
      H6 = H6/R3
      AA = 0.5D+00 - H3/8.0D+00
      AB = 3.0D+00 - 2.0D+00*AA*H5
      BV = .13298076D+00*H6*AB*GAUSS(H6)-EXP(-H5/R2)*(AB + AA*R2)*.053051647D+00
      DO 2 I= 1,5
      R1= R3*X(I)
      RR = R1*R1
      R2 = SQRT(DABS(1.0D+00 - RR))
      IF(R2.EQ.0.0D+00) R2=1.0D-100
      IF(RR.EQ.0.0D+00) RR=1.0D-100
      DVAL=-H5/RR
      IF(DVAL.GT.300.0D+00) DVAL=300.0D+00      
      DVAL2=-H3/(1.0D+00 + R2)
      IF(DVAL2.GT.300.0D+00) DVAL2=300.0D+00
      BV = BV - W(I)*EXP(DVAL)*(EXP(DVAL2)/R2/H7 - 1.0D+00 - AA*RR)
    2 CONTINUE
    3 IF(R.GT.0.0D+00)BV = BV*R3*H7 + GAUSS(MAX(H1,H2))
      IF(R.LT.0.0D+00)BV = MAX(0.0D+00,GAUSS(H1) - GAUSS(H2)) - BV*R3*H7
      BV2=BV
      RETURN
    4 H3=H1*H2
      DO 1 I= 1,5
      R1= R*X(I)
      RR2= 1.0D+00-R1*R1
      IF(RR2.EQ.0.0D+00) RR2=1.0D-100
    1 BV = BV + W(I)*EXP((R1*H3 -H12)/RR2)/SQRT(RR2)
      BV2 =GAUSS(H1)*GAUSS(H2) + R*BV
      RETURN
      END

      FUNCTION GAUSS(Z)
      IMPLICIT NONE
!      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 Z,GAUSS,X,G
      REAL*8 A(4)
      INTEGER I
      DATA A/ -.72657601,.71070688,-.142248368,.127414796/
      X= 1.0D+00/(1.0D+00 + .23164189D+00*ABS(Z))
      G = .53070271D+00
      DO 1 I= 1,4
    1 G=G*X+A(I)
      GAUSS = G*X*EXP( - Z*Z/2.0D+00)
      IF(Z.LT.0.0D+00)GAUSS = 1.0D+00 -GAUSS
      RETURN
      END

! USE PROGRAM HEADER FOR STAND-ALONE EXECUTABLE TESTING
!      PROGRAM BIVTEST
      SUBROUTINE BIVTEST
      IMPLICIT REAL*8 (A-H,O-Z)
      REAL*8 A(9),A1(9),A2(9,9)
      REAL*8 AH(9),AH1(9),AH2(9,9)
      REAL*8 AJ(9),AJ1(9),AJ2(9,9)
      REAL*8 B(9),B1(9),B2(9,9)
      REAL*8 H,K
      HDEL=1.0D-04
    1 CONTINUE
      WRITE(*,*) 'ENTER RHO,H,K,INTEGRAND-TYPE,BVTYPE'
      READ(*,*) RHO,H,K,DTYPE,BVTYPE
      A(1)=RHO
      A(2)=H
      A(3)=K
      A(4)=DTYPE
      A(5)=BVTYPE
      BV=BIVARIATE(A,A1,A2,9)
      WRITE(*,*) 'VALUE ',BV
      WRITE(*,*) GAUSS(-1.96D+00),GAUSS(1.96D+00)
      AH(1:5)=A(1:5)
      AH(1)=RHO-HDEL
      AJ(1:5)=A(1:5)
      AJ(1)=RHO+HDEL
      BVH=BIVARIATE(AH,AH1,AH2,9)
      BVJ=BIVARIATE(AJ,AJ1,AJ2,9)
      B1(1)=(BVJ-BVH)/2.0D+00/HDEL
      B2(1,1)=(AJ1(1)-AH1(1))/2.0D+00/HDEL
      B2(1,2)=(AJ1(2)-AH1(2))/2.0D+00/HDEL
      B2(1,3)=(AJ1(3)-AH1(3))/2.0D+00/HDEL
      WRITE(*,*)
      WRITE(*,*) 'ANALOG RHO',A1(1),A2(1,1),A2(1,2),A2(1,3)
      WRITE(*,*) 'NUMER RHO',B1(1),B2(1,1),B2(1,2),B2(1,3)

      AH(1:5)=A(1:5)
      AH(2)=H-HDEL
      AJ(1:5)=A(1:5)
      AJ(2)=H+HDEL
      BVH=BIVARIATE(AH,AH1,AH2,9)
      BVJ=BIVARIATE(AJ,AJ1,AJ2,9)
      B1(2)=(BVJ-BVH)/2.0D+00/HDEL
      B2(2,1)=(AJ1(1)-AH1(1))/2.0D+00/HDEL
      B2(2,2)=(AJ1(2)-AH1(2))/2.0D+00/HDEL
      B2(2,3)=(AJ1(3)-AH1(3))/2.0D+00/HDEL
      WRITE(*,*)
      WRITE(*,*) 'ANALOG H',A1(2),A2(2,1),A2(2,2),A2(2,3)
      WRITE(*,*) 'NUMER H',B1(2),B2(2,1),B2(2,2),B2(2,3)


      AH(1:5)=A(1:5)
      AH(3)=K-HDEL
      AJ(1:5)=A(1:5)
      AJ(3)=K+HDEL
      BVH=BIVARIATE(AH,AH1,AH2,9)
      BVJ=BIVARIATE(AJ,AJ1,AJ2,9)
      B1(3)=(BVJ-BVH)/2.0D+00/HDEL
      B2(3,1)=(AJ1(1)-AH1(1))/2.0D+00/HDEL
      B2(3,2)=(AJ1(2)-AH1(2))/2.0D+00/HDEL
      B2(3,3)=(AJ1(3)-AH1(3))/2.0D+00/HDEL
      WRITE(*,*)
      WRITE(*,*) 'ANALOG K',A1(3),A2(3,1),A2(3,2),A2(3,3)
      WRITE(*,*) 'NUMER K',B1(3),B2(3,1),B2(3,2),B2(3,3)

      GO TO 1
      END
