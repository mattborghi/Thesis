module rm
	contains
! =====================================================================
      SUBROUTINE RANMAR(RVEC,LENV,INI)                                      
! =====================================================================
! Universal random number generator proposed by Marsaglia and Zaman     
! in report FSU-SCRI-87-50                                              
!        modified by F. James, 1988 and 1989, to generate a vector      
!        of pseudorandom numbers RVEC of length LENV, and to put in     
!        the COMMON block everything needed to specify currrent state,  
!        and to add input and output entry points RMARIN, RMARUT.       
!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
!!!  Calling sequences for RANMAR:                                  ++ 
!!!      CALL RANMAR (RVEC, LEN)   returns a vector RVEC of LEN     ++ 
!!!                   32-bit random floating point numbers between  ++ 
!!!                   zero and one.                                 ++ 
!!!      CALL RMARIN(I1,N1,N2)   initializes the generator from one ++ 
!!!                   32-bit integer I1, and number counts N1,N2    ++ 
!!!                  (for initializing, set N1=N2=0, but to restart ++ 
!!!                    a previously generated sequence, use values  ++ 
!!!                    output by RMARUT)                            ++ 
!!!      CALL RMARUT(I1,N1,N2)   outputs the value of the original  ++ 
!!!                  seed and the two number counts, to be used     ++ 
!!!                  for restarting by initializing to I1 and       ++ 
!!!                  skipping N1*100000000+N2 numbers.              ++ 
!!! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
      DIMENSION RVEC(*)                                                 
      COMMON/RASET1/U(97),C,I97,J97                                     
      PARAMETER (MODCNS=1000000000)                                     
      SAVE CD, CM, TWOM24, NTOT, NTOT2, IJKL                            
      DATA NTOT,NTOT2,IJKL/-1,0,0/                                      
      integer,dimension(8)::values
      integer,intent(in)::INI

      IF (NTOT .GE. 0)  GO TO 50                                        
                                                                       
!        Default initialization. User has called RANMAR without RMARIN. 
      !call date_and_time(VALUES=values)
!http://gcc.gnu.org/onlinedocs/gcc-4.0.4/gfortran/DATE_005fAND_005fTIME.html      
      IJKL = INI
      !IJKL = values(8)
      NTOT = 0                                                          
      NTOT2 = 0                                                         
      KALLED = 0                                                        
      GO TO 1                                                           
                                                                       
      ENTRY      RMARIN(IJKLIN, NTOTIN,NTOT2N)                          
!         Initializing routine for RANMAR, may be called before         
!         generating pseudorandom numbers with RANMAR. The input        
!         values should be in the ranges:  0<=IJKLIN<=900 OOO OOO       
!                                          0<=NTOTIN<=999 999 999       
!                                          0<=NTOT2N<<999 999 999!      
! To get the standard values in Marsaglia's paper, IJKLIN=54217137      
!                                            NTOTIN,NTOT2N=0            
      IJKL = IJKLIN                                                     
      NTOT = MAX(NTOTIN,0)                                              
      NTOT2= MAX(NTOT2N,0)                                              
      KALLED = 1                                                        
!          always come here to initialize                               
    1 CONTINUE                                                          
      IJ = IJKL/30082                                                   
      KL = IJKL - 30082*IJ                                              
      I = MOD(IJ/177, 177) + 2                                          
      J = MOD(IJ, 177)     + 2                                          
      K = MOD(KL/169, 178) + 1                                          
      L = MOD(KL, 169)                                                  
      WRITE(6,'(A,I10,2X,2I10)') ' SEMILLA RANMAR:',IJKL,NTOT,NTOT2 
!      PRINT '(A,4I10)', '   I,J,K,L= ',I,J,K,L                       
      DO 2 II= 1, 97                                                    
      S = 0.                                                            
      T = .5                                                            
      DO 3 JJ= 1, 24                                                    
         M = MOD(MOD(I*J,179)*K, 179)                                   
         I = J                                                          
         J = K                                                          
         K = M                                                          
         L = MOD(53*L+1, 169)                                           
         IF (MOD(L*M,64) .GE. 32)  S = S+T                              
    3    T = 0.5*T                                                      
    2 U(II) = S                                                         
      TWOM24 = 1.0                                                      
      DO 4 I24= 1, 24                                                   
    4 TWOM24 = 0.5*TWOM24                                               
      C  =   362436.*TWOM24                                             
      CD =  7654321.*TWOM24                                             
      CM = 16777213.*TWOM24                                             
      I97 = 97                                                          
      J97 = 33                                                          
!       Complete initialization by skipping                             
!            (NTOT2*MODCNS + NTOT) random numbers                       
      DO 45 LOOP2= 1, NTOT2+1                                           
      NOW = MODCNS                                                      
      IF (LOOP2 .EQ. NTOT2+1)  NOW=NTOT                                 
      IF (NOW .GT. 0)  THEN                                             
        WRITE(6,'(A,I15)') ' RMARIN SKIPPING OVER ',NOW                 
       DO 40 IDUM = 1, NTOT                                             
       UNI = U(I97)-U(J97)                                              
       IF (UNI .LT. 0.)  UNI=UNI+1.                                     
       U(I97) = UNI                                                     
       I97 = I97-1                                                      
       IF (I97 .EQ. 0)  I97=97                                          
       J97 = J97-1                                                      
       IF (J97 .EQ. 0)  J97=97                                          
       C = C - CD                                                       
       IF (C .LT. 0.)  C=C+CM                                           
   40  CONTINUE                                                         
      ENDIF                                                             
   45 CONTINUE                                                          
      IF (KALLED .EQ. 1)  RETURN                                        
!                                                                       
!          Normal entry to generate LENV random numbers                 
   50 CONTINUE                                                          
      DO 100 IVEC= 1, LENV                                              
      UNI = U(I97)-U(J97)                                               
      IF (UNI .LT. 0.)  UNI=UNI+1.                                      
      U(I97) = UNI                                                      
      I97 = I97-1                                                       
      IF (I97 .EQ. 0)  I97=97                                           
      J97 = J97-1                                                       
      IF (J97 .EQ. 0)  J97=97                                           
      C = C - CD                                                        
      IF (C .LT. 0.)  C=C+CM                                            
      UNI = UNI-C                                                       
      IF (UNI .LT. 0.) UNI=UNI+1.                                       
      RVEC(IVEC) = UNI                                                  
!             Replace exact zeros by uniform distr. *2**-24             
         IF (UNI .EQ. 0.)  THEN                                         
         ZUNI = TWOM24*U(2)                                             
!             An exact zero here is very unlikely, but let's be safe.   
         IF (ZUNI .EQ. 0.) ZUNI= TWOM24*TWOM24                          
         RVEC(IVEC) = ZUNI                                              
         ENDIF                                                          
  100 CONTINUE                                                          
      NTOT = NTOT + LENV                                                
         IF (NTOT .GE. MODCNS)  THEN                                    
         NTOT2 = NTOT2 + 1                                              
         NTOT = NTOT - MODCNS                                           
         ENDIF                                                          
      RETURN                                                            
!           Entry to output current status                              
      ENTRY RMARUT(IJKLUT,NTOTUT,NTOT2T)                                
      IJKLUT = IJKL                                                     
      NTOTUT = NTOT                                                     
      NTOT2T = NTOT2                                                    
      RETURN                                                            
      END  subroutine                                                             

end module rm