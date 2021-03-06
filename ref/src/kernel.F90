!----------------
! kernels
!----------------
MODULE my_kernels

  USE crtm_type
  USE crtm_utils

  IMPLICIT NONE

 CONTAINS

  !************************************************************
  !
  ! myMATMUL
  !
  !************************************************************
  SUBROUTINE  myMATMUL(A, B, C)

    REAL(fp), INTENT(IN), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: A, B
    REAL(fp), INTENT(OUT), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: C
    REAL(fp) :: acc
    INTEGER :: I, J, K

    DO I = 1, MAX_N_ANGLES
      DO J = 1, MAX_N_ANGLES
        acc = 0
        DO K = 1, MAX_N_ANGLES
            acc = acc + A(I,K) * B(K,J)
        END DO
        C(I, J) = acc
      END DO
    END DO

  END SUBROUTINE myMATMUL

  !************************************************************
  !
  ! myTRANSPOSE
  !
  !************************************************************
  SUBROUTINE myTRANSPOSE(A, At)

    REAL(fp), INTENT(IN), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: A
    REAL(fp), INTENT(OUT), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: At
    INTEGER :: I, J

    DO I = 1, MAX_N_ANGLES
      DO J = 1, MAX_N_ANGLES
         At(I,J) = A(J,I)
      END DO
    END DO

  END SUBROUTINE myTRANSPOSE

  !************************************************************
  !
  ! myADD
  !
  !************************************************************
  SUBROUTINE myADD(A, B, C)

    REAL(fp), INTENT(IN), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: A, B
    REAL(fp), INTENT(OUT), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES) :: C
    INTEGER :: I, J

    DO I = 1, MAX_N_ANGLES
      DO J = 1, MAX_N_ANGLES
         C(I,J) = A(I,J) + B(I,J)
      END DO
    END DO

  END SUBROUTINE myADD

  !************************************************************
  !
  ! RTV_Create
  !
  !************************************************************
  SUBROUTINE RTV_Create( &
    RTV, &
    n_Angles        , &
    n_Legendre_Terms, &
    n_Layers          )

    USE mt19937_64

    ! Arguments
    TYPE(RTV_type), INTENT(OUT) :: RTV
    INTEGER       , INTENT(IN)  :: n_Angles        
    INTEGER       , INTENT(IN)  :: n_Legendre_Terms
    INTEGER       , INTENT(IN)  :: n_Layers        
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Angles < 1 .OR. n_Legendre_Terms < 1 .OR. n_Layers < 1 ) RETURN
    
    ALLOCATE( RTV%Pff(n_Angles, n_Angles+1, n_Layers) , &
         RTV%Pbb(n_Angles, n_Angles+1, n_Layers) , &
         STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Perform the allocation for SOI variables
    ALLOCATE( RTV%Number_Doubling(n_Layers), &
              RTV%Delta_Tau(n_Layers), &      
              RTV%Refl(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%Trans(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%Inv_BeT(n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers), &
              RTV%C1(n_Angles, n_Layers), &
              RTV%C2(n_Angles, n_Layers), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Set dimensions
    RTV%n_Layers         = n_Layers
    RTV%n_Angles         = n_Angles
    
    RTV%n_SOI_Iterations = 0

    ! Set the allocate flag
    RTV%Is_Allocated = .TRUE.

    !------ Fill arrays with random values --- !
    RTV%Number_Doubling(:) = MAX_N_DOUBLING
    CALL mt19937_real3d(RTV%Pff)
    CALL mt19937_real3d(RTV%Pbb)
    CALL mt19937_real1d(RTV%Delta_Tau)
    CALL mt19937_real4d(RTV%Refl)
    CALL mt19937_real4d(RTV%Trans)
    CALL mt19937_real4d(RTV%Inv_BeT)
    CALL mt19937_real1d(RTV%COS_Angle)
    CALL mt19937_real1d(RTV%COS_Weight)
    CALL mt19937_real2d(RTV%C1)
    CALL mt19937_real2d(RTV%C2)

    RTV%Refl=RTV%Refl/100.0_fp
    RTV%Trans=RTV%Trans/100.0_fp
    RTV%Inv_Bet=RTV%Inv_Bet/100.0_fp


  END SUBROUTINE RTV_Create

  !************************************************************
  !
  ! CRTM_Doubling_layer_AD
  !
  !************************************************************
  SUBROUTINE CRTM_Doubling_layer_AD(n_streams, & ! Input, number of streams
                                         NANG, & ! Input, number of angles
                                           KL, & ! Input, number of angles
                                single_albedo, & ! Input, single scattering albedo
                                optical_depth, & ! Input, layer optical depth
                                    COS_Angle, & ! Input, COSINE of ANGLES
                                   COS_Weight, & ! Input, GAUSSIAN Weights
                                           ff, & ! Input, Phase matrix (forward part)
                                           bb, & ! Input, Phase matrix (backward part)
                                  Planck_Func, & ! Input, Planck for layer temperature
                                     trans_AD, & ! Input, layer tangent-linear trans 
                                      refl_AD, & ! Input, layer tangent-linear refl 
                                 source_up_AD, & ! Input, layer tangent-linear source_up 
                               source_down_AD, & ! Input, layer tangent-linear source_down 
                                          RTV, & ! Input, structure containing forward results 
                             single_albedo_AD, & ! Output adjoint single scattering albedo
                             optical_depth_AD, & ! Output AD layer optical depth
                                        ff_AD, & ! Output AD forward Phase matrix
                                        bb_AD, & ! Output AD backward Phase matrix
                               Planck_Func_AD, & ! Output AD Planck for layer temperature
                               streamid, &
                               term1,term2,term3,term4,term5_AD,trans1,trans3,trans4,temp1,temp2,temp3,C1_AD,C2_AD)   ! Temporaries

    INTEGER, INTENT(IN) :: n_streams,NANG,KL,streamid
    TYPE(RTV_type), INTENT(IN) :: RTV
    REAL(fp), INTENT(IN), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES+1) :: ff,bb
    REAL(fp), INTENT(IN), DIMENSION(1:MAX_N_ANGLES) :: COS_Angle, COS_Weight 
    REAL(fp), INTENT(IN) :: single_albedo,optical_depth,Planck_Func

    ! Tangent-Linear Part
    REAL(fp), INTENT( INOUT ), DIMENSION( 1:MAX_N_ANGLES,1:MAX_N_ANGLES ) :: trans_AD,refl_AD
    REAL(fp), INTENT( INOUT ), DIMENSION( 1:MAX_N_ANGLES ) :: source_up_AD,source_down_AD
    REAL(fp), INTENT( INOUT ) :: single_albedo_AD
    REAL(fp), INTENT( INOUT ) :: optical_depth_AD,Planck_Func_AD
    REAL(fp), INTENT(INOUT), DIMENSION(1:MAX_N_ANGLES,1:MAX_N_ANGLES+1) :: ff_AD,bb_AD

    ! internal variables
    REAL(fp), INTENT(OUT),DIMENSION(MAX_N_ANGLES,MAX_N_ANGLES) :: term1,term2,term3,term4,term5_AD
    REAL(fp), INTENT(OUT),DIMENSION(MAX_N_ANGLES,MAX_N_ANGLES) :: trans1,trans3,trans4,temp1,temp2,temp3
    REAL(fp), INTENT(OUT),DIMENSION(MAX_N_ANGLES) :: C1_AD, C2_AD
    REAL(fp) :: s, c
    REAL(fp) :: s_AD, c_AD, Delta_Tau_AD
    INTEGER :: i,j,L

    ! Tangent-Linear Beginning
    IF( optical_depth < OPTICAL_DEPTH_THRESHOLD ) THEN
      trans_AD = ZERO
      refl_AD = ZERO
      source_up_AD = ZERO
      source_down_AD = ZERO
      RETURN
    ENDIF

    DO i = NANG, 1, -1
      source_up_AD(i) = source_up_AD(i) + source_down_AD(i)
      source_down_AD(i) = ZERO
      C2_AD(i) = -source_up_AD(i)*Planck_Func
      C1_AD(i) = -source_up_AD(i)*Planck_Func
      Planck_Func_AD = Planck_Func_AD + (ONE-RTV%C1(i,KL)-RTV%C2(i,KL))*source_up_AD(i)
    END DO

    ! Compute the source function in the up and downward directions.
    IF(NANG == (n_Streams+1)) THEN
        trans_AD(NANG,NANG)=trans_AD(NANG,NANG)+C1_AD(NANG)
    ENDIF
    DO i = NANG, 1, -1
      DO j = n_Streams, 1, -1 
        refl_AD(i,j)=refl_AD(i,j)+C2_AD(i)
        trans_AD(i,j)=trans_AD(i,j)+C1_AD(i)
      END DO
    END DO

    DO L = RTV%Number_Doubling(KL), 1, -1 

      CALL myMATMUL(RTV%Trans(:,:,L-1,KL),RTV%Inv_BeT(:,:,L,KL), term1)
      CALL myMATMUL(RTV%Inv_BeT(:,:,L,KL),RTV%Refl(:,:,L-1,KL), term2)
      CALL myMATMUL(RTV%Inv_BeT(:,:,L,KL),RTV%Trans(:,:,L-1,KL), term3)
      CALL myMATMUL(term2,RTV%Trans(:,:,L-1,KL), term4)

      CALL myTRANSPOSE(term1,trans1)
      CALL myTRANSPOSE(term3,trans3)
      CALL myTRANSPOSE(term4,trans4)

      CALL myMATMUL(trans1,trans_AD,temp1)
      CALL myMATMUL(temp1,trans3,term5_AD)

      CALL myMATMUL(trans_AD,trans3,temp1)
      CALL myMATMUL(trans1,trans_AD,temp2)
      CALL myADD(temp1,temp2,trans_AD)

      CALL myMATMUL(term1,RTV%Refl(:,:,L-1,KL),temp1)
      CALL myTRANSPOSE(temp1,temp2)
      CALL myMATMUL(temp2,refl_AD,temp1) 
      CALL myADD(trans_AD,temp1,trans_AD)

      CALL myMATMUL(trans1,refl_AD,temp2)
      CALL myMATMUL(temp2,trans4,temp1)
      CALL myADD(term5_AD,temp1,term5_AD)

      CALL myMATMUL(refl_AD,trans4,temp1)
      CALL myADD(trans_AD,temp1,trans_AD)

      CALL myMATMUL(trans1,refl_AD,temp1)
      CALL myTRANSPOSE(RTV%Trans(:,:,L-1,KL),temp3)
      CALL myMATMUL(temp1,temp3,temp2)
      CALL myADD(refl_AD,temp2,refl_AD)

      CALL myTRANSPOSE(RTV%Refl(:,:,L-1,KL),temp3)
      CALL myMATMUL(term5_AD,temp3,temp1)
      CALL myADD(refl_AD,temp1,refl_AD)

      CALL myMATMUL(temp3,term5_AD, temp1)
      CALL myADD(refl_AD,temp1,refl_AD)

    ENDDO

    s = RTV%Delta_Tau(KL) * single_albedo
    c_AD = ZERO
    s_AD = ZERO
    Delta_Tau_AD=ZERO

    DO i = NANG, 1, -1

      c = s/COS_Angle(i)
      Delta_Tau_AD = Delta_Tau_AD - trans_AD(i,i)/COS_Angle(i)

      DO j = NANG, 1, -1
        c_AD = c_AD + trans_AD(i,j)*ff(i,j)*COS_Weight(j)
        ff_AD(i,j)=ff_AD(i,j)+trans_AD(i,j)*c*COS_Weight(j)
        c_AD = c_AD + refl_AD(i,j)*bb(i,j)*COS_Weight(j)
        bb_AD(i,j)=bb_AD(i,j) + refl_AD(i,j)*c*COS_Weight(j)
      END DO

      s_AD = s_AD + c_AD/COS_Angle(i) 
      c_AD = ZERO

    ENDDO

    Delta_Tau_AD = Delta_Tau_AD + s_AD* single_albedo
    single_albedo_AD = single_albedo_AD+RTV%Delta_Tau(KL) * s_AD
    optical_depth_AD = optical_depth_AD + Delta_Tau_AD/(TWO**RTV%Number_Doubling(KL))

  END SUBROUTINE CRTM_Doubling_layer_AD

END MODULE my_kernels


!---------------------
!
! test_kernels
! 
! Driver
!---------------------
PROGRAM test_kernels

  USE my_kernels
  USE omp_lib
  USE mt19937_64
  USE crtm_type
  USE crtm_utils

  !------- Test -------!
  INTEGER, PARAMETER :: N_LAYERS = MAX_N_LAYERS
  INTEGER, PARAMETER :: N_PROFILESxCHANNELS = 100
  TYPE(RTV_type), ALLOCATABLE, DIMENSION(:) :: RTV
  INTEGER :: k, t, alloc_stat, n_omp_threads, streamid
  INTEGER :: s, e
  INTEGER :: count_rate, count_start, count_end
  REAL :: elapsed

  !---- local arrays --- !
  REAL(fp), ALLOCATABLE, DIMENSION( :,:,:,: ) :: Pff_AD, Pbb_AD, s_Refl_AD, s_Trans_AD
  REAL(fp), ALLOCATABLE, DIMENSION( :,:,: ) :: s_source_UP_AD, s_source_DOWN_AD
  REAL(fp), ALLOCATABLE, DIMENSION(:,:) ::  w, T_OD
  REAL(fp), ALLOCATABLE, DIMENSION(:,:) :: w_AD, T_OD_AD
  REAL(fp), ALLOCATABLE, DIMENSION(:,:) :: Planck_Atmosphere_AD 

  REAL(fp), DIMENSION(MAX_N_ANGLES,MAX_N_ANGLES) :: term1, term2, term3, term4, term5_AD
  REAL(fp), DIMENSION(MAX_N_ANGLES,MAX_N_ANGLES) :: trans1, trans3, trans4, temp1, temp2, temp3
  REAL(fp), DIMENSION(MAX_N_ANGLES) :: C1_AD, C2_AD

  !---- openmp -----!
!$OMP PARALLEL
!$OMP SINGLE
  n_omp_threads = OMP_GET_NUM_THREADS()
!$OMP END SINGLE
!$OMP END PARALLEL

  WRITE(6,*)
  WRITE(6,'("   Using",i3," OpenMP threads for ",i3," profiles and channels.")') &
         n_omp_threads, N_PROFILESxCHANNELS
  WRITE(6,'("   N_LAYERS = ",i3,", N_ANGLES =",i3)') &
         N_LAYERS, MAX_N_ANGLES
  WRITE(6,*)

  !---- allocate ----!
  ALLOCATE(RTV(N_PROFILESxCHANNELS), &
           STAT = alloc_stat)
  IF ( alloc_stat /= 0 ) STOP

  ALLOCATE(Pff_AD(MAX_N_ANGLES, MAX_N_ANGLES+1, N_LAYERS, N_PROFILESxCHANNELS), &
           Pbb_AD(MAX_N_ANGLES, MAX_N_ANGLES+1, N_LAYERS, N_PROFILESxCHANNELS), &
           s_Refl_AD(MAX_N_ANGLES, MAX_N_ANGLES, N_LAYERS, N_PROFILESxCHANNELS), &
           s_Trans_AD(MAX_N_ANGLES, MAX_N_ANGLES, N_LAYERS, N_PROFILESxCHANNELS), &
           s_source_UP_AD(MAX_N_ANGLES, N_LAYERS, N_PROFILESxCHANNELS), &
           s_source_DOWN_AD(MAX_N_ANGLES, N_LAYERS, N_PROFILESxCHANNELS), &
           w(N_LAYERS, N_PROFILESxCHANNELS), &
           T_OD(N_LAYERS, N_PROFILESxCHANNELS), &
           w_AD(N_LAYERS, N_PROFILESxCHANNELS), &
           T_OD_AD(N_LAYERS, N_PROFILESxCHANNELS), &
           Planck_Atmosphere_AD(0:N_LAYERS, N_PROFILESxCHANNELS), &
           STAT = alloc_stat )
  IF ( alloc_stat /= 0 ) STOP

  !---- fill arrays with random numbers ----!
  PRINT*, "Filling arrays with random values"
  CALL mt19937_real4d(Pff_AD)
  CALL mt19937_real4d(Pbb_AD)
  CALL mt19937_real4d(s_Refl_AD)
  CALL mt19937_real4d(s_Trans_AD)
  CALL mt19937_real3d(s_source_UP_AD)
  CALL mt19937_real3d(s_source_DOWN_AD)
  CALL mt19937_real2d(w)
  CALL mt19937_real2d(T_OD)
  CALL mt19937_real2d(w_AD)
  CALL mt19937_real2d(T_OD_AD)
  CALL mt19937_real2d(Planck_Atmosphere_AD)
  PRINT*, "Finished filling arrays with random values."

  !---- fill RTV ----!
  DO t = 1, N_PROFILESxCHANNELS
      RTV(t)%n_Streams = 6
  END DO

  !---- create RTV array ----!
  PRINT*, "Creating RTV"
  DO t = 1, N_PROFILESxCHANNELS
      CALL RTV_Create( RTV(t), MAX_N_ANGLES, MAX_N_LEGENDRE_TERMS, N_LAYERS )
  ENDDO
  PRINT*, "Finished creating RTV"

  !------- Print input state statistics -------!
  CALL print_state("Input state", &
                    MAX_N_ANGLES, &
                        N_LAYERS, &
             N_PROFILESxCHANNELS, &
                          Pff_AD, &
                          Pbb_AD, &
                       s_Refl_AD, &
                      s_Trans_AD, &
                  s_source_UP_AD, &
                s_source_DOWN_AD, &
                               w, &
                            T_OD, &
                            w_AD, &
                         T_OD_AD, &
            Planck_Atmosphere_AD, &
                             RTV)

  !---- call kernel ----!
  PRINT*, "Calling kernel"  
  CALL SYSTEM_CLOCK (count_rate=count_rate)
  CALL SYSTEM_CLOCK (count=count_start)

!$omp parallel do private(t,k,streamid, &
!$omp            term1,term2,term3,term4,term5_AD, &
!$omp            trans1,trans3,trans4,temp1,temp2,temp3,C1_AD,C2_AD)
  DO t = 1, N_PROFILESxCHANNELS

    streamid = mod(t - 1,n_omp_threads)

    DO k = 1, N_LAYERS
      streamid = k
      CALL CRTM_Doubling_layer_AD(RTV(t)%n_Streams,                     & ! Input
                                  RTV(t)%n_Angles,                      & ! Input
                                  k,                                    & ! Input
                                  w( k, t ),                            & ! Input
                                  T_OD( k, t ),                         & ! Input
                                  RTV(t)%COS_Angle,                     & ! Input
                                  RTV(t)%COS_Weight,                    & ! Input
                                  RTV(t)%Pff( :, :, k ),                & ! Input
                                  RTV(t)%Pbb( :, :, k ),                & ! Input
                                  RTV(t)%Planck_Atmosphere( k ),        & ! Input
                                  s_trans_AD( :, :, k, t ),             & ! Input / Output
                                  s_refl_AD( :, :, k, t ),              & ! Input / Output
                                  s_source_up_AD( :, k, t ),            & ! Input / Output
                                  s_source_down_AD( :, k, t ),          & ! Input / Output
                                  RTV(t),                               & ! Input
                                  w_AD( k, t ),                         & ! Input / Output
                                  T_OD_AD( k, t ),                      & ! Input / Output
                                  Pff_AD( :, :, k, t ),                 & ! Input / Output
                                  Pbb_AD( :, :, k, t ),                 & ! Input / Output
                                  Planck_Atmosphere_AD( k, t ),         & ! Input / Output
                                  streamid,                             & ! Input
                                  term1, term2, term3, term4, term5_AD, & ! Output
                                  trans1, trans3, trans4,               & ! Output
                                  temp1, temp2, temp3,                  & ! Output
                                  C1_AD, C2_AD)                           ! Output
    ENDDO

  ENDDO
!$omp end parallel do

  CALL SYSTEM_CLOCK (count=count_end)
  elapsed = REAL (count_end - count_start) / REAL (count_rate)
  PRINT*
  PRINT*
  PRINT*, "Finished executing kernel in =", elapsed  
  PRINT*

  !------- Print output state statistics -------!
  CALL print_state("Output state", &
                     MAX_N_ANGLES, &
                         N_LAYERS, &
              N_PROFILESxCHANNELS, &
                           Pff_AD, &
                           Pbb_AD, &
                        s_Refl_AD, &
                       s_Trans_AD, &
                   s_source_UP_AD, &
                 s_source_DOWN_AD, &
                                w, &
                             T_OD, &
                             w_AD, &
                          T_OD_AD, &
             Planck_Atmosphere_AD, &
                              RTV)

END PROGRAM test_kernels
