MODULE crtm_type

  IMPLICIT NONE

  INTEGER, PARAMETER :: fp = SELECTED_REAL_KIND(15)
  REAL(fp), PARAMETER :: ZERO          =  0.0_fp
  REAL(fp), PARAMETER :: ONE           =  1.0_fp
  REAL(fp), PARAMETER :: TWO           =  2.0_fp
  REAL(fp), PARAMETER :: OPTICAL_DEPTH_THRESHOLD = 0.000001_fp
  INTEGER, PARAMETER :: MAX_N_LAYERS   = 200
  INTEGER, PARAMETER :: INVALID_SENSOR = 0
  INTEGER, PARAMETER :: RT_ADA = 56
  INTEGER, PARAMETER :: MAX_N_ANGLES = 16
  INTEGER, PARAMETER :: MAX_N_LEGENDRE_TERMS = 16
  INTEGER, PARAMETER :: MAX_N_DOUBLING = 55
  INTEGER, PARAMETER :: MAX_N_SOI_ITERATIONS = 75

  !---- RTV type ---!
  TYPE :: RTV_type
  
    INTEGER :: n_Layers         = 0       ! Total number of atmospheric layers
    INTEGER :: n_Angles         = 0       ! Number of angles to be considered
    INTEGER :: n_SOI_Iterations = 0       ! Number of SOI iterations
    
    ! Planck radiances
    REAL(fp)                               :: Planck_Surface    = ZERO
    REAL(fp), DIMENSION(  0:MAX_N_LAYERS ) :: Planck_Atmosphere = ZERO

    ! Quadrature information
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Angle  = ZERO  ! Gaussian quadrature abscissa
    REAL(fp), DIMENSION( MAX_N_ANGLES ) :: COS_Weight = ZERO  ! Gaussian quadrature weights
    
    ! Scattering, visible model variables    
    INTEGER :: n_Streams         = 0       ! Number of *hemispheric* stream angles used in RT    

    !-----------------------------------
    ! Variables used in the ADA routines
    !-----------------------------------
    ! Flag to indicate the following arrays have all been allocated
    LOGICAL :: Is_Allocated = .FALSE.
     
    ! Phase function variables
    ! Forward and backward scattering phase matrices
    REAL(fp), ALLOCATABLE :: Pff(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS
    REAL(fp), ALLOCATABLE :: Pbb(:,:,:)  ! MAX_N_ANGLES, MAX_N_ANGLES+1, MAX_N_LAYERS

    !-----------------------------------
    ! Variables used in the SOI routines
    !-----------------------------------
    INTEGER :: Number_SOI_Iter = 0

    INTEGER , ALLOCATABLE :: Number_Doubling(:)  ! n_Layers
    REAL(fp), ALLOCATABLE :: Delta_Tau(:)        ! n_Layers
    REAL(fp), ALLOCATABLE :: Refl(:,:,:,:)       ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: Trans(:,:,:,:)      ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: Inv_BeT(:,:,:,:)    ! n_Angles, n_Angles, 0:MAX_N_DOUBLING, n_Layers
    REAL(fp), ALLOCATABLE :: C1(:,:)             ! n_Angles, n_Layers
    REAL(fp), ALLOCATABLE :: C2(:,:)             ! n_Angles, n_Layers

  END TYPE RTV_type

 CONTAINS

END MODULE crtm_type
