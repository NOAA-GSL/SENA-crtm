MODULE crtm_utils

  USE crtm_type, only : RTV_type, fp, MAX_N_DOUBLING

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: print_state

CONTAINS

  !------------------------------------------------------------------
  ! print_state
  !
  ! Prints statistics for the kernel state variables
  !------------------------------------------------------------------
  SUBROUTINE print_state(msg, &
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

    CHARACTER(LEN=*) :: msg
    INTEGER,        INTENT(IN) :: MAX_N_ANGLES, N_LAYERS, N_PROFILESxCHANNELS
    REAL(fp),       INTENT(IN) :: Pff_AD(:,:,:,:)
    REAL(fp),       INTENT(IN) :: Pbb_AD(:,:,:,:)
    REAL(fp),       INTENT(IN) :: s_Refl_AD(:,:,:,:)
    REAL(fp),       INTENT(IN) :: s_Trans_AD(:,:,:,:)
    REAL(fp),       INTENT(IN) :: s_source_UP_AD(:,:,:)
    REAL(fp),       INTENT(IN) :: s_source_DOWN_AD(:,:,:)
    REAL(fp),       INTENT(IN) :: w(:,:)
    REAL(fp),       INTENT(IN) :: T_OD(:,:)
    REAL(fp),       INTENT(IN) :: w_AD(:,:)
    REAL(fp),       INTENT(IN) :: T_OD_AD(:,:)
    REAL(fp),       INTENT(IN) :: Planck_Atmosphere_AD(:,:)
    TYPE(RTV_type), INTENT(IN) :: RTV(:)

    INTEGER               :: i
    REAL(fp), ALLOCATABLE :: temp2d(:,:), temp3d(:,:,:), temp4d(:,:,:,:), temp5d(:,:,:,:,:)

    WRITE(*,'(A4)') "TEST"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A32)') "TEST ", msg
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("=",117)
    WRITE(*,'(A5,A17,5A20)') "TEST ", "Variable", "Min", "Max", "First", "Last", "RMS"
    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)

    CALL print_4d_variable("Pff_AD", Pff_AD)
    CALL print_4d_variable("Pbb_AD", Pbb_AD)
    CALL print_4d_variable("s_Refl_AD", s_Refl_AD)
    CALL print_4d_variable("s_Trans_AD", s_Trans_AD)
    CALL print_3d_variable("s_source_UP_AD", s_source_UP_AD)
    CALL print_3d_variable("s_source_DOWN_AD", s_source_DOWN_AD)
    CALL print_2d_variable("w", w)
    CALL print_2d_variable("T_OD", T_OD)
    CALL print_2d_variable("w_AD", w_AD)
    CALL print_2d_variable("T_OD_AD", T_OD_AD)
    CALL print_2d_variable("Planck_Atmosphere_AD", Planck_Atmosphere_AD)

    ALLOCATE(temp4d(N_PROFILESxCHANNELS, MAX_N_ANGLES, MAX_N_ANGLES + 1, N_LAYERS))
    DO i = 1, N_PROFILESxCHANNELS
      temp4d(i, :, :, :) = RTV(i)%Pff
    END DO
    CALL print_4d_variable("RTV%Pff", temp4d)
    DO i = 1, N_PROFILESxCHANNELS
      temp4d(i, :, :, :) = RTV(i)%Pbb
    END DO
    CALL print_4d_variable("RTV%Pbb", temp4d)
    DEALLOCATE(temp4d)

    ALLOCATE(temp2d(N_PROFILESxCHANNELS, N_LAYERS))
    DO i = 1, N_PROFILESxCHANNELS
      temp2d(i, :) = RTV(i)%Delta_Tau
    END DO
    CALL print_2d_variable("RTV%Delta_Tau", temp2d)
    DEALLOCATE(temp2d)

    ALLOCATE(temp5d(N_PROFILESxCHANNELS, MAX_N_ANGLES, MAX_N_ANGLES, MAX_N_DOUBLING + 1, N_LAYERS))
    DO i = 1, N_PROFILESxCHANNELS
      temp5d(i, :, :, :, :) = RTV(i)%Refl
    END DO
    CALL print_5d_variable("RTV%Refl", temp5d)
    DO i = 1, N_PROFILESxCHANNELS
      temp5d(i, :, :, :, :) = RTV(i)%Trans
    END DO
    CALL print_5d_variable("RTV%Trans", temp5d)
    DO i = 1, N_PROFILESxCHANNELS
      temp5d(i, :, :, :, :) = RTV(i)%Inv_BeT
    END DO
    CALL print_5d_variable("RTV%Inv_BeT", temp5d)
    DEALLOCATE(temp5d)

    ALLOCATE(temp2d(N_PROFILESxCHANNELS, MAX_N_ANGLES))
    DO i = 1, N_PROFILESxCHANNELS
      temp2d(i, :) = RTV(i)%COS_Angle
    END DO
    CALL print_2d_variable("RTV%COS_Angle", temp2d)
    DO i = 1, N_PROFILESxCHANNELS
      temp2d(i, :) = RTV(i)%COS_Weight
    END DO
    CALL print_2d_variable("RTV%COS_Weight", temp2d)
    DEALLOCATE(temp2d)

    ALLOCATE(temp3d(N_PROFILESxCHANNELS, MAX_N_ANGLES, N_LAYERS))
    DO i = 1, N_PROFILESxCHANNELS
      temp3d(i, :, :) = RTV(i)%C1
    END DO
    CALL print_3d_variable("RTV%C1", temp3d)
    DO i = 1, N_PROFILESxCHANNELS
      temp3d(i, :, :) = RTV(i)%C2
    END DO
    CALL print_3d_variable("RTV%C2", temp3d)
    DEALLOCATE(temp3d)

    WRITE(*,'(A5,A117)') "TEST ", REPEAT("-",117)
    WRITE(*,'(A4)') "TEST"

  END SUBROUTINE print_state


  !------------------------------------------------------------------
  ! print_2d_variable
  !
  ! Prints statistics for a 2d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_2d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(fp)         :: data(:,:)

    ! Note: Assumed shape array sections always start with index=1 for all
    ! dimensions
    !       So we don't have to know start/end indices here
    WRITE(*,'(A5, A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1), &
                            data(SIZE(data,1), SIZE(data,2)),            &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_2d_variable

  !------------------------------------------------------------------
  ! print_3d_variable
  !
  ! Prints statistics for a 3d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_3d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(fp)         :: data(:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_3d_variable

  !------------------------------------------------------------------
  ! print_4d_variable
  !
  ! Prints statistics for a 4d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_4d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(fp)         :: data(:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_4d_variable


  !------------------------------------------------------------------
  ! print_5d_variable
  !
  ! Prints statistics for a 5d state variable
  !------------------------------------------------------------------
  SUBROUTINE print_5d_variable(name, data)

    CHARACTER(LEN=*) :: name
    REAL(fp)         :: data(:,:,:,:,:)

    ! Note: Assumed shape array sections always start with index=1 for all dimensions
    !       So we do not have to know start/end indices here
    WRITE(*,'(A5,A17,5ES20.10)') "TEST ", name, MINVAL(data), MAXVAL(data), data(1,1,1,1,1),  &
                            data(SIZE(data,1), SIZE(data,2), SIZE(data,3), SIZE(data,4), SIZE(data,5)), &
                            SQRT(SUM(data**2) / SIZE(data))

  END SUBROUTINE print_5d_variable


END MODULE crtm_utils
