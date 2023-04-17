module config_ids
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    character(*), public, parameter :: Config_ID_Initial_Value = "initial value"
    character(*), public, parameter :: Config_ID_Number_of_Iterations = "number of iterations"
end module config_ids
