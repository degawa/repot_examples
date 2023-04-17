module config_inmemory_repo
    use, intrinsic :: iso_fortran_env
    use :: repot
    use :: config_ids
    implicit none
    private

    character(*), public, parameter :: fmt_in_memory = "in_memory"
        !! format specifier

    type, public, extends(base_repository_atype) :: config_inMemory_repository_type
        real(real64) :: initial_value
        integer(int32) :: number_of_iterations
    contains
        procedure, public, pass :: construct
        !* constructs an instance

        procedure, public, pass :: find
        !* returns presence status of the value identified based on the ID

        procedure, public, pass :: get_int32
        !* returns the int32 value identified based on the ID
        procedure, public, pass :: get_real64
        !* returns the real64 value identified based on the ID
    end type config_inMemory_repository_type

contains
    !>constructs a `config_inMemory_repository_type` instance
    subroutine construct(this, args)
        implicit none
        class(config_inMemory_repository_type), intent(inout) :: this
        class(repository_constructor_arguments_type), intent(in) :: args

        return
        if (same_type_as(this, this)) return
        if (same_type_as(args, args)) return
    end subroutine construct

    !>returns `.true.` when the id presents in the repository
    !>and returns `.false.` elsewhere.
    function find(this, id) result(found)
        implicit none
        class(config_inMemory_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        logical :: found

        select case (trim(id))
        case (Config_ID_Initial_Value &
              , Config_ID_Number_of_Iterations &
              )
            found = .true.
        case default
            found = .false.
        end select

        if (same_type_as(this, this)) return
    end function find

    !>returns the 4-byte integer value identified based on the id
    subroutine get_int32(this, id, val)
        implicit none
        class(config_inMemory_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        integer(int32), intent(out) :: val

        select case (trim(id))
        case (Config_ID_Number_of_Iterations)
            val = this%number_of_iterations
        end select
    end subroutine get_int32

    !>returns the 8-byte real value identified based on the id
    subroutine get_real64(this, id, val)
        implicit none
        class(config_inMemory_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        real(real64), intent(out) :: val

        select case (trim(id))
        case (Config_ID_Initial_Value)
            val = this%initial_value
        end select
    end subroutine get_real64
end module config_inmemory_repo
