module config_json_repo
    use, intrinsic :: iso_fortran_env
    use :: repot
    use :: config_ids
    use :: json_module
    implicit none
    private

    character(*), public, parameter :: fmt_json = "json"
        !! format specifier

    type, public, extends(base_repository_atype) :: config_json_repository_type
        type(json_file) :: json
            !! json file containing pairs of id and value
    contains
        procedure, public, pass :: construct
        !* constructs an instance

        procedure, public, pass :: find
        !* returns presence status of the value identified based on the ID

        procedure, public, pass :: get_int32
        !* returns the int32 value identified based on the ID
        procedure, public, pass :: get_real64
        !* returns the real64 value identified based on the ID
    end type config_json_repository_type

contains
    !>constructs a `config_json_repository_type` instance
    subroutine construct(this, args)
        implicit none
        class(config_json_repository_type), intent(inout) :: this
        class(repository_constructor_arguments_type), intent(in) :: args

        if (args%get_format() == fmt_json .and. len(args%get_filename()) > 0) then
            call this%json%initialize()

            call this%json%load(filename=args%get_filename())
            call this%json%print_error_message(output_unit)
        end if
    end subroutine construct

    !>returns `.true.` when the id presents in the repository
    !>and returns `.false.` elsewhere.
    function find(this, id) result(found)
        implicit none
        class(config_json_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        logical :: found

        character(:), allocatable :: key
        character(:), allocatable :: val

        key = parameter_to_json_key(id)
        call this%json%get(key, val, found=found)
    end function find

    !>returns the 4-byte integer value identified based on the id
    subroutine get_int32(this, id, val)
        implicit none
        class(config_json_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        integer(int32), intent(out) :: val

        character(:), allocatable :: key

        key = parameter_to_json_key(id)
        call this%json%get(key, val)
    end subroutine get_int32

    !>returns the 8-byte real value identified based on the id
    subroutine get_real64(this, id, val)
        implicit none
        class(config_json_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        real(real64), intent(out) :: val

        character(:), allocatable :: key

        key = parameter_to_json_key(id)
        call this%json%get(key, val)
    end subroutine get_real64

    !>returns json key corresiponding to id.
    function parameter_to_json_key(id) result(key)
        implicit none
        character(*), intent(in) :: id
        character(:), allocatable :: key

        select case (trim(id))
        case (Config_ID_Initial_Value)
            key = "unknown.initial value"
        case (Config_ID_Number_of_Iterations)
            key = "iterative solver.number of iterations"
        case default
            key = ""
        end select
    end function parameter_to_json_key
end module config_json_repo
