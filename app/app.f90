program repo_example
    use, intrinsic :: iso_fortran_env
    use :: repot
    implicit none

    class(base_repository_atype), allocatable :: config_repo

    call configure_repository(config_repo)

    block
        use :: config_ids
        integer(int32) :: num
        real(real64) :: val

        if (config_repo%find(Config_ID_Number_of_Iterations)) &
            call config_repo%get(Config_ID_Number_of_Iterations, num)

        if (config_repo%find(Config_ID_Initial_Value)) &
            call config_repo%get(Config_ID_Initial_Value, val)

        print '(A,":", g0)', Config_ID_Initial_Value, val
        print '(A,":", g0)', Config_ID_Number_of_Iterations, num
    end block

contains
    subroutine get_commandline_args(format, filename)
        use :: M_CLI2, only:set_args, get_args
        implicit none
        character(:), allocatable, intent(out) :: format
        character(:), allocatable, intent(out) :: filename
        namelist /args/ format, filename

        call set_args('--format "in_memory" --filename ""')
        call get_args('format', format)
        call get_args('filename', filename)
    end subroutine get_commandline_args

    subroutine configure_repository(config)
        use :: config_inmemory_repo
        use :: config_inmemory_hashmap_repo
        use :: config_json_repo
        use :: config_namelist_repo
        use :: config_userDefined_repo
        implicit none
        class(base_repository_atype), allocatable, intent(out) :: config
        type(repository_constructor_arguments_type) :: args

        character(:), allocatable :: format
        character(:), allocatable :: filename

        logical :: exists

        call get_commandline_args(format, filename)

        if (len(filename) > 0) then
            inquire (file=filename, exist=exists)
            if (.not. exists) error stop filename//" does not exist"

            args = new_args(format, filename)
        else
            args = new_args(format)
        end if

        select case (args%get_format())
        case (fmt_in_memory)
            allocate (config_inMemory_repository_type :: config)
            select type (config); type is (config_inMemory_repository_type)
                config%initial_value = 4d0
                config%number_of_iterations = 100
            end select

        case (fmt_in_memory_hashmap)
            allocate (config_inMemory_hashmap_repository_type :: config)
            select type (config); type is (config_inMemory_hashmap_repository_type)
                config%initial_value = 9d0
                config%number_of_iterations = 25
            end select

        case (fmt_json)
            allocate (config_json_repository_type :: config)

        case (fmt_namelist)
            allocate (config_namelist_repository_type :: config)

        case (fmt_user_defined)
            allocate (config_userDefined_repository_type :: config)

        case default
            error stop "error: unsupported format "//args%get_format()
        end select

        call config%construct(args)
    end subroutine configure_repository
end program repo_example
