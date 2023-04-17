module config_inmemory_hashmap_repo
    use, intrinsic :: iso_fortran_env
    use :: repot
    use :: config_ids
    use stdlib_hashmaps, only: chaining_hashmap_type
    use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set, get
    implicit none
    private

    character(*), public, parameter :: fmt_in_memory_hashmap = "in_memory_hashmap"
        !! format specifier

    type, public, extends(base_repository_atype) :: config_inMemory_hashmap_repository_type
        real(real64) :: initial_value
        integer(int32) :: number_of_iterations

        type(chaining_hashmap_type), private :: map
            !! hashmap containing pairs of id and value
    contains
        procedure, public, pass :: construct
        !* constructs an instance

        procedure, public, pass :: find
        !* returns presence status of the value identified based on the ID

        procedure, public, pass :: get_int32
        !* returns the int32 value identified based on the ID
        procedure, public, pass :: get_real64
        !* returns the real64 value identified based on the ID
    end type config_inMemory_hashmap_repository_type
contains
    !>constructs a `config_inMemory_hashmap_repository_type` instance
    subroutine construct(this, args)
        implicit none
        class(config_inMemory_hashmap_repository_type), intent(inout) :: this
        class(repository_constructor_arguments_type), intent(in) :: args
            !! repository format and name

        if (args%get_format() == fmt_in_memory_hashmap) then
            call this%map%init(fnv_1_hasher)
            call append(this%map, Config_ID_Initial_Value, this%initial_value)
            call append(this%map, Config_ID_Number_of_Iterations, this%number_of_iterations)
        end if
    contains
        subroutine append(map, key, dat)
            implicit none
            type(chaining_hashmap_type), intent(inout) :: map
            character(*), intent(in) :: key
            class(*), intent(in) :: dat

            type(key_type) :: hash_key
            type(other_type) :: hash_dat
            logical :: conflict

            call set(hash_key, key)
            call set(hash_dat, dat)
            call map%map_entry(hash_key, hash_dat, conflict)
            if (conflict) print '(A)', "The key "//key//" is already present in the map."
        end subroutine append
    end subroutine construct

    !>returns `.true.` when the id presents in the repository
    !>and returns `.false.` elsewhere.
    function find(this, id) result(found)
        implicit none
        class(config_inMemory_hashmap_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        logical :: found

        type(key_type) :: hash_key
        type(other_type) :: hash_dat
        logical :: present_hash_key, present_hash_dat

        call set(hash_key, trim(id))
        call this%map%key_test(hash_key, present_hash_key)
        call this%map%get_other_data(hash_key, hash_dat, present_hash_dat)

        found = present_hash_key .and. present_hash_dat
    end function find

    !>returns the 4-byte integer value identified based on the id
    subroutine get_int32(this, id, val)
        implicit none
        class(config_inMemory_hashmap_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        integer(int32), intent(out) :: val

        class(*), allocatable :: dat
        call get_dat(this%map, id, dat)

        select type (dat); type is (integer(int32))
            val = dat
        end select
    end subroutine get_int32

    !>returns the 8-byte real value identified based on the id
    subroutine get_real64(this, id, val)
        implicit none
        class(config_inMemory_hashmap_repository_type), intent(inout) :: this
        character(*), intent(in) :: id
        real(real64), intent(out) :: val

        class(*), allocatable :: dat
        call get_dat(this%map, id, dat)

        select type (dat); type is (real(real64))
            val = dat
        end select
    end subroutine get_real64

    !>gets data from the hashmap
    subroutine get_dat(map, id, dat)
        implicit none
        type(chaining_hashmap_type), intent(inout) :: map
        character(*), intent(in) :: id
        class(*), allocatable, intent(out) :: dat

        type(key_type) :: hash_key
        type(other_type) :: hash_dat

        call set(hash_key, trim(id))
        call map%get_other_data(hash_key, hash_dat)
        call get(hash_dat, dat)
    end subroutine get_dat
end module config_inmemory_hashmap_repo
