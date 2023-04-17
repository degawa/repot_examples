# repot_examples
This repository provides practical usage examples of an abstract data type designed for the repository pattern defined as [repot](https://github.com/degawa/repot).
In this example, the abstract data type `base_repository_atype` is extended for reading an integer value and real value from repositories in 4 different formats:
- json
- namelist
- user-defined
- in-memory

In-memory here means that the data is not placed in an external file and is held in memory as components of an extended user-defined type.

This repository also provides examples of internal representations relating IDs (keys) to value. One is to use a select-case statement, and another is to use a hashmap.

From the main routine in `app/app.f90`, we can see that being able to get data through the common interface regardless of the repository types, i.e., external file format or internal representation.

```Fortran
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
```

The examples require the following to build:

- Modern Fortran compiler
    - gfortran 11.2 bundled with [quickstart Fortran on Windows](https://github.com/LKedward/quickstart-fortran)
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- [Fortran Package Manager](https://github.com/fortran-lang/fpm) (fpm) 0.7.0 alpha
- [JSON-Fortran](https://github.com/jacobwilliams/json-fortran)
- [stdlib](https://github.com/fortran-lang/stdlib)
- [M_CLI2](https://github.com/urbanjost/M_CLI2)

### Get the code
To get the code, execute the following command:

```console
git clone https://github.com/degawa/repot_examples.git
cd repot_examples
```

### gfortran
#### build
To build the example using fpm with gfortran, execute the following command:

```console
fpm build
```

#### execute
Execute the example with command line arguments. The example reads values from the data storage specified by the arguments.

```console
>fpm run -- --format "in_memory"
initial value:4.000000000000000
number of iterations:100

>fpm run -- --format "in_memory_hashmap"
initial value:9.0000000000000000
number of iterations:25

>cat app/config.json
{
    "unknown": {
        "initial value": 1.0
    },
    "iterative solver": {
        "number of iterations": 1
    }
}
>fpm run -- --format "json" --filename "app/config.json"
initial value:1.0000000000000000
number of iterations:1

>cat app/config.nml
&simulation_configulation
    config%initial_value = -100d0
    config%number_of_iterations = 20
/

>fpm run -- --format "namelist" --filename "app/config.nml"
initial value:-100.00000000000000
number of iterations:20

>cat app/config
-1.0
1000
>fpm run -- --format "user_defined" --filename "app/config"
initial value:-1.0000000000000000
number of iterations:1000
```

### Intel Fortran
#### build
To build the example using fpm with gfortran, execute the following command:

```console
fpm build --compiler ifort
```

#### execute
Execute the example with command line arguments. The example reads values from the data storage specified by the arguments.

```console
>fpm run --compiler ifort -- --format "in_memory"
initial value:4.000000000000000
number of iterations:100

>fpm run --compiler ifort -- --format "in_memory_hashmap"
initial value:9.0000000000000000
number of iterations:25

>fpm run --compiler ifort -- --format "json" --filename "app/config.json"
initial value:1.0000000000000000
number of iterations:1

>fpm run --compiler ifort -- --format "namelist" --filename "app/config.nml"
initial value:-100.00000000000000
number of iterations:20

>fpm run --compiler ifort -- --format "user_defined" --filename "app/config"
initial value:-1.0000000000000000
number of iterations:1000
```

### NAG Fortran
#### modification of JSON-Fortran
NAG Fortran compiler cannot build json-fortran because the preprocessor's behavior differs from gfortran and Intel Fortran. Some modifications are required to build json-fortran.

- Copy the `json_initialize_dummy_arguments.inc` to `json_initialize_dummy_arguments_nagfor.inc` and add one more `&` to the final line of `json_initialize_dummy_arguments_nagfor.inc`.

```diff
-strict_integer_type_checking &
+strict_integer_type_checking &&
```

- Define the preprocessor macro in the `#if` block added to the top of `json_value_module.F90` and `json_file_module.F90` for choosing the appropriate `inc` file.

```diff
+#if defined(NAGFOR)
+#define JSON_INIT_DUMMY_ARGS "json_initialize_dummy_arguments_nagfor.inc"
+#else
+#define JSON_INIT_DUMMY_ARGS "json_initialize_dummy_arguments.inc"
+#endif
```

- Replace the `#include` target to the macro in `json_value_module.F90` and `json_file_module.F90`.

```diff
-#include "json_initialize_dummy_arguments.inc"
+#include JSON_INIT_DUMMY_ARGS
```

#### build
To build the example using fpm with gfortran, execute the following command:

```console
fpm build --compiler nagfor --flag "-I build\dependencies\json-fortran\src"
```

#### execute
Execute the example with command line arguments. The example reads values from the data storage specified by the arguments.

```console
>fpm run --compiler nagfor --flag "-I build\dependencies\json-fortran\src" -- --format "in_memory"
initial value:4.000000000000000
number of iterations:100

>fpm run --compiler nagfor --flag "-I build\dependencies\json-fortran\src" -- --format "in_memory_hashmap"
initial value:9.000000000000000
number of iterations:25

>fpm run --compiler nagfor --flag "-I build\dependencies\json-fortran\src" -- --format "json" --filename "app/config.json"
initial value:1.000000000000000
number of iterations:1

>fpm run --compiler nagfor --flag "-I build\dependencies\json-fortran\src" -- --format "namelist" --filename "app/config.nml"
initial value:-100.0000000000000
number of iterations:20

>fpm run --compiler nagfor --flag "-I build\dependencies\json-fortran\src" -- --format "user_defined" --filename "app/config"
initial value:-1.000000000000000
number of iterations:1000
```
