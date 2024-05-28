# - Try to find PETSc
# Once done this will define
#
#  PETSC_FOUND        - system has PETSc
#  PETSC_INCLUDES     - the PETSc include directories
#  PETSC_LIBRARIES    - Link these to use PETSc
#  PETSC_COMPILER     - Compiler used by PETSc, helpful to find a compatible MPI
#  PETSC_DEFINITIONS  - Compiler switches for using PETSc
#  PETSC_MPIEXEC      - Executable for running MPI programs
#  PETSC_VERSION      - Version string (MAJOR.MINOR.SUBMINOR)
#
#  Usage:
#  find_package(PETSc COMPONENTS CXX)  - required if build --with-clanguage=C++ --with-c-support=0
#  find_package(PETSc COMPONENTS C)    - standard behavior of checking build using a C compiler
#  find_package(PETSc)                 - same as above
#
# Setting these changes the behavior of the search
#  PETSC_DIR - directory in which PETSc resides
#  PETSC_ARCH - build architecture
#
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#

set(PETSC_VALID_COMPONENTS
  C
  CXX)

if(NOT PETSc_FIND_COMPONENTS)
  set(PETSC_LANGUAGE_BINDINGS "C")
else()
  # Right now, this is designed for compatability with the --with-clanguage option, so
  # only allow one item in the components list.
  list(LENGTH ${PETSc_FIND_COMPONENTS} components_length)
  if(${components_length} GREATER 1)
    message(FATAL_ERROR "Only one component for PETSc is allowed to be specified")
  endif()
  # This is a stub for allowing multiple components should that time ever come. Perhaps
  # to also test Fortran bindings?
  foreach(component ${PETSc_FIND_COMPONENTS})
    list(FIND PETSC_VALID_COMPONENTS ${component} component_location)
    if(${component_location} EQUAL -1)
      message(FATAL_ERROR "\"${component}\" is not a valid PETSc component.")
    else()
      list(APPEND PETSC_LANGUAGE_BINDINGS ${component})
    endif()
  endforeach()
endif()

function (petsc_get_version)
  if (EXISTS "${PETSC_DIR}/include/petscversion.h")
    file (STRINGS "${PETSC_DIR}/include/petscversion.h" vstrings REGEX "#define PETSC_VERSION_(RELEASE|MAJOR|MINOR|SUBMINOR|PATCH) ")
    foreach (line ${vstrings})
      string (REGEX REPLACE " +" ";" fields ${line}) # break line into three fields (the first is always "#define")
      list (GET fields 1 var)
      list (GET fields 2 val)
      set (${var} ${val} PARENT_SCOPE)
      set (${var} ${val})         # Also in local scope so we have access below
    endforeach ()
    if (PETSC_VERSION_RELEASE)
      set (PETSC_VERSION "${PETSC_VERSION_MAJOR}.${PETSC_VERSION_MINOR}.${PETSC_VERSION_SUBMINOR}p${PETSC_VERSION_PATCH}" PARENT_SCOPE)
    else ()
      # make dev version compare higher than any patch level of a released version
      set (PETSC_VERSION "${PETSC_VERSION_MAJOR}.${PETSC_VERSION_MINOR}.${PETSC_VERSION_SUBMINOR}.99" PARENT_SCOPE)
    endif ()
  else ()
    message (SEND_ERROR "PETSC_DIR can not be used, ${PETSC_DIR}/include/petscversion.h does not exist")
  endif ()
endfunction ()

find_path (PETSC_DIR include/petsc.h
  HINTS ENV PETSC_DIR
  PATHS
  # Debian paths
  /usr/lib/petscdir/3.5.1 /usr/lib/petscdir/3.5
  /usr/lib/petscdir/3.4.2 /usr/lib/petscdir/3.4
  /usr/lib/petscdir/3.3 /usr/lib/petscdir/3.2 /usr/lib/petscdir/3.1
  /usr/lib/petscdir/3.0.0 /usr/lib/petscdir/2.3.3 /usr/lib/petscdir/2.3.2
  # MacPorts path
  /opt/local/lib/petsc
  $ENV{HOME}/petsc
  DOC "PETSc Directory")

find_program (MAKE_EXECUTABLE NAMES make gmake)

if (PETSC_DIR AND NOT PETSC_ARCH)
  set (_petsc_arches
    $ENV{PETSC_ARCH}                   # If set, use environment variable first
    linux-gnu-c-debug linux-gnu-c-opt  # Debian defaults
    x86_64-unknown-linux-gnu i386-unknown-linux-gnu)
  set (petscconf "NOTFOUND" CACHE FILEPATH "Cleared" FORCE)
  foreach (arch ${_petsc_arches})
    if (NOT PETSC_ARCH)
      find_path (petscconf petscconf.h
        HINTS ${PETSC_DIR}
        PATH_SUFFIXES ${arch}/include bmake/${arch}
        NO_DEFAULT_PATH)
      if (petscconf)
        set (PETSC_ARCH "${arch}" CACHE STRING "PETSc build architecture")
      endif (petscconf)
    endif (NOT PETSC_ARCH)
  endforeach (arch)
  set (petscconf "NOTFOUND" CACHE INTERNAL "Scratch variable" FORCE)
endif (PETSC_DIR AND NOT PETSC_ARCH)

set (petsc_slaves LIBRARIES_SYS LIBRARIES_VEC LIBRARIES_MAT LIBRARIES_DM LIBRARIES_KSP LIBRARIES_SNES LIBRARIES_TS
  INCLUDE_DIR INCLUDE_CONF)
include (FindPackageMultipass)
find_package_multipass (PETSc petsc_config_current
  STATES DIR ARCH
  DEPENDENTS INCLUDES LIBRARIES COMPILER MPIEXEC ${petsc_slaves})

# Determine whether the PETSc layout is old-style (through 2.3.3) or
# new-style (>= 3.0.0)
if (EXISTS "${PETSC_DIR}/${PETSC_ARCH}/lib/petsc/conf/petscvariables") # > 3.5
  set (petsc_conf_rules "${PETSC_DIR}/lib/petsc/conf/rules")
  set (petsc_conf_variables "${PETSC_DIR}/lib/petsc/conf/variables")
elseif (EXISTS "${PETSC_DIR}/${PETSC_ARCH}/include/petscconf.h")   # > 2.3.3
  set (petsc_conf_rules "${PETSC_DIR}/conf/rules")
  set (petsc_conf_variables "${PETSC_DIR}/conf/variables")
elseif (EXISTS "${PETSC_DIR}/bmake/${PETSC_ARCH}/petscconf.h") # <= 2.3.3
  set (petsc_conf_rules "${PETSC_DIR}/bmake/common/rules")
  set (petsc_conf_variables "${PETSC_DIR}/bmake/common/variables")
elseif (PETSC_DIR)
  message (SEND_ERROR "The pair PETSC_DIR=${PETSC_DIR} PETSC_ARCH=${PETSC_ARCH} do not specify a valid PETSc installation")
endif ()

if (petsc_conf_rules AND petsc_conf_variables AND NOT petsc_config_current)
  petsc_get_version()

  # Put variables into environment since they are needed to get
  # configuration (petscvariables) in the PETSc makefile
  set (ENV{PETSC_DIR} "${PETSC_DIR}")
  set (ENV{PETSC_ARCH} "${PETSC_ARCH}")

  # A temporary makefile to probe the PETSc configuration
  set (petsc_config_makefile "${PROJECT_BINARY_DIR}/Makefile.petsc")
  file (WRITE "${petsc_config_makefile}"
"## This file was autogenerated by FindPETSc.cmake
# PETSC_DIR  = ${PETSC_DIR}
# PETSC_ARCH = ${PETSC_ARCH}
include ${petsc_conf_rules}
include ${petsc_conf_variables}
show :
\t-@echo -n \${\${VARIABLE}}
")

  macro (PETSC_GET_VARIABLE name var)
    set (${var} "NOTFOUND" CACHE INTERNAL "Cleared" FORCE)
    execute_process (COMMAND ${MAKE_EXECUTABLE} --no-print-directory -f ${petsc_config_makefile} show VARIABLE=${name}
      OUTPUT_VARIABLE ${var}
      RESULT_VARIABLE petsc_return)
  endmacro (PETSC_GET_VARIABLE)
  petsc_get_variable (PETSC_LIB_DIR            petsc_lib_dir)
  petsc_get_variable (PETSC_EXTERNAL_LIB_BASIC petsc_libs_external)
  petsc_get_variable (PETSC_CCPPFLAGS          petsc_cpp_line)
  petsc_get_variable (PETSC_INCLUDE            petsc_include)
  petsc_get_variable (PCC                      petsc_cc)
  petsc_get_variable (PCC_FLAGS                petsc_cc_flags)
  petsc_get_variable (MPIEXEC                  petsc_mpiexec)
  # We are done with the temporary Makefile, calling PETSC_GET_VARIABLE after this point is invalid!
  file (REMOVE ${petsc_config_makefile})

  include (ResolveCompilerPaths)
  # Extract include paths and libraries from compile command line
  resolve_includes (petsc_includes_all "${petsc_cpp_line}")

  #on windows we need to make sure we're linking against the right
  #runtime library
  if (WIN32)
    if (petsc_cc_flags MATCHES "-MT")
      set(using_md False)
      foreach(flag_var
          CMAKE_C_FLAGS CMAKE_C_FLAGS_DEBUG CMAKE_C_FLAGS_RELEASE
          CMAKE_C_FLAGS_MINSIZEREL CMAKE_C_FLAGS_RELWITHDEBINFO
          CMAKE_CXX_FLAGS CMAKE_CXX_FLAGS_DEBUG CMAKE_CXX_FLAGS_RELEASE
          CMAKE_CXX_FLAGS_MINSIZEREL CMAKE_CXX_FLAGS_RELWITHDEBINFO)
        if(${flag_var} MATCHES "/MD")
          set(using_md True)
        endif(${flag_var} MATCHES "/MD")
      endforeach(flag_var)
      if(${using_md} MATCHES "True")
        message(WARNING "PETSc was built with /MT, but /MD is currently set.
 See http://www.cmake.org/Wiki/CMake_FAQ#How_can_I_build_my_MSVC_application_with_a_static_runtime.3F")
      endif(${using_md} MATCHES "True")
    endif (petsc_cc_flags MATCHES "-MT")
  endif (WIN32)

  include (CorrectWindowsPaths)
  convert_cygwin_path(petsc_lib_dir)
  message (STATUS "petsc_lib_dir ${petsc_lib_dir}")

  macro (PETSC_FIND_LIBRARY suffix name)
    set (PETSC_LIBRARY_${suffix} "NOTFOUND" CACHE INTERNAL "Cleared" FORCE) # Clear any stale value, if we got here, we need to find it again
    if (WIN32)
      set (libname lib${name}) #windows expects "libfoo", linux expects "foo"
    else (WIN32)
      set (libname ${name})
    endif (WIN32)
    find_library (PETSC_LIBRARY_${suffix} NAMES ${libname} HINTS ${petsc_lib_dir} NO_DEFAULT_PATH)
    set (PETSC_LIBRARIES_${suffix} "${PETSC_LIBRARY_${suffix}}")
    mark_as_advanced (PETSC_LIBRARY_${suffix})
  endmacro (PETSC_FIND_LIBRARY suffix name)

  # Look for petscvec first, if it doesn't exist, we must be using single-library
  petsc_find_library (VEC petscvec)
  if (PETSC_LIBRARY_VEC)
    petsc_find_library (SYS  "petscsys;petsc") # libpetscsys is called libpetsc prior to 3.1 (when single-library was introduced)
    petsc_find_library (MAT  petscmat)
    petsc_find_library (DM   petscdm)
    petsc_find_library (KSP  petscksp)
    petsc_find_library (SNES petscsnes)
    petsc_find_library (TS   petscts)
    macro (PETSC_JOIN libs deps)
      list (APPEND PETSC_LIBRARIES_${libs} ${PETSC_LIBRARIES_${deps}})
    endmacro (PETSC_JOIN libs deps)
    petsc_join (VEC  SYS)
    petsc_join (MAT  VEC)
    petsc_join (DM   MAT)
    petsc_join (KSP  DM)
    petsc_join (SNES KSP)
    petsc_join (TS   SNES)
    petsc_join (ALL  TS)
  else ()
    set (PETSC_LIBRARY_VEC "NOTFOUND" CACHE INTERNAL "Cleared" FORCE) # There is no libpetscvec
    petsc_find_library (SINGLE petsc)
    foreach (pkg SYS VEC MAT DM KSP SNES TS ALL)
      set (PETSC_LIBRARIES_${pkg} "${PETSC_LIBRARY_SINGLE}")
    endforeach ()
  endif ()
  if (PETSC_LIBRARY_TS)
    message (STATUS "Recognized PETSc install with separate libraries for each package")
  else ()
    message (STATUS "Recognized PETSc install with single library for all packages")
  endif ()

  include(Check${PETSC_LANGUAGE_BINDINGS}SourceRuns)
  macro (PETSC_TEST_RUNS includes libraries runs)
    if(${PETSC_LANGUAGE_BINDINGS} STREQUAL "C")
      set(_PETSC_ERR_FUNC "CHKERRQ(ierr)")
    elseif(${PETSC_LANGUAGE_BINDINGS} STREQUAL "CXX")
      set(_PETSC_ERR_FUNC "CHKERRXX(ierr)")
    endif()
    if (PETSC_VERSION VERSION_GREATER 3.1)
      set (_PETSC_TSDestroy "TSDestroy(&ts)")
    else ()
      set (_PETSC_TSDestroy "TSDestroy(ts)")
    endif ()

    set(_PETSC_TEST_SOURCE "
static const char help[] = \"PETSc test program.\";
#include <petscts.h>
int main(int argc,char *argv[]) {
  PetscErrorCode ierr;
  TS ts;

  ierr = PetscInitialize(&argc,&argv,0,help);${_PETSC_ERR_FUNC};
  ierr = TSCreate(PETSC_COMM_WORLD,&ts);${_PETSC_ERR_FUNC};
  ierr = TSSetFromOptions(ts);${_PETSC_ERR_FUNC};
  ierr = ${_PETSC_TSDestroy};${_PETSC_ERR_FUNC};
  ierr = PetscFinalize();${_PETSC_ERR_FUNC};
  return 0;
}
")
    multipass_source_runs ("${includes}" "${libraries}" "${_PETSC_TEST_SOURCE}" ${runs} "${PETSC_LANGUAGE_BINDINGS}")
    if (${${runs}})
      set (PETSC_EXECUTABLE_RUNS "YES" CACHE BOOL
        "Can the system successfully run a PETSc executable?  This variable can be manually set to \"YES\" to force CMake to accept a given PETSc configuration, but this will almost always result in a broken build.  If you change PETSC_DIR, PETSC_ARCH, or PETSC_CURRENT you would have to reset this variable." FORCE)
    endif (${${runs}})
  endmacro (PETSC_TEST_RUNS)


  find_path (PETSC_INCLUDE_DIR petscts.h HINTS "${PETSC_DIR}" PATH_SUFFIXES include NO_DEFAULT_PATH)
  find_path (PETSC_INCLUDE_CONF petscconf.h HINTS "${PETSC_DIR}" PATH_SUFFIXES "${PETSC_ARCH}/include" "bmake/${PETSC_ARCH}" NO_DEFAULT_PATH)
  mark_as_advanced (PETSC_INCLUDE_DIR PETSC_INCLUDE_CONF)
  set (petsc_includes_minimal ${PETSC_INCLUDE_CONF} ${PETSC_INCLUDE_DIR})

  petsc_test_runs ("${petsc_includes_minimal}" "${PETSC_LIBRARIES_TS}" petsc_works_minimal)
  if (petsc_works_minimal)
    message (STATUS "Minimal PETSc includes and libraries work.  This probably means we are building with shared libs.")
    set (petsc_includes_needed "${petsc_includes_minimal}")
  else (petsc_works_minimal)     # Minimal includes fail, see if just adding full includes fixes it
    petsc_test_runs ("${petsc_includes_all}" "${PETSC_LIBRARIES_TS}" petsc_works_allincludes)
    if (petsc_works_allincludes) # It does, we just need all the includes (
      message (STATUS "PETSc requires extra include paths, but links correctly with only interface libraries.  This is an unexpected configuration (but it seems to work fine).")
      set (petsc_includes_needed ${petsc_includes_all})
    else (petsc_works_allincludes) # We are going to need to link the external libs explicitly
      resolve_libraries (petsc_libraries_external "${petsc_libs_external}")
      foreach (pkg SYS VEC MAT DM KSP SNES TS ALL)
        list (APPEND PETSC_LIBRARIES_${pkg}  ${petsc_libraries_external})
      endforeach (pkg)
      petsc_test_runs ("${petsc_includes_minimal}" "${PETSC_LIBRARIES_TS}" petsc_works_alllibraries)
      if (petsc_works_alllibraries)
         message (STATUS "PETSc only need minimal includes, but requires explicit linking to all dependencies.  This is expected when PETSc is built with static libraries.")
        set (petsc_includes_needed ${petsc_includes_minimal})
      else (petsc_works_alllibraries)
        # It looks like we really need everything, should have listened to Matt
        set (petsc_includes_needed ${petsc_includes_all})
        petsc_test_runs ("${petsc_includes_all}" "${PETSC_LIBRARIES_TS}" petsc_works_all)
        if (petsc_works_all) # We fail anyways
          message (STATUS "PETSc requires extra include paths and explicit linking to all dependencies.  This probably means you have static libraries and something unexpected in PETSc headers.")
        else (petsc_works_all) # We fail anyways
          message (STATUS "PETSc could not be used, maybe the install is broken.")
        endif (petsc_works_all)
      endif (petsc_works_alllibraries)
    endif (petsc_works_allincludes)
  endif (petsc_works_minimal)

  # We do an out-of-source build so __FILE__ will be an absolute path, hence __INSDIR__ is superfluous
  if (${PETSC_VERSION} VERSION_LESS 3.1)
    set (PETSC_DEFINITIONS "-D__SDIR__=\"\"" CACHE STRING "PETSc definitions" FORCE)
  else ()
    set (PETSC_DEFINITIONS "-D__INSDIR__=" CACHE STRING "PETSc definitions" FORCE)
  endif ()
  # Sometimes this can be used to assist FindMPI.cmake
  set (PETSC_MPIEXEC ${petsc_mpiexec} CACHE FILEPATH "Executable for running PETSc MPI programs" FORCE)
  set (PETSC_INCLUDES ${petsc_includes_needed} CACHE STRING "PETSc include path" FORCE)
  set (PETSC_LIBRARIES ${PETSC_LIBRARIES_ALL} CACHE STRING "PETSc libraries" FORCE)
  set (PETSC_COMPILER ${petsc_cc} CACHE FILEPATH "PETSc compiler" FORCE)
  # Note that we have forced values for all these choices.  If you
  # change these, you are telling the system to trust you that they
  # work.  It is likely that you will end up with a broken build.
  mark_as_advanced (PETSC_INCLUDES PETSC_LIBRARIES PETSC_COMPILER PETSC_DEFINITIONS PETSC_MPIEXEC PETSC_EXECUTABLE_RUNS)
endif ()

include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (PETSc
  "PETSc could not be found.  Be sure to set PETSC_DIR and PETSC_ARCH."
  PETSC_INCLUDES PETSC_LIBRARIES PETSC_EXECUTABLE_RUNS)
