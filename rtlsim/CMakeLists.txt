include(ProcessorCount)
ProcessorCount(N_THREADS)

option(USE_TRACE "Enable VCD tracing" OFF)
option(USE_TRACE_FST "Enable FST tracing" OFF)

option(USE_ITRACE "Enable instruction tracing" OFF)
option(USE_DROMAJO "Enable Dromajo co-simulation" OFF)

option(USE_RAMULATOR "Enable DRAM simulation" OFF)

find_package(verilator HINTS $ENV{VERILATOR_ROOT} ${VERILATOR_ROOT})
if (NOT verilator_FOUND)
  message(FATAL_ERROR "Verilator was not found. Either install it, or set the VERILATOR_ROOT environment variable")
endif()

if (USE_RAMULATOR)
   file(GLOB_RECURSE RAMULATOR_SRCLIST "ramulator/src/*.cpp")

   add_library(ramulator ${RAMULATOR_SRCLIST})
   target_compile_definitions(ramulator PRIVATE -DRAMULATOR)
   target_include_directories(ramulator PRIVATE ramulator/src)
   set_property(TARGET ramulator PROPERTY POSITION_INDEPENDENT_CODE ON)
endif()

set(SOURCE_FILES main.cpp
                 soc.cpp
                 uart.cpp
                 sdcard.cpp
                 tracer.cpp
                 snapshot.cpp
                 jtagdpi/tcp_server.c
                 jtagdpi/jtagdpi.c)

set(TOP_MODULE soc_wrapper)

set(RTL_INCLUDE /tmp
                ../rtl)

set(VL_FLAGS -O2 --language 1800-2009 --assert -Wall -Wpedantic
             -Wno-WIDTH -Wno-DECLFILENAME -Wno-REDEFMACRO
             -Wno-CASEINCOMPLETE -Wno-UNUSEDSIGNAL -Wno-CASEOVERLAP
             -Wno-CMPCONST -Wno-UNOPTFLAT -Wno-UNSIGNED
             --x-initial unique --x-assign unique)

set(LIBRARIES spdlog::spdlog
              cxxopts::cxxopts)

if (USE_TRACE)
   add_definitions(-DVCD_OUTPUT)
   set(VL_FLAGS ${VL_FLAGS} --trace --trace-structs -DVCD_OUTPUT)
elseif (USE_TRACE_FST)
   add_definitions(-DFST_OUTPUT)
   set(VL_FLAGS ${VL_FLAGS} --trace-fst --trace-threads 2 -DFST_OUTPUT)
endif()

if (USE_ITRACE)
   add_definitions(-DITRACE)
endif()

if (USE_DROMAJO)
   add_definitions(-DDROMAJO)
   add_subdirectory(dromajo)
   target_compile_definitions(dromajo_cosim PRIVATE VLEN=0 ELEN=0)

   include_directories(dromajo/include)

   set(LIBRARIES ${LIBRARIES} dromajo_cosim)
endif()

if (USE_RAMULATOR)
   add_definitions(-DRAMULATOR)
   set(LIBRARIES ${LIBRARIES} ramulator)
endif()

add_executable(rtlsim ${SOURCE_FILES})

verilate(rtlsim SOURCES /tmp/soc_wrapper.v TOP_MODULE ${TOP_MODULE}
                INCLUDE_DIRS ${RTL_INCLUDE} THREADS ${N_THREADS}
                VERILATOR_ARGS ${VL_FLAGS})

target_link_libraries(rtlsim PUBLIC ${LIBRARIES})
