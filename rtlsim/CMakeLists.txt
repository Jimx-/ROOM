include(ProcessorCount)
ProcessorCount(N_THREADS)

option(USE_TRACE "Enable VCD tracing" OFF)
option(USE_TRACE_FST "Enable FST tracing" OFF)

find_package(verilator HINTS $ENV{VERILATOR_ROOT} ${VERILATOR_ROOT})
if (NOT verilator_FOUND)
  message(FATAL_ERROR "Verilator was not found. Either install it, or set the VERILATOR_ROOT environment variable")
endif()

set(SOURCE_FILES main.cpp
                 soc.cpp
                 uart.cpp
                 sdcard.cpp)

set(TOP_MODULE soc_wrapper)

set(RTL_INCLUDE /tmp)

set(VL_FLAGS -O2 --language 1800-2009 --assert -Wall -Wpedantic
             -Wno-WIDTH -Wno-DECLFILENAME -Wno-REDEFMACRO
             -Wno-CASEINCOMPLETE -Wno-UNUSEDSIGNAL -Wno-CASEOVERLAP
             -Wno-CMPCONST -Wno-UNOPTFLAT -Wno-UNSIGNED
             --x-initial unique --x-assign unique)

if (USE_TRACE)
   add_definitions(-DVCD_OUTPUT)
   set(VL_FLAGS ${VL_FLAGS} --trace --trace-structs -DVCD_OUTPUT)
elseif (USE_TRACE_FST)
   add_definitions(-DFST_OUTPUT)
   set(VL_FLAGS ${VL_FLAGS} --trace-fst --trace-threads 2 -DFST_OUTPUT)
endif()

set(LIBRARIES CONAN_PKG::spdlog
              CONAN_PKG::cxxopts)

add_executable(rtlsim ${SOURCE_FILES})

verilate(rtlsim SOURCES /tmp/soc_wrapper.v TOP_MODULE ${TOP_MODULE}
                INCLUDE_DIRS ${RTL_INCLUDE} THREADS ${N_THREADS}
                VERILATOR_ARGS ${VL_FLAGS})

target_link_libraries(rtlsim PUBLIC ${LIBRARIES})
